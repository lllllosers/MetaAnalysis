# =========================
# scripts/R/04_effect_size.R
# 功能：
#   1) 读取 03 的补齐结果（data/interim/<IND>_imputed.csv）
#   2) 计算效应量 lnRR 及其方差（Delta 方法）：
#        yi = lnRR = log(Mean_E_std / Mean_C_std)
#        vi = SD_E_imp^2 / (n_E * Mean_E_std^2) + SD_C_imp^2 / (n_C * Mean_C_std^2)
#      并派生 sei = sqrt(vi)、wi = 1/vi（供 metafor::rma 用）
#   3) 做可用性筛查：均值>0、n>0、方差>0、SD 不缺失等；保留/剔除分别导出
#   4) 写主数据到 data/interim/<IND>_effects.csv；日志到 results/logs/04_effect_size/
# 依赖：
#   - 00_setup.R（提供 cfg、ppath、%||%、write_safe_csv、write_log_csv、STAGE_04）
#   - 03_impute_variance.R 已产出 data/interim/<IND>_imputed.csv
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)

# ---- 0.1) 列名映射（结合 *_std 与 03 的 *_imp）----
col_map       <- cfg$columns
col_indicator <- col_map$indicator
col_mean_e    <- "Mean_E_std"   # 统一用 02 的标准化列
col_mean_c    <- "Mean_C_std"
col_n_e       <- col_map$n_e
col_n_c       <- col_map$n_c
# 03 生成的列：
col_sd_e_imp  <- "SD_E_imp"
col_sd_c_imp  <- "SD_C_imp"

target_indicator <- cfg$indicator

# ---- 1) 读取 03 的补齐结果 ----
imp_path <- file.path(ppath("interim_dir"), paste0(target_indicator, "_imputed.csv"))
stopifnot(file.exists(imp_path))
dat_imp <- readr::read_csv(imp_path, show_col_types = FALSE)
message("[04_effect] Loaded imputed: ", imp_path, " | rows = ", nrow(dat_imp), ", cols = ", ncol(dat_imp))

# 只保留目标指标（容错：如果 03 已经筛选过，这里仍确保）
dat_ind <- dat_imp %>% dplyr::filter(.data[[col_indicator]] == target_indicator)
k <- nrow(dat_ind)
if (k == 0L) stop("[04_effect] 无该指标记录：", target_indicator)

# ---- 2) 计算 lnRR 及方差（Delta 方法）----
to_num <- function(x) suppressWarnings(as.numeric(x))

ME_E <- to_num(dat_ind[[col_mean_e]])
ME_C <- to_num(dat_ind[[col_mean_c]])
NE   <- to_num(dat_ind[[col_n_e]])
NC   <- to_num(dat_ind[[col_n_c]])
SD_E <- to_num(dat_ind[[col_sd_e_imp]])
SD_C <- to_num(dat_ind[[col_sd_c_imp]])

# 基本检查（向量逻辑）
ok_mean_pos <- (ME_E > 0) & (ME_C > 0)
ok_n_pos    <- (NE > 0) & (NC > 0)
ok_sd_pos   <- is.finite(SD_E) & is.finite(SD_C) & (SD_E >= 0) & (SD_C >= 0)

# lnRR 与方差
yi <- log(ME_E / ME_C)
vi <- (SD_E^2) / (NE * ME_E^2) + (SD_C^2) / (NC * ME_C^2)

# 计算后的有效性（方差必须有限且>0；yi 必须有限）
ok_yi <- is.finite(yi)
ok_vi <- is.finite(vi) & (vi > 0)

# 组合总可用标记
keep <- ok_mean_pos & ok_n_pos & ok_sd_pos & ok_yi & ok_vi

# ---- 3) 组织输出数据框 ----
out_all <- dat_ind %>%
  dplyr::mutate(
    yi  = yi,
    vi  = vi,
    sei = sqrt(vi),
    wi  = 1/vi,
    # 质量标记列（便于排查）
    chk_mean_pos = ok_mean_pos,
    chk_n_pos    = ok_n_pos,
    chk_sd_pos   = ok_sd_pos,
    chk_yi_fin   = ok_yi,
    chk_vi_pos   = ok_vi,
    keep_effect  = keep
  )

# 拆分：保留/剔除
out_keep <- out_all %>% dplyr::filter(keep_effect)
out_drop <- out_all %>% dplyr::filter(!keep_effect)

# ---- 4) 导出主数据与日志 ----
# 4.1 主体数据（供 05/06 直接使用）：仅保留分析必需列 + 你关心的元信息列
# 你可按需在此补充 Author_Year、StudyID、Soil_layer 等列名（只要存在于 dat_ind）
keep_cols <- c(
  # 元信息（若存在就保留）
  col_map$study_id %||% "StudyID",
  col_map$author_year %||% "Author_Year",
  col_indicator,
  "Soil_layer", "Soil_layer_export",
  # 参与计算的列
  col_mean_e, col_mean_c, col_n_e, col_n_c, col_sd_e_imp, col_sd_c_imp,
  # 效应量列
  "yi", "vi", "sei", "wi"
)
keep_cols <- intersect(keep_cols, names(out_keep))

effects_path <- file.path(ppath("processed_dir"), paste0(target_indicator, "_effects.csv"))
readr::write_excel_csv(out_keep[, keep_cols, drop = FALSE], effects_path)
message("[04_effect] Effects data -> ", effects_path)

# 4.2 日志目录
STAGE_04 <- "04_effect_size"   # 若你已在 utils 定义常量，可替换为该常量
log_dir <- file.path("results", "logs", STAGE_04)
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# 4.3 基本汇总日志
basic_summary <- tibble::tibble(
  indicator     = target_indicator,
  rows_in       = nrow(dat_ind),
  rows_keep     = nrow(out_keep),
  rows_drop     = nrow(out_drop),
  yi_min        = suppressWarnings(min(out_keep$yi, na.rm = TRUE)),
  yi_median     = suppressWarnings(stats::median(out_keep$yi, na.rm = TRUE)),
  yi_mean       = suppressWarnings(mean(out_keep$yi, na.rm = TRUE)),
  yi_max        = suppressWarnings(max(out_keep$yi, na.rm = TRUE)),
  vi_min        = suppressWarnings(min(out_keep$vi, na.rm = TRUE)),
  vi_median     = suppressWarnings(stats::median(out_keep$vi, na.rm = TRUE)),
  vi_mean       = suppressWarnings(mean(out_keep$vi, na.rm = TRUE)),
  vi_max        = suppressWarnings(max(out_keep$vi, na.rm = TRUE))
)
readr::write_excel_csv(basic_summary, file.path(log_dir, paste0("basic_summary_", target_indicator, ".csv")))

# 4.4 剔除明细（若有）
if (nrow(out_drop) > 0) {
  # 只导出关键诊断列，便于快速定位问题
  drop_cols <- c(
    col_map$study_id %||% "StudyID",
    col_map$author_year %||% "Author_Year",
    col_indicator, col_mean_e, col_mean_c, col_n_e, col_n_c, col_sd_e_imp, col_sd_c_imp,
    "yi", "vi", "chk_mean_pos", "chk_n_pos", "chk_sd_pos", "chk_yi_fin", "chk_vi_pos"
  )
  drop_cols <- intersect(drop_cols, names(out_drop))
  readr::write_excel_csv(out_drop[, drop_cols, drop = FALSE],
                         file.path(log_dir, paste0("dropped_rows_", target_indicator, ".csv")))
  message("[04_effect] Dropped rows -> results/logs/04_effect_size/")
} else {
  message("[04_effect] No dropped rows.")
}

message("[04_effect] 完成。下一步：05_meta_aggregate.R（合并效应/森林图/异质性）。")
