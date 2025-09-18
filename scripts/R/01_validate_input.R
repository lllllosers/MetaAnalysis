# =========================
# scripts/R/01_validate.R
# 功能：
#   1) 读取数据（优先使用 02 产物 LongData_clean.csv，否则回退到原始Excel）
#   2) 对全表做概览（指标分布、单位分布）
#   3) 针对“当前指标”（cfg$indicator）做合规性检查：
#      - 必需列是否存在
#      - 均值/SD/n 的缺失与非正值统计
#   4) 输出检查日志到 results/logs/
# 依赖：
#   - 已运行 00_setup.R（加载 cfg、ppath、write_safe_csv 等）
#   - 可选：已运行 02_clean_standardize.R（则会优先读取 *_std 列）
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)  # 来自 00_setup
ind  <- cfg$indicator

# ---- 1) 读取数据 ----
clean_csv <- file.path(ppath("interim_dir"), "LongData_clean.csv")
if (file.exists(clean_csv)) {
  dat <- readr::read_csv(clean_csv, show_col_types = FALSE)
  message("[01_validate] Loaded cleaned data: ", clean_csv)
} else {
  raw_xlsx <- ppath("raw_xlsx")
  stopifnot(file.exists(raw_xlsx))
  stopifnot(cfg$sheets$longdata %in% readxl::excel_sheets(raw_xlsx))
  dat <- readxl::read_excel(raw_xlsx, sheet = cfg$sheets$longdata)
  message("[01_validate] Loaded raw Excel (no cleaned file found): ", raw_xlsx)
}
message("[01_validate] Rows = ", nrow(dat), ", Cols = ", ncol(dat))

# ---- 2) 必需列存在性检查（按 YAML 列名映射）----
req_keys <- c("indicator","unit","mean_e","sd_e","n_e","mean_c","sd_c","n_c")
req_cols <- unlist(cfg$columns[req_keys], use.names = FALSE)
missing_cols <- setdiff(req_cols, names(dat))
if (length(missing_cols) > 0) {
  stop(sprintf("[01_validate] 缺少必需列：%s", paste(missing_cols, collapse = ", ")))
}
message("[01_validate] 必需列检查通过。")

# ---- 3) 全表概览：指标分布 & 单位分布 ----
# 指标分布
ind_overview <- dat %>%
  dplyr::count(.data[[cfg$columns$indicator]], name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::rename(Indicator = !!cfg$columns$indicator)

write_log_csv(ind_overview, STAGE_01, "indicators_overview.csv")
message("[01_validate] 指标列表与计数写入：results/logs/indicators_overview.csv")
message(utils::capture.output(print(utils::head(ind_overview, 10))), sep = "\n")

# 单位分布（全表）
units_overview <- dat %>%
  dplyr::count(.data[[cfg$columns$unit]], name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::rename(Unit = !!cfg$columns$unit)

write_log_csv(units_overview, STAGE_01, "units_overview.csv")
message("[01_validate] 单位分布写入：results/logs/units_overview.csv")

# ---- 4) 针对当前指标（cfg$indicator）做“定向检查” ----
d1 <- dat %>% dplyr::filter(.data[[cfg$columns$indicator]] == ind)
k  <- nrow(d1)
message(sprintf("[01_validate] 当前指标：%s 记录数：%d", ind, k))

# 选择用于检查的列：若存在 *_std 列则优先（由 02 生成）
prefer_std <- isTRUE(cfg$imputation$prefer_std_cols)
col_mean_e <- if (prefer_std && "Mean_E_std" %in% names(d1)) "Mean_E_std" else cfg$columns$mean_e
col_mean_c <- if (prefer_std && "Mean_C_std" %in% names(d1)) "Mean_C_std" else cfg$columns$mean_c
col_sd_e   <- if (prefer_std && "SD_E_std"   %in% names(d1)) "SD_E_std"   else cfg$columns$sd_e
col_sd_c   <- if (prefer_std && "SD_C_std"   %in% names(d1)) "SD_C_std"   else cfg$columns$sd_c
col_n_e    <- cfg$columns$n_e
col_n_c    <- cfg$columns$n_c

# 可选列（SE）
col_se_e <- cfg$columns$se_e %||% NA_character_
col_se_c <- cfg$columns$se_c %||% NA_character_

# 小工具：安全取列
getcol <- function(df, col) if (!is.na(col) && col %in% names(df)) df[[col]] else rep(NA, nrow(df))

# ---- 4.1 缺失统计 ----
miss_tbl <- tibble::tibble(
  Mean_E_NA = sum(is.na(getcol(d1, col_mean_e))),
  SD_E_NA   = sum(is.na(getcol(d1, col_sd_e))),
  n_E_NA    = sum(is.na(getcol(d1, col_n_e))),
  Mean_C_NA = sum(is.na(getcol(d1, col_mean_c))),
  SD_C_NA   = sum(is.na(getcol(d1, col_sd_c))),
  n_C_NA    = sum(is.na(getcol(d1, col_n_c))),
  SE_E_NA   = sum(is.na(getcol(d1, col_se_e))),
  SE_C_NA   = sum(is.na(getcol(d1, col_se_c)))
)
out_missing_path <- file.path(ppath("logs_dir"), sprintf("validate_%s_missing.csv", ind))
write_log_csv(miss_tbl, STAGE_01, sprintf("validate_%s_missing.csv", ind))
message("[01_validate] ", ind, " 缺失统计写入：", out_missing_path)
message(utils::capture.output(print(miss_tbl)), sep = "\n")

# ---- 4.2 合法性初检：非正均值/负SD/非正样本量 ----
# 强制转数值后检查（防止字符型）
to_num <- function(x) suppressWarnings(as.numeric(x))
mE <- to_num(getcol(d1, col_mean_e)); mC <- to_num(getcol(d1, col_mean_c))
sE <- to_num(getcol(d1, col_sd_e));   sC <- to_num(getcol(d1, col_sd_c))
nE <- to_num(getcol(d1, col_n_e));    nC <- to_num(getcol(d1, col_n_c))

idx_mean_nonpos <- which(!(mE > 0 & mC > 0))
idx_sd_neg      <- which((!is.na(sE) & sE < 0) | (!is.na(sC) & sC < 0))
idx_n_nonpos    <- which(!(nE > 0 & nC > 0))

basic_checks <- tibble::tibble(
  rows_total   = k,
  mean_nonpos  = length(idx_mean_nonpos),
  sd_negative  = length(idx_sd_neg),
  n_nonpos     = length(idx_n_nonpos)
)

out_basic_path <- file.path(ppath("logs_dir"), sprintf("validate_%s_basic_checks.csv", ind))
write_log_csv(basic_checks, STAGE_01, sprintf("validate_%s_basic_checks.csv", ind))
message("[01_validate] ", ind, " 合法性初检写入：", out_basic_path)
message(utils::capture.output(print(basic_checks)), sep = "\n")

# ---- 4.3 标记可能问题行（便于人工核查）----
flag_tbl <- d1 %>%
  dplyr::mutate(
    .row = dplyr::row_number(),
    Mean_nonpos = !(to_num(.data[[col_mean_e]]) > 0 & to_num(.data[[col_mean_c]]) > 0),
    SD_neg      = (!is.na(to_num(.data[[col_sd_e]])) & to_num(.data[[col_sd_e]]) < 0) |
      (!is.na(to_num(.data[[col_sd_c]])) & to_num(.data[[col_sd_c]]) < 0),
    n_nonpos    = !(to_num(.data[[col_n_e]]) > 0 & to_num(.data[[col_n_c]]) > 0)
  ) %>%
  dplyr::filter(Mean_nonpos | SD_neg | n_nonpos) %>%
  dplyr::select(.row,
                Indicator = !!cfg$columns$indicator,
                Unit      = !!cfg$columns$unit,
                dplyr::all_of(c(col_mean_e, col_sd_e, col_n_e,
                                col_mean_c, col_sd_c, col_n_c)),
                Mean_nonpos, SD_neg, n_nonpos)

out_flag_path <- file.path(ppath("logs_dir"), sprintf("validate_%s_flags.csv", ind))
write_log_csv(flag_tbl, STAGE_01, sprintf("validate_%s_flags.csv", ind))
if (nrow(flag_tbl) == 0) {
  message("[01_validate] 未发现需要特别标记的 ", ind, " 记录。")
} else {
  message("[01_validate] 标记问题行写入：", out_flag_path,
          "  （rows = ", nrow(flag_tbl), "）")
}

message("[01_validate] 完成。下一步请运行 02_clean_standardize.R（如需标准化列/单位），然后再运行 03_impute_variance.R。")

