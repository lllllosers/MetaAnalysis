# =========================
# scripts/R/03_impute_variance.R
# 功能：
#   1) 基于 02 输出的 LongData_clean.csv，针对 cfg$indicator（如 SOC）补齐 SD_E/SD_C
#   2) 补齐规则（逐条，E/C 各自独立）：
#      (1) 若原始 SD 已有 -> 原样使用
#      (2) 否则若 SE 有且 n 有 -> SD = SE * sqrt(n)
#      (3) 否则用“平均变异系数” mr：mr = mean(SD_r / Mean_r) over reported（剔除无效/0/NA）
#          再 SD_i = mr * Mean_i
#      (4) 若仍缺失 -> SD = default_cv * Mean_i（default_cv=0.1，可在 YAML 覆盖）
#   3) 输出：
#      - data/interim/<IND>_imputed.csv                 # 含补齐后的 SD 与标记位
#      - results/logs/03_impute_variance/impute_<IND>_cv_notes.csv   # mr、nr 等汇总
#      - results/logs/03_impute_variance/impute_<IND>_summary.csv    # 补齐来源统计
#   4) （可选）做 lnRR 方差的行内快速核对列（v_lnRR_check），便于下一步 04 使用前自查
# 依赖：
#   - 00_setup.R（提供 cfg、ppath、%||%、write_safe_csv、write_log_csv、STAGE_03）
#   - 02_clean_standardize.R 已产出 data/interim/LongData_clean.csv
# 备注：
#   - 本脚本不做 metagear 自动估算（按你要求：先用人工规则）
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)

# ---- 0.1) 读取列名映射（兼容 *_std 优先）----
col_map       <- cfg$columns
col_indicator <- col_map$indicator
col_mean_e    <- "Mean_E_std"   # 统一使用 *_std（来自 02）
col_sd_e      <- "SD_E_std"
col_n_e       <- col_map$n_e
col_mean_c    <- "Mean_C_std"
col_sd_c      <- "SD_C_std"
col_n_c       <- col_map$n_c
col_se_e      <- col_map$se_e %||% NA_character_
col_se_c      <- col_map$se_c %||% NA_character_

target_indicator <- cfg$indicator
default_cv       <- (cfg$imputation$default_cv %||% 0.1) %>% as.numeric()

# ---- 1) 读取 02 的清洗数据 ----
clean_path <- file.path(ppath("interim_dir"), "LongData_clean.csv")
stopifnot(file.exists(clean_path))
dat <- readr::read_csv(clean_path, show_col_types = FALSE)
message("[03_impute] Loaded clean: ", clean_path, " | rows = ", nrow(dat), ", cols = ", ncol(dat))

# 仅保留目标指标
dat_ind <- dat %>% dplyr::filter(.data[[col_indicator]] == target_indicator)
k <- nrow(dat_ind)
if (k == 0L) stop("[03_impute] 无该指标记录：", target_indicator)

# ---- 2) 计算 mr（平均变异系数）：基于“已报告 SD 的记录”----
# E 与 C 各自计算，再取 pooled（all）一并给出，便于记录/透明化
safe_ratio <- function(sd, mu) {
  sd <- suppressWarnings(as.numeric(sd))
  mu <- suppressWarnings(as.numeric(mu))
  out <- sd / mu
  out[!is.finite(out) | mu <= 0] <- NA_real_
  out
}

cv_e_vec <- safe_ratio(dat_ind[[col_sd_e]], dat_ind[[col_mean_e]])
cv_c_vec <- safe_ratio(dat_ind[[col_sd_c]], dat_ind[[col_mean_c]])

mr_e   <- mean(cv_e_vec, na.rm = TRUE)
nr_e   <- sum(!is.na(cv_e_vec))
mr_c   <- mean(cv_c_vec, na.rm = TRUE)
nr_c   <- sum(!is.na(cv_c_vec))
mr_all <- mean(c(cv_e_vec, cv_c_vec), na.rm = TRUE)
nr_all <- sum(!is.na(c(cv_e_vec, cv_c_vec)))

# 若 mr_all 不可得，则回退到 default_cv（仍会记录）
use_cv <- ifelse(is.finite(mr_all) && !is.na(mr_all), mr_all, default_cv)

# ---- 3) 按规则补齐 SD（向量化实现；E/C 各自一套）----
to_num <- function(x) suppressWarnings(as.numeric(x))

SE_E <- if (!is.na(col_se_e) && col_se_e %in% names(dat_ind)) to_num(dat_ind[[col_se_e]]) else rep(NA_real_, k)
SE_C <- if (!is.na(col_se_c) && col_se_c %in% names(dat_ind)) to_num(dat_ind[[col_se_c]]) else rep(NA_real_, k)

ME_E <- to_num(dat_ind[[col_mean_e]])
ME_C <- to_num(dat_ind[[col_mean_c]])
SD_E <- to_num(dat_ind[[col_sd_e]])
SD_C <- to_num(dat_ind[[col_sd_c]])
NE   <- to_num(dat_ind[[col_n_e]])
NC   <- to_num(dat_ind[[col_n_c]])

# 辅助：判定可用性
has_sd_e <- !is.na(SD_E) & is.finite(SD_E)
has_sd_c <- !is.na(SD_C) & is.finite(SD_C)
has_se_e <- !is.na(SE_E) & is.finite(SE_E) & !is.na(NE) & is.finite(NE) & NE > 0
has_se_c <- !is.na(SE_C) & is.finite(SE_C) & !is.na(NC) & is.finite(NC) & NC > 0
has_me_e <- !is.na(ME_E) & is.finite(ME_E) & ME_E > 0
has_me_c <- !is.na(ME_C) & is.finite(ME_C) & ME_C > 0

# 结果容器 & 来源标记
SD_E_imp   <- SD_E
SD_C_imp   <- SD_C
src_E      <- rep(NA_character_, k)
src_C      <- rep(NA_character_, k)

# (1) 原始 SD 直接用
src_E[has_sd_e] <- "as_is"
src_C[has_sd_c] <- "as_is"

# (2) 用 SE 补：SD = SE * sqrt(n)
need_e_se <- !has_sd_e & has_se_e
need_c_se <- !has_sd_c & has_se_c
SD_E_imp[need_e_se] <- SE_E[need_e_se] * sqrt(NE[need_e_se])
SD_C_imp[need_c_se] <- SE_C[need_c_se] * sqrt(NC[need_c_se])
src_E[need_e_se] <- "from_se"
src_C[need_c_se] <- "from_se"

# (3) 用 mr 补：SD = mr * mean（优先 mr_all；也记录 mr_e/mr_c 备查）
need_e_cv <- !has_sd_e & !need_e_se & has_me_e
need_c_cv <- !has_sd_c & !need_c_se & has_me_c
SD_E_imp[need_e_cv] <- use_cv * ME_E[need_e_cv]
SD_C_imp[need_c_cv] <- use_cv * ME_C[need_c_cv]
src_E[need_e_cv] <- ifelse(use_cv == mr_all, "from_cv_avg_all", "from_cv_def")
src_C[need_c_cv] <- ifelse(use_cv == mr_all, "from_cv_avg_all", "from_cv_def")

# (4) 兜底 default_cv：仅当 mean 可用但前面都未触发（理论同上分支其实已覆盖 from_cv_def）
need_e_def <- !has_sd_e & !need_e_se & !need_e_cv & has_me_e
need_c_def <- !has_sd_c & !need_c_se & !need_c_cv & has_me_c
SD_E_imp[need_e_def] <- default_cv * ME_E[need_e_def]
SD_C_imp[need_c_def] <- default_cv * ME_C[need_c_def]
src_E[need_e_def] <- "from_cv_def"
src_C[need_c_def] <- "from_cv_def"

# 若仍有 NA（如 mean 不可用、n=0 之类），保持 NA 并在 summary 里体现
any_E_missing <- sum(is.na(SD_E_imp))
any_C_missing <- sum(is.na(SD_C_imp))

# ---- 4) 写回数据框，输出产物 ----
dat_ind$SD_E_imp  <- SD_E_imp
dat_ind$SD_C_imp  <- SD_C_imp
dat_ind$SD_E_src  <- src_E
dat_ind$SD_C_src  <- src_C

# 便于 04 直接使用（同时保留原 *_std）
out_imp_path <- file.path(ppath("interim_dir"), paste0(target_indicator, "_imputed.csv"))
readr::write_excel_csv(dat_ind, out_imp_path)
message("[03_impute] Imputed data -> ", out_imp_path)

# ---- 5) 日志：mr / summary ----
cv_notes <- tibble::tibble(
  mr_e   = mr_e,   nr_e   = nr_e,
  mr_c   = mr_c,   nr_c   = nr_c,
  mr_all = mr_all, nr_all = nr_all,
  default_cv = default_cv,
  cv_method  = ifelse(is.finite(mr_all) && !is.na(mr_all), "average", "default"),
  use_from_se = any(need_e_se | need_c_se)
)
write_log_csv(cv_notes, STAGE_03, paste0("impute_", target_indicator, "_cv_notes.csv"))

imp_summary <- tibble::tibble(
  rows = k,
  SD_E_as_is        = sum(src_E == "as_is", na.rm = TRUE),
  SD_E_from_se      = sum(src_E == "from_se", na.rm = TRUE),
  SD_E_from_cv_all  = sum(src_E == "from_cv_avg_all", na.rm = TRUE),
  SD_E_from_cv_def  = sum(src_E == "from_cv_def", na.rm = TRUE),
  SD_C_as_is        = sum(src_C == "as_is", na.rm = TRUE),
  SD_C_from_se      = sum(src_C == "from_se", na.rm = TRUE),
  SD_C_from_cv_all  = sum(src_C == "from_cv_avg_all", na.rm = TRUE),
  SD_C_from_cv_def  = sum(src_C == "from_cv_def", na.rm = TRUE),
  any_SD_E_missing  = any_E_missing,
  any_SD_C_missing  = any_C_missing
)
write_log_csv(imp_summary, STAGE_03, paste0("impute_", target_indicator, "_summary.csv"))

# ---- 6) （可选）行内快速核对：v_lnRR（仅检查是否全为有限值；04 会正式计算）----
# v = SD_E^2/(n_E * Mean_E^2) + SD_C^2/(n_C * Mean_C^2)
v_chk <- (SD_E_imp^2)/(NE * ME_E^2) + (SD_C_imp^2)/(NC * ME_C^2)
dat_ind$v_lnRR_check <- v_chk

out_chk_path <- file.path(ppath("interim_dir"), paste0(target_indicator, "_for_effectsize.csv"))
readr::write_excel_csv(dat_ind, out_chk_path)
message("[03_impute] Quick-check data (with v_lnRR_check) -> ", out_chk_path)

# 质控提示
finite_rate <- mean(is.finite(v_chk), na.rm = TRUE)
message(sprintf("[03_impute] v_lnRR_check finite ratio: %.1f%%", 100 * finite_rate))
message("[03_impute] 完成。下一步可运行 04_effect_size.R。")
