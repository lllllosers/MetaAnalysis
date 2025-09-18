# =========================
# scripts/R/03b_impute_variance_metagear.R
# 流程：
#   原始SD → (可选)SE*sqrt(n) → metagear::impute_SD(单列、分两次) → default CV兜底
# 产出：
#   data/interim/soc_imputed_metagear.csv
#   results/logs/impute_soc_summary_metagear.csv
#   results/logs/impute_soc_metagear_params.csv
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")
root <- here::here()
cfg  <- read_yaml_safe(file.path(root, "config", "meta_config.yaml"))

if (!"metagear" %in% rownames(installed.packages())) {
  install.packages("metagear", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(library(metagear))

sheet_long    <- cfg$sheets$longdata
col_indicator <- cfg$columns$indicator
col_unit      <- cfg$columns$unit
col_mean_e    <- cfg$columns$mean_e
col_sd_e      <- cfg$columns$sd_e
col_n_e       <- cfg$columns$n_e
col_mean_c    <- cfg$columns$mean_c
col_sd_c      <- cfg$columns$sd_c
col_n_c       <- cfg$columns$n_c

# 可选 SE 列
se_e_col <- if (!is.null(cfg$columns$se_e)) cfg$columns$se_e else "SE_E"
se_c_col <- if (!is.null(cfg$columns$se_c)) cfg$columns$se_c else "SE_C"

use_from_se <- isTRUE(cfg$imputation$from_se)
default_cv  <- if (!is.null(cfg$imputation$default_cv)) cfg$imputation$default_cv else 0.10

raw_xlsx <- file.path(root, cfg$paths$raw_xlsx)
stopifnot(file.exists(raw_xlsx))

# ---- 1) 读取目标指标 ----
dat <- readxl::read_excel(raw_xlsx, sheet = sheet_long)
target_indicator <- cfg$indicator
soc <- dplyr::filter(dat, .data[[col_indicator]] == target_indicator)
if (nrow(soc) == 0) stop(sprintf("[03b_metagear] 未找到 indicator == '%s' 的记录。", target_indicator))
message("[03b_metagear] ", target_indicator, " rows: ", nrow(soc))

# 强制相关列为 numeric，避免字符类型导致 impute 失败
soc <- soc %>%
  dplyr::mutate(across(
    dplyr::any_of(c(col_mean_e, col_sd_e, col_n_e, col_mean_c, col_sd_c, col_n_c, se_e_col, se_c_col)),
    ~ suppressWarnings(as.numeric(.x))
  ))

# ---- 2) 初始来源标签 ----
soc <- soc %>%
  dplyr::mutate(
    SD_E_src = dplyr::if_else(!is.na(.data[[col_sd_e]]), "as_is", NA_character_),
    SD_C_src = dplyr::if_else(!is.na(.data[[col_sd_c]]), "as_is", NA_character_)
  )

# ---- 3) 可选：SE→SD ----
has_se_e <- se_e_col %in% names(soc)
has_se_c <- se_c_col %in% names(soc)

if (use_from_se) {
  if (has_se_e) {
    idx <- is.na(soc[[col_sd_e]]) & !is.na(soc[[se_e_col]]) & !is.na(soc[[col_n_e]])
    soc[[col_sd_e]][idx] <- soc[[se_e_col]][idx] * sqrt(soc[[col_n_e]][idx])
    soc$SD_E_src[idx] <- dplyr::coalesce(soc$SD_E_src[idx], "from_se")
  }
  if (has_se_c) {
    idx <- is.na(soc[[col_sd_c]]) & !is.na(soc[[se_c_col]]) & !is.na(soc[[col_n_c]])
    soc[[col_sd_c]][idx] <- soc[[se_c_col]][idx] * sqrt(soc[[col_n_c]][idx])
    soc$SD_C_src[idx] <- dplyr::coalesce(soc$SD_C_src[idx], "from_se")
  }
}

# ---- 4) 用 metagear::impute_SD（分两次：E 列与 C 列分别处理）----
# 说明：一次只传入一个 SD 列和其对应的 Mean 列，避免内部 missingness 变成矩阵
# 4.1 处理 E 列
try({
  idx_e <- which(is.na(soc[[col_sd_e]]) & !is.na(soc[[col_mean_e]]) & (soc[[col_mean_e]] > 0))
  if (length(idx_e) > 0) {
    tmp_e <- metagear::impute_SD(
      aDataFrame    = soc,
      columnSDnames = col_sd_e,
      columnXnames  = col_mean_e,
      method        = "Bracken1992"
    )
    filled_e <- which(is.na(soc[[col_sd_e]]) & !is.na(tmp_e[[col_sd_e]]))
    if (length(filled_e) > 0) {
      soc[[col_sd_e]][filled_e] <- tmp_e[[col_sd_e]][filled_e]
      soc$SD_E_src[filled_e] <- dplyr::coalesce(soc$SD_E_src[filled_e], "metagear_bracken")
    }
  }
}, silent = TRUE)

# 4.2 处理 C 列
try({
  idx_c <- which(is.na(soc[[col_sd_c]]) & !is.na(soc[[col_mean_c]]) & (soc[[col_mean_c]] > 0))
  if (length(idx_c) > 0) {
    tmp_c <- metagear::impute_SD(
      aDataFrame    = soc,
      columnSDnames = col_sd_c,
      columnXnames  = col_mean_c,
      method        = "Bracken1992"
    )
    filled_c <- which(is.na(soc[[col_sd_c]]) & !is.na(tmp_c[[col_sd_c]]))
    if (length(filled_c) > 0) {
      soc[[col_sd_c]][filled_c] <- tmp_c[[col_sd_c]][filled_c]
      soc$SD_C_src[filled_c] <- dplyr::coalesce(soc$SD_C_src[filled_c], "metagear_bracken")
    }
  }
}, silent = TRUE)

# ---- 5) 仍缺失 → default CV 兜底 ----
idx_e_cv <- is.na(soc[[col_sd_e]]) & !is.na(soc[[col_mean_e]]) & (soc[[col_mean_e]] > 0)
soc[[col_sd_e]][idx_e_cv] <- soc[[col_mean_e]][idx_e_cv] * default_cv
soc$SD_E_src[idx_e_cv] <- dplyr::coalesce(soc$SD_E_src[idx_e_cv], "from_cv_default")

idx_c_cv <- is.na(soc[[col_sd_c]]) & !is.na(soc[[col_mean_c]]) & (soc[[col_mean_c]] > 0)
soc[[col_sd_c]][idx_c_cv] <- soc[[col_mean_c]][idx_c_cv] * default_cv
soc$SD_C_src[idx_c_cv] <- dplyr::coalesce(soc$SD_C_src[idx_c_cv], "from_cv_default")

# ---- 6) 导出与汇总 ----
soc <- soc %>% dplyr::mutate(SD_E_use = .data[[col_sd_e]], SD_C_use = .data[[col_sd_c]])

dir.create(file.path(root, cfg$paths$interim_dir), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(root, cfg$paths$logs_dir),    showWarnings = FALSE, recursive = TRUE)
readr::write_excel_csv(soc, file.path(root, cfg$paths$interim_dir, "soc_imputed_metagear.csv"))

summary_src <- soc %>%
  dplyr::summarise(
    rows = dplyr::n(),
    SD_E_as_is        = sum(SD_E_src == "as_is", na.rm = TRUE),
    SD_E_from_se      = sum(SD_E_src == "from_se", na.rm = TRUE),
    SD_E_metagear     = sum(SD_E_src == "metagear_bracken", na.rm = TRUE),
    SD_E_from_cv_def  = sum(SD_E_src == "from_cv_default", na.rm = TRUE),
    SD_C_as_is        = sum(SD_C_src == "as_is", na.rm = TRUE),
    SD_C_from_se      = sum(SD_C_src == "from_se", na.rm = TRUE),
    SD_C_metagear     = sum(SD_C_src == "metagear_bracken", na.rm = TRUE),
    SD_C_from_cv_def  = sum(SD_C_src == "from_cv_default", na.rm = TRUE),
    any_SD_E_missing  = sum(is.na(SD_E_use), na.rm = TRUE),
    any_SD_C_missing  = sum(is.na(SD_C_use), na.rm = TRUE)
  )

params <- tibble::tibble(
  metagear_method = "Bracken1992",
  default_cv = default_cv,
  used_from_se = use_from_se
)

readr::write_excel_csv(summary_src, file.path(root, cfg$paths$logs_dir, "impute_soc_summary_metagear.csv"))
readr::write_excel_csv(params,     file.path(root, cfg$paths$logs_dir, "impute_soc_metagear_params.csv"))

message("[03b_metagear] Write -> data/interim/soc_imputed_metagear.csv")
message("[03b_metagear] Summary -> results/logs/impute_soc_summary_metagear.csv")
message("[03b_metagear] Params  -> results/logs/impute_soc_metagear_params.csv")
print(summary_src); print(params)
message("[03b_metagear] 完成。")
