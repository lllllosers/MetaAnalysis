# =========================
# utils/functions_export.R
# 统一的 CSV 导出（带 BOM，Excel 不乱码/不串列）
# =========================
write_safe_csv <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_excel_csv(x, path)
  message("[export] -> ", path)
}

# 固定的 stage 常量，避免拼写错误
STAGE_00 <- "00_setup"
STAGE_01 <- "01_validate_input"
STAGE_02 <- "02_clean_standardize"
STAGE_03 <- "03_impute_variance"
STAGE_04 <- "04_effect_size"
STAGE_05 <- "05_meta_models"

# 根据 stage 返回并创建日志目录
get_log_dir <- function(stage) {
  # 依赖 ppath()（由 00_setup.R 定义）
  d <- file.path(ppath("logs_dir"), stage)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# 写入日志CSV：自动放到 results/logs/<stage>/<filename>
write_log_csv <- function(x, stage, filename) {
  dir <- get_log_dir(stage)
  path <- file.path(dir, filename)
  readr::write_excel_csv(x, path)
  message("[log] -> ", path)
  invisible(path)
}