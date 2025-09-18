source("scripts/R/00_setup.R")
root <- here::here()
cfg  <- read_yaml_safe(file.path(root, "config", "meta_config.yaml"))

m1 <- readr::read_csv(file.path(root, "data/interim/soc_imputed.csv"), show_col_types = FALSE)              # 手工
m2 <- readr::read_csv(file.path(root, "data/interim/soc_imputed_metagear.csv"), show_col_types = FALSE)     # metagear

# 按行号对齐比较（确保两表行顺序一致；如不一致需设置唯一键来 join）
stopifnot(nrow(m1) == nrow(m2))
cmp <- tibble::tibble(
  row_id = seq_len(nrow(m1)),
  SD_E_use_manual    = m1$SD_E_use,
  SD_E_use_metagear  = m2$SD_E_use,
  SD_C_use_manual    = m1$SD_C_use,
  SD_C_use_metagear  = m2$SD_C_use
) %>%
  dplyr::mutate(
    diff_E = SD_E_use_manual - SD_E_use_metagear,
    diff_C = SD_C_use_manual - SD_C_use_metagear
  )

readr::write_excel_csv(cmp, file.path(root, "results/logs/impute_soc_compare_manual_vs_metagear.csv"))
message("[03c_compare] 差异表已写入 results/logs/impute_soc_compare_manual_vs_metagear.csv")
