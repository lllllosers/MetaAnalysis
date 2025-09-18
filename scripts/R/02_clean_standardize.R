# =========================
# scripts/R/02_clean_standardize.R
# 功能：
#   1) 读取原始长表（Excel），标准化关键字段（单位、土层），生成 *_std 列
#   2) 仅在“目标指标”（cfg$indicator）上示例化单位换算（SOC→g/kg，可扩展）
#   3) 深度/土层：纠偏 Excel 日期化（如“10月20日”），构造 Soil_layer（ASCII）与“导出安全”列
#   4) 输出 data/interim/LongData_clean.csv，并写日志到 results/logs/02_clean_standardize/
# 依赖：
#   - 00_setup.R（提供 cfg、ppath、%||%、write_safe_csv、write_log_csv、STAGE_02）
#   - utils/functions_common.R（%||%）
#   - utils/functions_export.R（write_log_csv, STAGE_02）
# 说明：
#   - 内部分析统一使用 ASCII 的 Soil_layer（0-10/10-20/>20）
#   - 为避免 Excel 将 “10-20” 识别成日期，另外生成“导出安全”列（默认把 - 改为 _）
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)

# 列名映射（来自 YAML）
col_map       <- cfg$columns
col_indicator <- col_map$indicator
col_unit      <- col_map$unit
col_mean_e    <- col_map$mean_e
col_sd_e      <- col_map$sd_e
col_n_e       <- col_map$n_e
col_mean_c    <- col_map$mean_c
col_sd_c      <- col_map$sd_c
col_n_c       <- col_map$n_c
col_depth     <- col_map$depth_cm %||% NA_character_

# Excel 展示保护策略（用于“导出安全”列；不影响内部分析）：
#   - "underscore"（默认）：将 "10-20" → "10_20"
#   - "apostrophe"：在 "10-20" 前加 "'"，Excel 以文本显示（视觉仍是 10-20）
#   - "unit_suffix"：在 "10-20" 后加 " cm"
#   - "en_dash"：将 "-" 改为 EN DASH "–"
excel_label_mode <- tryCatch(cfg$export$excel_label_mode, error = function(...) NA_character_) %||% "underscore"

# ---- 0.1) 最小可用工具（若 utils 未定义时启用）----

# SOC → g/kg（示例；其他指标可在 utils 扩展）
if (!exists("convert_soc_to_g_per_kg")) {
  convert_soc_to_g_per_kg <- function(value, unit) {
    u <- trimws(as.character(unit))
    v <- suppressWarnings(as.numeric(value))
    out <- rep(NA_real_, length(v))
    # g/kg, g·kg-1, g kg-1
    hit_gkg <- grepl("^(g\\s*/?\\s*kg|g\\s*·\\s*kg\\s*-?1|g\\s*kg\\s*-?1)$", u, ignore.case = TRUE)
    out[hit_gkg] <- v[hit_gkg]
    # mg/g == g/kg
    hit_mgg <- grepl("^mg\\s*/\\s*g$", u, ignore.case = TRUE)
    out[hit_mgg] <- v[hit_mgg]
    # % → g/kg（1% = 10 g/kg）
    hit_pct <- grepl("^%$|percent|百分", u, ignore.case = TRUE)
    out[hit_pct] <- v[hit_pct] * 10
    out
  }
}

# 规范化“土层原始字符串”：修复“10月20日”、全角破折号、单位等
normalize_depth_string <- function(x) {
  if (inherits(x, "Date"))   x <- format(x, "%m-%d")  # 避免本地化“10月20日”
  if (inherits(x, "POSIXt")) x <- format(x, "%m-%d")
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("月", "-", x, perl = TRUE)        # "10月20日" → "10-20"
  x <- gsub("日", "", x,  perl = TRUE)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212\uFF0D]", "-", x, perl = TRUE)  # 全角/长横 → "-"
  x <- gsub("(cm|CM|Cm|㎝)", "", x, perl = TRUE)     # 去单位
  x <- gsub("[^0-9\\-\\s]", "", x, perl = TRUE)      # 仅保留数字、减号、空格
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  x <- gsub("\\s*-\\s*", "-", x, perl = TRUE)        # "0 - 20" → "0-20"
  x
}

# 提取上下界（支持 "a-b" 与 "a"）
parse_depth_bounds <- function(x_norm) {
  lower <- upper <- mid <- rep(NA_real_, length(x_norm))
  has_range <- grepl("^[0-9]+-[0-9]+$", x_norm)
  if (any(has_range)) {
    a <- suppressWarnings(as.numeric(sub("-.*$", "", x_norm[has_range])))
    b <- suppressWarnings(as.numeric(sub("^[0-9]+-", "", x_norm[has_range])))
    lower[has_range] <- pmin(a, b)
    upper[has_range] <- pmax(a, b)
    mid[has_range]   <- (lower[has_range] + upper[has_range]) / 2
  }
  has_single <- grepl("^[0-9]+$", x_norm)
  if (any(has_single)) {
    v <- suppressWarnings(as.numeric(x_norm[has_single]))
    lower[has_single] <- v
    upper[has_single] <- v
    mid[has_single]   <- v
  }
  tibble::tibble(
    Depth_raw_norm = x_norm,  # 纠偏后的原始字符串（便于排查）
    Depth_low_cm   = lower,
    Depth_high_cm  = upper,
    Depth_mid_cm   = mid
  )
}

# 总入口
parse_depth <- function(x) parse_depth_bounds(normalize_depth_string(x))

# 3bin（内部分析用，ASCII，不带单位）
make_soil_layer_ascii <- function(depth_df) {
  mid <- depth_df$Depth_mid_cm
  out <- ifelse(is.na(mid), NA,
                ifelse(mid <= 10, "0-10",
                       ifelse(mid <= 20, "10-20", ">20")))
  out
}

# “导出安全”标签（仅用于展示/Excel；不影响内部分析）
protect_label_for_excel <- function(x_ascii, mode = "underscore") {
  x <- as.character(x_ascii)
  need <- grepl("^[0-9]{1,2}-[0-9]{1,2}$", x)  # 易被误判为日期的模式
  if (mode == "underscore") {
    x[need] <- gsub("-", "_", x[need], fixed = TRUE)        # 10-20 → 10_20
    return(x)
  }
  if (mode == "apostrophe") {
    x[need] <- paste0("'", x[need])                         # '10-20
    return(x)
  }
  if (mode == "unit_suffix") {
    x[need] <- paste0(x[need], " cm")                       # 10-20 cm
    return(x)
  }
  if (mode == "en_dash") {
    x[need] <- gsub("-", "\u2013", x[need], fixed = TRUE)   # 10–20
    return(x)
  }
  # 默认 fallback
  x[need] <- gsub("-", "_", x[need], fixed = TRUE)
  x
}

# ---- 1) 读取原始数据 ----
raw_xlsx <- ppath("raw_xlsx")
stopifnot(file.exists(raw_xlsx))
stopifnot(cfg$sheets$longdata %in% readxl::excel_sheets(raw_xlsx))

dat <- readxl::read_excel(raw_xlsx, sheet = cfg$sheets$longdata)
message("[02_clean] Loaded raw: ", raw_xlsx, " | rows = ", nrow(dat), ", cols = ", ncol(dat))

# 强制把“土层列”当字符，避免 readxl 读成 Date
if (!is.na(col_depth) && col_depth %in% names(dat)) {
  dat[[col_depth]] <- as.character(dat[[col_depth]])
}

# ---- 2) 生成 *_std 列（先复制原值，后按规则覆盖）----
dat <- dat %>%
  dplyr::mutate(
    Mean_E_std = .data[[col_mean_e]],
    Mean_C_std = .data[[col_mean_c]],
    SD_E_std   = .data[[col_sd_e]],
    SD_C_std   = .data[[col_sd_c]]
  )

# ---- 3) 单位标准化（以 SOC → g/kg 为例，仅对目标指标）----
target_indicator <- cfg$indicator
is_target <- dat[[col_indicator]] == target_indicator

if (col_unit %in% names(dat) && any(is_target, na.rm = TRUE)) {
  new_me_e <- convert_soc_to_g_per_kg(dat[[col_mean_e]][is_target], dat[[col_unit]][is_target])
  new_me_c <- convert_soc_to_g_per_kg(dat[[col_mean_c]][is_target], dat[[col_unit]][is_target])
  new_sd_e <- convert_soc_to_g_per_kg(dat[[col_sd_e]][is_target], dat[[col_unit]][is_target])
  new_sd_c <- convert_soc_to_g_per_kg(dat[[col_sd_c]][is_target], dat[[col_unit]][is_target])
  
  ok_me_e <- !is.na(new_me_e); dat$Mean_E_std[is_target][ok_me_e] <- new_me_e[ok_me_e]
  ok_me_c <- !is.na(new_me_c); dat$Mean_C_std[is_target][ok_me_c] <- new_me_c[ok_me_c]
  ok_sd_e <- !is.na(new_sd_e); dat$SD_E_std[is_target][ok_sd_e]   <- new_sd_e[ok_sd_e]
  ok_sd_c <- !is.na(new_sd_c); dat$SD_C_std[is_target][ok_sd_c]   <- new_sd_c[ok_sd_c]
} else {
  message("[02_clean] Unit column missing or no target rows; skip unit conversion.")
}

# ---- 4) 深度解析与土层分组 ----
if (!is.na(col_depth) && col_depth %in% names(dat)) {
  depth_df <- parse_depth(dat[[col_depth]])                    # 纠偏+解析
  soil_layer_ascii <- make_soil_layer_ascii(depth_df)          # 内部分析用
  soil_layer_safe  <- protect_label_for_excel(soil_layer_ascii, mode = excel_label_mode) # 导出安全
  
  dat <- dplyr::bind_cols(dat, depth_df) %>%
    dplyr::mutate(
      Soil_layer        = soil_layer_ascii,  # 分析用（ASCII）
      Soil_layer_export = soil_layer_safe    # Excel 安全展示用
    )
} else {
  message("[02_clean] Depth column not found or not set in YAML; skip soil-layer grouping.")
  dat$Depth_raw_norm   <- NA_character_
  dat$Depth_low_cm     <- NA_real_
  dat$Depth_high_cm    <- NA_real_
  dat$Depth_mid_cm     <- NA_real_
  dat$Soil_layer       <- NA_character_
  dat$Soil_layer_export<- NA_character_
}

# ---- 5) 质检与日志 ----
to_num <- function(x) suppressWarnings(as.numeric(x))
check_tbl <- dat %>%
  dplyr::filter(.data[[col_indicator]] == target_indicator) %>%
  dplyr::transmute(
    Mean_E_std_nonpos = !(to_num(Mean_E_std) > 0),
    Mean_C_std_nonpos = !(to_num(Mean_C_std) > 0),
    SD_E_std_neg      = (to_num(SD_E_std) < 0),
    SD_C_std_neg      = (to_num(SD_C_std) < 0)
  )

summary_nonpos <- tibble::tibble(
  rows_target   = sum(dat[[col_indicator]] == target_indicator, na.rm = TRUE),
  Mean_E_nonpos = sum(check_tbl$Mean_E_std_nonpos, na.rm = TRUE),
  Mean_C_nonpos = sum(check_tbl$Mean_C_std_nonpos, na.rm = TRUE),
  SD_E_negative = sum(check_tbl$SD_E_std_neg, na.rm = TRUE),
  SD_C_negative = sum(check_tbl$SD_C_std_neg, na.rm = TRUE)
)

# 分布日志（展示用列，确保 Excel 不再日期化）
soil_layer_counts <- dat %>%
  dplyr::count(Soil_layer = .data$Soil_layer_export, name = "n") %>%
  dplyr::arrange(dplyr::desc(n))

units_after <- if (col_unit %in% names(dat)) {
  dat %>% dplyr::count(Unit = .data[[col_unit]], name = "n") %>% dplyr::arrange(dplyr::desc(n))
} else {
  tibble::tibble(Unit = character(0), n = integer(0))
}

# ---- 6) 导出 ----
# 数据主体（包含：Soil_layer 与 Soil_layer_export）
out_clean <- file.path(ppath("interim_dir"), "LongData_clean.csv")
write_safe_csv(dat, out_clean)
message("[02_clean] Clean data -> ", out_clean)

# 日志
write_log_csv(units_after,       STAGE_02, "units_overview_after_clean.csv")
write_log_csv(soil_layer_counts, STAGE_02, "soil_layer_counts.csv")
write_log_csv(summary_nonpos,    STAGE_02, sprintf("check_%s_std_nonpos.csv", cfg$indicator))

message("[02_clean] Logs -> results/logs/02_clean_standardize/")
message("[02_clean] 建议：可复跑 01_validate_input.R（在标准化后再自检一次），然后运行 03_impute_variance.R")
