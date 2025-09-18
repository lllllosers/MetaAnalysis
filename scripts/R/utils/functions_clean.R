# =========================
# utils/functions_clean.R
# （深度/土层相关：修复 Excel 日期化 + 解析区间 + 构造分组）
# 依赖：%||%（在 utils/functions_common.R 中已定义）
# =========================

# 1) 规范化土层字符串：统一各种破折号/全角，修复“10月20日”等日期化
normalize_depth_string <- function(x) {
  # 输入可能是字符、日期(Date/POSIXt)、或数字
  # 目标：尽量得到形如 "a-b" 或 "a" 的字符串（单位 cm）
  if (inherits(x, "Date")) {
    # Date → "m-d"，再走统一替换（如 "10-20"）
    x <- format(x, "%m-%d")
  } else if (inherits(x, "POSIXt")) {
    x <- format(x, "%m-%d")
  }
  x <- as.character(x)
  x <- trimws(x)
  
  # Excel 本地化常见把 "10-20" 当成日期显示为 "10月20日"
  x <- gsub("月", "-",  x, perl = TRUE)
  x <- gsub("日", "",  x, perl = TRUE)
  
  # 全角/其他破折号替换成半角 - （常见：－、—、–）
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212\uFF0D]", "-", x, perl = TRUE) # 各类长短横线、减号
  
  # 去掉单位字符（若有），避免 "0-20cm" 之类干扰
  x <- gsub("(cm|CM|Cm|㎝)", "", x, perl = TRUE)
  
  # 保留数字、减号、空格；把多个空白压缩为单空格
  x <- gsub("[^0-9\\-\\s]", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  
  # 把形如 "0 - 20" 的空格去掉，统一成 "0-20"
  x <- gsub("\\s*-\\s*", "-", x, perl = TRUE)
  
  x
}

# 2) 从“规范化后”的字符串提取上下界（cm）
#   支持： "a-b"；单值 "a"（视为 a-a）
parse_depth_bounds <- function(x_norm) {
  lower <- upper <- mid <- rep(NA_real_, length(x_norm))
  
  # 匹配 a-b
  has_range <- grepl("^[0-9]+-[0-9]+$", x_norm)
  if (any(has_range)) {
    a <- suppressWarnings(as.numeric(sub("-.*$", "", x_norm[has_range])))
    b <- suppressWarnings(as.numeric(sub("^[0-9]+-", "", x_norm[has_range])))
    lower[has_range] <- pmin(a, b)
    upper[has_range] <- pmax(a, b)
    mid[has_range]   <- (lower[has_range] + upper[has_range]) / 2
  }
  
  # 匹配单值 a
  has_single <- grepl("^[0-9]+$", x_norm)
  if (any(has_single)) {
    v <- suppressWarnings(as.numeric(x_norm[has_single]))
    lower[has_single] <- v
    upper[has_single] <- v
    mid[has_single]   <- v
  }
  
  tibble::tibble(
    Depth_raw_norm = x_norm,
    Depth_low_cm   = lower,
    Depth_high_cm  = upper,
    Depth_mid_cm   = mid
  )
}

# 3) 对一列“原始深度”做完整解析：返回 tibble（含上下界与中点）
parse_depth <- function(x) {
  x_norm <- normalize_depth_string(x)
  parse_depth_bounds(x_norm)
}

# 4) 土层分组：支持 "binary" | "3bin" | "fine"
make_soil_layer <- function(depth_df, mode = "3bin",
                            top_cut = 20,
                            fine_breaks = c(0,10,20,30,40,9999),
                            fine_labels = c("0-10","10-20","20-30","30-40",">40")) {
  stopifnot(all(c("Depth_low_cm","Depth_high_cm","Depth_mid_cm") %in% names(depth_df)))
  if (mode == "binary") {
    out <- ifelse(!is.na(depth_df$Depth_high_cm) & depth_df$Depth_high_cm <= top_cut,
                  "0-20", ifelse(!is.na(depth_df$Depth_high_cm), ">20", NA))
    return(out)
  }
  if (mode == "3bin") {
    mid <- depth_df$Depth_mid_cm
    out <- ifelse(is.na(mid), NA,
                  ifelse(mid <= 10, "0-10",
                         ifelse(mid <= 20, "10-20", ">20")))
    return(out)
  }
  if (mode == "fine") {
    mid  <- depth_df$Depth_mid_cm
    brks <- fine_breaks
    brks[length(brks)] <- Inf     # 9999 视作 +Inf
    return(cut(mid, breaks = brks, labels = fine_labels,
               include.lowest = TRUE, right = TRUE))
  }
  stop("Unknown mode for make_soil_layer().")
}
