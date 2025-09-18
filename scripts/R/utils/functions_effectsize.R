# =========================
# utils/functions_effectsize.R
# 计算 lnRR 与方差 v；并打上质检标签
# =========================
compute_lnRR_v <- function(df, mean_e, mean_c, sd_e, sd_c, n_e, n_c) {
  stopifnot(all(c(mean_e, mean_c, sd_e, sd_c, n_e, n_c) %in% names(df)))
  out <- df
  
  # 强制为数值
  num_cols <- c(mean_e, mean_c, sd_e, sd_c, n_e, n_c)
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))
  
  # 质检
  out$is_invalid <- FALSE
  out$invalid_reason <- NA_character_
  
  # 非正均值
  idx_mean_bad <- !(out[[mean_e]] > 0 & out[[mean_c]] > 0)
  out$is_invalid[idx_mean_bad] <- TRUE
  out$invalid_reason[idx_mean_bad] <- "NonPositiveMean"
  
  # 非正方差/样本量
  idx_var_bad <- !(out[[sd_e]] > 0 & out[[sd_c]] > 0 & out[[n_e]] > 0 & out[[n_c]] > 0)
  out$is_invalid[idx_var_bad & !out$is_invalid] <- TRUE
  out$invalid_reason[idx_var_bad & is.na(out$invalid_reason)] <- "NonPositiveV"
  
  # 缺失
  need_cols <- !is.na(out[[mean_e]]) & !is.na(out[[mean_c]]) &
    !is.na(out[[sd_e]])   & !is.na(out[[sd_c]]) &
    !is.na(out[[n_e]])    & !is.na(out[[n_c]])
  out$is_invalid[!need_cols & !out$is_invalid] <- TRUE
  out$invalid_reason[!need_cols & is.na(out$invalid_reason)] <- "NA_in_metric"
  
  # lnRR & v
  out$lnRR <- log(out[[mean_e]] / out[[mean_c]])
  out$v    <- (out[[sd_e]]^2 / (out[[n_e]] * out[[mean_e]]^2)) +
    (out[[sd_c]]^2 / (out[[n_c]] * out[[mean_c]]^2))
  
  out
}
