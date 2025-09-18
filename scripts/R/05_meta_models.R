# =========================
# scripts/R/05_meta_models.R
# 功能：
#   1) 读取 04 产出的效应量（data/processed/<IND>_effects.csv）
#   2) 基于 YAML 中的模型配置运行随机效应 / 多层模型
#   3) 汇总合并效应、置信区间、预测区间与异质性统计
#   4) 导出结果表、模型对象、森林图，并记录日志
# 依赖：
#   - 00_setup.R（提供 cfg、ppath、%||%、write_safe_csv、write_log_csv 等）
#   - 04_compute_effect_size.R 已产出 data/processed/<IND>_effects.csv
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)

# ---- 0.1) YAML 参数 ----
meta_params   <- cfg$model %||% list()
plot_cfg      <- cfg$plotting$forest %||% list()
digits_cfg    <- cfg$export$digits_round %||% list()

meta_method        <- meta_params$method        %||% "REML"
meta_test          <- meta_params$test          %||% "knha"
conf_level         <- meta_params$conf_level    %||% 0.95
pred_level         <- meta_params$pred_level    %||% 0.95
multilevel_req     <- isTRUE(meta_params$multilevel)
robust_req         <- isTRUE(meta_params$robust_cluster)
cluster_col_config <- meta_params$cluster_col %||% cfg$columns$study_id

# 保留位数
digits_lnRR <- digits_cfg$lnRR %||% 3
digits_pct  <- digits_cfg$pct  %||% 2
digits_tau2 <- digits_cfg$tau2 %||% 6
digits_Q    <- digits_cfg$Q    %||% 3
digits_I2   <- digits_cfg$I2   %||% 1

# 取 YAML 中路径
effects_path <- file.path(ppath("processed_dir"), paste0(cfg$indicator, "_effects.csv"))
overall_path <- file.path(ppath("tables_dir"),   paste0("overall_", cfg$indicator, "_effect.csv"))
hetero_path  <- file.path(ppath("tables_dir"),   paste0("overall_", cfg$indicator, "_heterogeneity.csv"))
model_path   <- file.path(ppath("models_dir"),   paste0("meta_model_", cfg$indicator, ".rds"))
fig_path     <- file.path(ppath("figures_dir"),  paste0("Fig_forest_", cfg$indicator, ".png"))

# 辅助函数
round_if <- function(x, digits) {
  if (is.null(digits) || is.na(digits)) return(x)
  round(x, digits)
}

pull_scalar <- function(obj, candidates) {
  for (nm in candidates) {
    val <- obj[[nm]]
    if (!is.null(val)) {
      num <- suppressWarnings(as.numeric(val))
      if (length(num) > 0) return(num[1])
    }
  }
  NA_real_
}

to_pct <- function(x) (exp(x) - 1) * 100

# ---- 1) 读取效应量数据 ----
if (!file.exists(effects_path)) {
  stop("[05_meta] Effects file not found: ", effects_path)
}

dat <- readr::read_csv(effects_path, show_col_types = FALSE)
message("[05_meta] Loaded effects: ", effects_path,
        " | rows = ", nrow(dat), ", cols = ", ncol(dat))

required_cols <- c("yi", "vi")
if (!all(required_cols %in% names(dat))) {
  stop("[05_meta] 缺少必要列：", paste(setdiff(required_cols, names(dat)), collapse = ", "))
}

dat <- dat %>%
  dplyr::mutate(
    yi = as.numeric(.data$yi),
    vi = as.numeric(.data$vi)
  ) %>%
  dplyr::filter(is.finite(.data$yi), is.finite(.data$vi), .data$vi > 0) %>%
  dplyr::mutate(.row_id = dplyr::row_number())

k <- nrow(dat)
if (k == 0L) stop("[05_meta] 无可用效应量。")

# ---- 2) 集群列准备 ----
cluster_col <- if (!is.null(cluster_col_config) && cluster_col_config %in% names(dat)) {
  cluster_col_config
} else {
  NULL
}

use_multilevel <- multilevel_req
use_robust     <- robust_req

if (is.null(cluster_col)) {
  if (use_multilevel || use_robust) {
    message("[05_meta] Cluster列在数据中不存在，multilevel/robust 功能自动禁用。")
  }
  use_multilevel <- FALSE
  use_robust     <- FALSE
  dat$.cluster_id <- paste0("ROW_", dat$.row_id)
} else {
  cluster_vals <- as.character(dat[[cluster_col]])
  cluster_vals[is.na(cluster_vals) | !nzchar(cluster_vals)] <- NA_character_
  dat$.cluster_id <- dplyr::coalesce(cluster_vals, paste0("ROW_", dat$.row_id))
}

dat$.obs_id <- dat$.row_id

# ---- 3) 拟合模型 ----
fit_control <- list(stepadj = 0.5)
rma_test    <- if (tolower(meta_test) == "knha") "knha" else meta_test

if (use_multilevel) {
  message("[05_meta] 使用 rma.mv (multilevel) 模型。")
  res <- metafor::rma.mv(
    yi     = yi,
    V      = vi,
    random = ~ 1 | .cluster_id/.obs_id,
    method = meta_method,
    test   = rma_test,
    level  = conf_level,
    control = fit_control,
    data   = dat
  )
  model_type <- "rma.mv"
} else {
  message("[05_meta] 使用 rma.uni 模型。")
  res <- metafor::rma(
    yi     = yi,
    vi     = vi,
    method = meta_method,
    test   = rma_test,
    level  = conf_level,
    control = fit_control,
    data   = dat
  )
  model_type <- "rma.uni"
}

# ---- 3.1) 聚类稳健 SE ----
robust_obj <- NULL
if (use_robust) {
  robust_obj <- tryCatch(
    {
      metafor::robust(res, cluster = dat$.cluster_id, adjust = TRUE)
    },
    error = function(e) {
      message("[05_meta] Robust SE 计算失败：", e$message)
      NULL
    }
  )
  if (!is.null(robust_obj)) {
    message("[05_meta] 已使用聚类稳健 SE (CR2)。")
  }
}

stat_obj <- if (!is.null(robust_obj)) robust_obj else res
stat_source <- if (!is.null(robust_obj)) "robust" else "model"

# ---- 4) 预测区间 ----
pred <- tryCatch(
  metafor::predict(res, level = pred_level),
  error = function(e) {
    message("[05_meta] 预测区间计算失败：", e$message)
    NULL
  }
)

pi_lb <- if (!is.null(pred) && !is.null(pred$pi.lb)) as.numeric(pred$pi.lb)[1] else NA_real_
pi_ub <- if (!is.null(pred) && !is.null(pred$pi.ub)) as.numeric(pred$pi.ub)[1] else NA_real_

# ---- 5) 合并结果表 ----
coef_est   <- pull_scalar(res,       c("beta", "b"))
se_est     <- pull_scalar(stat_obj,  c("se"))
ci_lb      <- pull_scalar(stat_obj,  c("ci.lb"))
ci_ub      <- pull_scalar(stat_obj,  c("ci.ub"))
statistic  <- pull_scalar(stat_obj,  c("zval", "tval"))
stat_type  <- if (!is.null(stat_obj$zval)) "z" else if (!is.null(stat_obj$tval)) "t" else NA_character_
p_value    <- pull_scalar(stat_obj,  c("pval"))
df_value   <- pull_scalar(stat_obj,  c("df"))

overall_tbl <- tibble::tibble(
  indicator        = cfg$indicator,
  k                = k,
  model_type       = model_type,
  method           = meta_method,
  test             = rma_test,
  conf_level       = conf_level,
  pred_level       = pred_level,
  multilevel_req   = multilevel_req,
  multilevel_fit   = use_multilevel,
  robust_req       = robust_req,
  robust_used      = !is.null(robust_obj),
  cluster_column   = cluster_col %||% NA_character_,
  inference_source = stat_source,
  statistic_type   = stat_type,
  statistic_df     = df_value,
  yi_overall       = coef_est,
  yi_ci_lb         = ci_lb,
  yi_ci_ub         = ci_ub,
  yi_se            = se_est,
  yi_statistic     = statistic,
  yi_pval          = p_value,
  pct_overall      = to_pct(coef_est),
  pct_ci_lb        = to_pct(ci_lb),
  pct_ci_ub        = to_pct(ci_ub),
  yi_pi_lb         = pi_lb,
  yi_pi_ub         = pi_ub,
  pct_pi_lb        = to_pct(pi_lb),
  pct_pi_ub        = to_pct(pi_ub)
)

overall_tbl <- overall_tbl %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c("yi_overall", "yi_ci_lb", "yi_ci_ub", "yi_se", "yi_pi_lb", "yi_pi_ub")),
      ~ round_if(.x, digits_lnRR)
    ),
    dplyr::across(
      dplyr::any_of(c("pct_overall", "pct_ci_lb", "pct_ci_ub", "pct_pi_lb", "pct_pi_ub")),
      ~ round_if(.x, digits_pct)
    ),
    yi_statistic = round_if(.data$yi_statistic, digits_lnRR)
  )

# ---- 6) 异质性统计 ----
tau2_values <- res$tau2
if (is.null(tau2_values)) tau2_values <- res$sigma2
if (is.null(tau2_values)) tau2_values <- NA_real_
tau2_total  <- if (all(is.na(tau2_values))) NA_real_ else sum(tau2_values, na.rm = TRUE)

components_txt <- if (length(tau2_values) > 1 && any(is.finite(tau2_values))) {
  paste(round_if(tau2_values, digits_tau2), collapse = "; ")
} else {
  NA_character_
}

Q_val   <- pull_scalar(res, c("QE"))
Q_pval  <- pull_scalar(res, c("QEp"))
Q_df    <- if (!is.null(res$k)) res$k - length(metafor::coef(res)) else NA_real_
I2_val  <- if (!is.null(res$I2)) suppressWarnings(as.numeric(res$I2)) else NA_real_
H2_val  <- if (!is.null(res$H2)) suppressWarnings(as.numeric(res$H2)) else NA_real_

degrees_round <- function(x, digits) {
  if (is.na(x)) return(NA_real_)
  round_if(x, digits)
}

hetero_tbl <- tibble::tibble(
  indicator        = cfg$indicator,
  k                = k,
  model_type       = model_type,
  method           = meta_method,
  Q                = Q_val,
  df_Q             = Q_df,
  Q_pval           = Q_pval,
  I2               = I2_val,
  H2               = H2_val,
  tau2_total       = tau2_total,
  tau              = if (is.na(tau2_total)) NA_real_ else sqrt(tau2_total),
  tau2_components  = components_txt
) %>%
  dplyr::mutate(
    Q          = round_if(.data$Q, digits_Q),
    I2         = round_if(.data$I2, digits_I2),
    H2         = round_if(.data$H2, digits_I2),
    tau2_total = round_if(.data$tau2_total, digits_tau2),
    tau        = round_if(.data$tau, digits_tau2)
  )

# ---- 7) 导出 ----
write_safe_csv(overall_tbl, overall_path)
write_safe_csv(hetero_tbl, hetero_path)

if (!dir.exists(dirname(model_path))) {
  dir.create(dirname(model_path), recursive = TRUE, showWarnings = FALSE)
}
saveRDS(list(model = res, robust = robust_obj), model_path)
message("[05_meta] Model RDS -> ", model_path)

write_log_csv(overall_tbl, STAGE_05, paste0("overall_", cfg$indicator, "_meta_summary.csv"))

# ---- 8) 森林图 ----
fig_width  <- plot_cfg$width_px  %||% 1800
fig_height <- plot_cfg$height_px %||% 2400
fig_dpi    <- plot_cfg$dpi       %||% 300
xlab_title <- plot_cfg$xlab      %||% "ln Response Ratio (lnRR)"

slab_candidates <- c(plot_cfg$slab_col, cfg$columns$author_year, cfg$columns$study_id)
slab_values <- NULL
for (col_name in slab_candidates) {
  if (!is.null(col_name) && !is.na(col_name) && col_name %in% names(dat)) {
    slab_values <- dat[[col_name]]
    break
  }
}
if (is.null(slab_values)) {
  slab_values <- paste0("Row ", dat$.row_id)
}

vi_sqrt <- sqrt(dat$vi)
default_xlim <- range(dat$yi + c(-4, 4) * vi_sqrt, na.rm = TRUE)
xlim_cfg <- plot_cfg$xlim %||% list(lower = NULL, upper = NULL)
if (!is.null(xlim_cfg$lower)) default_xlim[1] <- xlim_cfg$lower
if (!is.null(xlim_cfg$upper)) default_xlim[2] <- xlim_cfg$upper

if (!dir.exists(dirname(fig_path))) {
  dir.create(dirname(fig_path), recursive = TRUE, showWarnings = FALSE)
}

try({
  grDevices::png(filename = fig_path, width = fig_width, height = fig_height, res = fig_dpi)
  op <- graphics::par(mar = c(4, 10, 2, 2))
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)

  metafor::forest(
    x    = dat$yi,
    sei  = vi_sqrt,
    slab = slab_values,
    xlab = xlab_title,
    alim = default_xlim,
    level = conf_level,
    cex  = 0.8
  )
  metafor::addpoly(res, row = -1, cex = 0.9, mlab = "Overall")
}, silent = TRUE)

message("[05_meta] Forest  -> ", fig_path)
message(sprintf(
  "[05_meta] Done. lnRR = %.3f (%.3f, %.3f); I^2 = %s; method = %s",
  overall_tbl$yi_overall,
  overall_tbl$yi_ci_lb,
  overall_tbl$yi_ci_ub,
  ifelse(is.na(hetero_tbl$I2), "NA", sprintf("%.1f%%", hetero_tbl$I2)),
  meta_method
))
