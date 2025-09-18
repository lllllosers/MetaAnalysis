# =========================
# scripts/R/05_meta_aggregate.R
# 功能：
#   1) 读取 04 的效应量数据（data/interim/<IND>_effects.csv）
#   2) 用随机效应模型（默认 REML，可在 YAML: cfg$meta$estimator 设置）合并 lnRR
#   3) 导出整体效应（lnRR 与 回译%变化）、95%CI、预测区间（PI）
#   4) 导出异质性统计（Q/df/p、I2、H2、tau2、tau）
#   5) 绘制森林图并保存（基础清晰版；后续 06 做亚组/分面森林）
# 依赖：
#   - 00_setup.R（提供 cfg、ppath、%||%、write_safe_csv 等）
#   - 04_effect_size.R 已产出 data/interim/<IND>_effects.csv
# =========================

# ---- 0) 环境与配置 ----
source("scripts/R/00_setup.R")

root <- here::here()
cfg  <- get("cfg", envir = .GlobalEnv)

# ---- 0.1) 从 YAML 配置读取参数 ----
meta_params <- cfg$model

# 配置：随机效应合并方法、假设检验、置信区间及预测区间
meta_estimator   <- (meta_params$method   %||% "REML")     # 默认 "REML"
meta_test        <- (meta_params$test        %||% "knha")     # 默认 "knha"
prediction_level <- (meta_params$pred_level  %||% 0.95)       # 预测区间置信度
conf_level       <- (meta_params$conf_level  %||% 0.95)       # 合并效应置信区间
digits_out       <- (cfg$export$digits_round       %||% 3)          # 结果显示精度

# ---- 1) 读取 04 的效应量数据 ----
eff_path <- file.path(ppath("processed_dir"), paste0(cfg$indicator, "_effects.csv"))
stopifnot(file.exists(eff_path))
dat <- readr::read_csv(eff_path, show_col_types = FALSE)
message("[05_meta] Loaded effects: ", eff_path, " | rows = ", nrow(dat), ", cols = ", ncol(dat))

# ---- 2) 基本可用性筛查（双保险；04 已经筛过，这里仅防御）----
need_cols <- c("yi", "vi")
if (!all(need_cols %in% names(dat))) {
  stop("[05_meta] 缺少必要列：", paste(setdiff(need_cols, names(dat)), collapse = ", "))
}
dat <- dat %>%
  dplyr::filter(is.finite(yi), is.finite(vi), vi > 0)
k <- nrow(dat)
if (k == 0L) stop("[05_meta] 无可用效应量。")

# ---- 3) 随机效应合并（metafor::rma）----
# test:
#  - "knha"：Knapp-Hartung 稳健t检验（推荐小样本/高异质）
#  - 其他传给 rma 的 test 参数（"z" 等）
rma_test <- if (tolower(meta_test) == "knha") "knha" else meta_test

res <- metafor::rma(
  yi = dat$yi, vi = dat$vi,
  method = meta_estimator,
  test   = rma_test,
  control = list(stepadj = 0.5) # 收敛稳定性的小调整，必要可注释
)

# 预测区间（PI）
pred <- predict(res, level = prediction_level)

# ---- 4) 组织整体结果并回译为百分比 ----
to_pct <- function(x) (exp(x) - 1) * 100

overall_tbl <- tibble::tibble(
  indicator     = cfg$indicator,
  k             = k,
  method        = meta_estimator,
  test          = rma_test,
  conf_level    = conf_level,
  pred_level    = prediction_level,
  yi_overall    = unname(res$b[1]),
  yi_ci_lb      = unname(res$ci.lb),
  yi_ci_ub      = unname(res$ci.ub),
  yi_se         = unname(res$se),
  yi_zval       = unname(res$zval %||% NA_real_),  # knha 返回 t，z 可能NA
  yi_pval       = unname(res$pval),
  # 回译为百分比变化（更直观）
  pct_overall   = to_pct(res$b[1]),
  pct_ci_lb     = to_pct(res$ci.lb),
  pct_ci_ub     = to_pct(res$ci.ub),
  # 预测区间（以 lnRR 给出，同时回译%）
  yi_pi_lb      = unname(pred$pi.lb),
  yi_pi_ub      = unname(pred$pi.ub),
  pct_pi_lb     = to_pct(pred$pi.lb),
  pct_pi_ub     = to_pct(pred$pi.ub)
)

# 异质性统计
het_tbl <- tibble::tibble(
  indicator  = cfg$indicator,
  Q          = unname(res$QE),
  df_Q       = unname(res$k - 1),
  Q_pval     = unname(res$QEp),
  I2         = unname(res$I2),
  H2         = unname(res$H2),
  tau2       = unname(res$tau2),
  tau        = sqrt(unname(res$tau2))
)

# ---- 5) 导出整体与异质性 ----
overall_path <- file.path("results", paste0("overall_", cfg$indicator, "_effect.csv"))
het_path     <- file.path("results", paste0("overall_", cfg$indicator, "_heterogeneity.csv"))
readr::write_excel_csv(overall_tbl, overall_path)
readr::write_excel_csv(het_tbl,     het_path)
message("[05_meta] Overall  -> ", overall_path)
message("[05_meta] Hetero   -> ", het_path)

# ---- 6) 基础森林图（metafor 原生绘制，稳定可靠）----
# 备注：后续 06 我们会做“亚组森林/分面森林”的 ggplot 版本
fig_dir <- file.path("results", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
fig_path <- file.path(fig_dir, paste0("Fig_forest_", cfg$indicator, ".png"))

# 研究标签（如果存在 Author_Year/StudyID 就显示在左侧）
lab_left <- NULL
if (!is.null(cfg$columns$author_year) && cfg$columns$author_year %in% names(dat)) {
  lab_left <- dat[[cfg$columns$author_year]]
} else if (!is.null(cfg$columns$study_id) && cfg$columns$study_id %in% names(dat)) {
  lab_left <- paste0("Study ", dat[[cfg$columns$study_id]])
} else {
  lab_left <- paste0("Row ", seq_len(nrow(dat)))
}

png(filename = fig_path, width = 1800, height = 2200, res = 250)
op <- par(mar = c(4, 10, 2, 2))  # 左边更宽放标签
try({
  metafor::forest(
    x    = dat$yi, sei = sqrt(dat$vi),
    slab = lab_left,
    xlab = "lnRR (log response ratio)",
    alim = c(min(dat$yi - 4*sqrt(dat$vi), na.rm = TRUE),
             max(dat$yi + 4*sqrt(dat$vi), na.rm = TRUE)),
    cex  = 0.8
  )
  # 加入合并效应与其置信区间
  metafor::addpoly(res, row = -1, cex = 0.9, mlab = "Overall")
}, silent = TRUE)
par(op)
dev.off()

message("[05_meta] Forest  -> ", fig_path)
message(sprintf("[05_meta] Done. 合并 lnRR = %.3f (%.3f, %.3f) ; I^2 = %.1f%%",
                overall_tbl$yi_overall, overall_tbl$yi_ci_lb, overall_tbl$yi_ci_ub, het_tbl$I2))

# ---- 7) 小提示：06 做亚组/元回归时的准备 ----
# 06_subgroup_meta.R 会读取本脚本的同一路径效应数据（<IND>_effects.csv），
# 并基于 Soil_layer / Region / Crop types / Fert 等做分组 rma 或 meta-reg。
