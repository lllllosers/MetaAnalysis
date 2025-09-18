# =========================
# scripts/R/00_setup.R
# 功能：
#   1) 安装并加载全项目所需R包
#   2) 设定全局选项（保证可复现）
#   3) 提供安全读取配置的函数
#   4) 读取全局配置（YAML）并提供路径助手
#   5) 按需安全加载 utils/ 下的工具脚本
# =========================

# ---- 1) 设置CRAN镜像 ----
repo_url <- getOption("repos")
if (is.null(repo_url) || is.na(repo_url) || repo_url[1] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# ---- 2) 所需R包清单 ----
# 说明：
# - 尽量按“精确依赖”列包名，避免一次性引入 tidyverse 全家桶（更可控）
# - 后续如需稳健方差/网络meta等，可在此列表里追加，见文末建议
required_pkgs <- c(
  # 数据处理
  "readxl", "readr", "dplyr", "tidyr", "stringr", "tibble", "yaml",
  # 元分析与统计
  "metafor", "meta",
  # 可视化
  "ggplot2", "cowplot", "ggpubr",
  # 辅助
  "here",
  # 建议追加的小工具（轻量、常用；可按需注释掉）
  "purrr" # map/检测list列等，在导出/清洗时很实用
)

# ---- 3) 安装缺失包 ----
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) {
  message("[00_setup] Installing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

# ---- 4) 加载所有包 ----
invisible(lapply(required_pkgs, function(pk) {
  suppressPackageStartupMessages(library(pk, character.only = TRUE))
}))

message("[00_setup] Packages loaded: ", paste(required_pkgs, collapse = ", "))

# ---- 5) 全局选项 ----
options(
  stringsAsFactors = FALSE, # 避免自动转因子
  scipen = 999, # 避免科学计数法
  dplyr.summarise.inform = FALSE,
  digits = 7 # 控制打印有效位
)
# Windows 控制台UTF-8（尽量避免中文乱码）
if (.Platform$OS.type == "windows") {
  try(suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese")))
}

# ---- 6) 配置读取工具函数 ----
read_yaml_safe <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("[00_setup] Config file not found: %s", path))
  }
  tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      stop(sprintf("[00_setup] Failed to read YAML (%s): %s", path, e$message))
    }
  )
}

# （新增）安全 source 工具：若文件存在则加载，不存在仅提示
safe_source <- function(path) {
  if (file.exists(path)) {
    source(path, local = FALSE, encoding = "UTF-8")
    message("[00_setup] Sourced: ", path)
  } else {
    message("[00_setup] (skip) Not found: ", path)
  }
}

# ---- 7) 读取全局配置（YAML）并提供路径助手 ----
root <- here::here()
cfg_path <- file.path(root, "config", "meta_config.yaml")
cfg <- read_yaml_safe(cfg_path)
message("[00_setup] Config loaded. Indicator = ", cfg$indicator)

# 路径助手：ppath("interim_dir") 等同于 file.path(root, cfg$paths$interim_dir)
ppath <- function(key) {
  val <- cfg$paths[[key]]
  if (is.null(val)) stop(sprintf("[00_setup] Missing path key in YAML: %s", key))
  file.path(root, val)
}

# 暴露 cfg/ppath 到全局，方便后续脚本直接使用
assign("cfg", cfg, envir = .GlobalEnv)
assign("ppath", ppath, envir = .GlobalEnv)

# ---- 8) 按需加载 utils/ 下的工具脚本（不存在也不报错）----
safe_source(file.path(root, "scripts/R/utils", "functions_common.R"))    # << 新增：放最前，提供 %||%
safe_source(file.path(root, "scripts/R/utils", "functions_export.R"))    # 统一CSV导出：write_safe_csv()
safe_source(file.path(root, "scripts/R/utils", "functions_effectsize.R"))# lnRR与v计算：compute_lnRR_v()
safe_source(file.path(root, "scripts/R/utils", "functions_clean.R"))     # 清洗/单位/土层分组工具
safe_source(file.path(root, "scripts/R/utils", "functions_impute.R"))    # SD补齐的封装（可选）

# 可选：记录一次 sessionInfo（便于复现）
sess_txt <- capture.output(utils::sessionInfo())
write_log_csv(data.frame(session = sess_txt), STAGE_00, "session_info.txt")

message("[00_setup] Global options set. Ready.")
