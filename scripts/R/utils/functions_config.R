# =========================
# utils/functions_config.R
# 读取 YAML（带更清晰的报错）
# =========================
read_yaml_safe <- function(path) {
  if (!file.exists(path)) stop(sprintf("[config] Not found: %s", path))
  y <- tryCatch(yaml::read_yaml(path), error = function(e) e)
  if (inherits(y, "error")) stop(sprintf("[config] YAML parse error at %s:\n%s", path, y$message))
  y
}

