# =========================
# utils/functions_common.R
# 功能：
#   1) 健壮版默认值运算符 `%||%`
#   2) 其他轻量通用工具可继续加在此文件
# =========================

# 健壮版默认值运算符：
# 用法： a %||% b   # 若 a 为 NULL 或 单个 NA，则返回 b，否则返回 a
if (!exists("%||%")) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    # 其余情况（包括长度>1的向量/列表等）原样返回 a
    a
  }
}
