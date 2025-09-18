impute_from_se <- function(sd, se, n) {
  idx <- is.na(sd) & !is.na(se) & !is.na(n)
  sd[idx] <- se[idx] * sqrt(n[idx]); sd
}
