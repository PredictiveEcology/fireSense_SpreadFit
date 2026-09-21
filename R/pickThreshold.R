#' The calibrated SNLL threshold: the smallest threshold whose trial did not fail
#'
#' A trial that bailed returns fireSenseUtils' failVal (1e6), plus the adTest term when that test is on.
#' Anything below that is a real value, however large: an ELF's objective is a sum over its fires, so
#' six-figure values are normal for a big ELF and must not be mistaken for failures.
#'
#' @param thresholds numeric; candidate thresholds.
#' @param objFun objective value of each candidate; non-numeric or non-finite entries count as failed.
#' @param failVal numeric; values at or above this are failed trials.
#' @return the smallest non-failed threshold, or `NA_real_` with a warning if every trial failed.
pickThreshold <- function(thresholds, objFun, failVal = 1e6) {
  v <- suppressWarnings(as.numeric(objFun))
  ok <- is.finite(v) & v < failVal
  if (!any(ok)) {
    warning("no threshold calibrated: every trial failed (objective >= ", failVal, ")")
    return(NA_real_)
  }
  min(thresholds[ok])
}
