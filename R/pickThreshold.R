## The calibrated SNLL threshold: the smallest threshold whose trial did not fail.
##
## `objFun` holds one objective value per candidate threshold. A trial that bailed returns
## fireSenseUtils' failVal (1e6) -- plus the adTest term when that test is on, hence `>=`. Anything below
## that is a real value, however large: an ELF's objective is a sum over its fires, so six-figure values are
## normal for a big ELF and must not be mistaken for failures (the old cutoff, `objFun < 1e5`, dropped every
## row for such an ELF and left thresh = Inf, i.e. no early bail at all -- ELF 6.1.1, 2026-09-16).
pickThreshold <- function(thresholds, objFun, failVal = 1e6) {
  v <- suppressWarnings(as.numeric(objFun))
  ok <- is.finite(v) & v < failVal
  if (!any(ok)) {
    warning("no threshold calibrated: every trial failed (objective >= ", failVal, ")")
    return(NA_real_)
  }
  min(thresholds[ok])
}
