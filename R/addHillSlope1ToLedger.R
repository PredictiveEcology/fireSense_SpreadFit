#' Insert the fixed `hillSlope1` into the ledger's parameter sets
#'
#' `hillSlope1` (the spread link's slope) is fixed at 1, not fitted (see `estimateSpreadParams()`):
#' it is not identifiable together with the covariate coefficients, so it is no longer part of
#' `DEoptim`'s parameter space, and `paramsBest` (from `bestParamSets()`) does not include it either.
#' `fireSense_spreadPredict` splits a ledger row's parameters from its covariates BY NAME, and the
#' spread link (`fireSenseUtils::logistic3p()`/`logistic3pUpper()`) reads the result BY POSITION --
#' `maxAsymptote`, `hillSlope1`, `inflectionPoint1`, and (with the upper-tail link) `upperTail1`.
#' This restores that position for
#' new fits, so a new ledger row predicts with `hillSlope1 = 1` exactly like an old row whose
#' `hillSlope1` happened to be fitted at that value, and an old row keeps predicting with its own
#' fitted value.
#'
#' @param paramsBest a `data.table`, one row per parameter set, as `bestParamSets()$params` returns
#'   it: columns named `names(P(sim)$lower)`, so without `hillSlope1`, `maxAsymptote` first.
#' @return `paramsBest` with a `hillSlope1` column of 1s inserted right after `maxAsymptote`.
addHillSlope1ToLedger <- function(paramsBest) {
  paramsBest <- data.table::copy(paramsBest)
  data.table::set(paramsBest, NULL, "hillSlope1", 1)
  nm <- append(setdiff(names(paramsBest), "hillSlope1"), "hillSlope1", after = 1L)
  data.table::setcolorder(paramsBest, nm)
  paramsBest
}
