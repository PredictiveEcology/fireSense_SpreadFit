## The parameter sets the ledger records for this ELF: the `n` distinct members of the final population
## with the lowest replicated mean (fireSenseUtils::runDEoptim() attaches the re-score as
## attr(DE, "finalRescore")). Without a re-score, the final population's own values are used.
##
## This replaces "the 5 generations with the lowest best value", which were five copies of one frozen,
## usually lucky, member (FireSense, 2026-09-17).
bestParamSets <- function(DE, parNames, n = 5L) {
  last <- DE[[length(DE)]]$member
  pop <- last$pop
  colnames(pop) <- parNames
  scores <- attr(DE, "finalRescore")
  if (is.null(scores))
    scores <- data.table::data.table(member = seq_len(NROW(pop)), rep = 1L, value = as.numeric(last$popval))
  best <- fireSenseUtils::bestByReplicatedMean(pop, scores, n = n)
  list(params = best$params, objFunVal = best$objFunVal)
}
