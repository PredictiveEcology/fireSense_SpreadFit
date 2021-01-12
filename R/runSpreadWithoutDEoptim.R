runSpreadWithoutDEoptim <- function(sim) {
  message("SNLL_FS_thresh not specified. Self calibrating threshold value for runDEoptim...")

  seed <- sample(1e6, 1)
  set.seed(seed)

  n <- P(sim)$iterThresh ## the more you do, the lower the resulting threshold
  pars <- lapply(1:n, function(x) runif(length(P(sim)$lower), P(sim)$lower, P(sim)$upper))
  thresholds <- sample(2000, size = n)
  nCores <- ceiling(parallel::detectCores() / ceiling(parallel::detectCores() / pemisc::optimalClusterNum(7500)))
  st1 <- system.time(
    a <- mcmapply(mc.cores = min(nCores, length(pars)),
                  par = pars, FUN = .objfunSpreadFit,
                  mc.preschedule = FALSE, thresh = thresholds,
                  MoreArgs = list(
                    FS_formula = sim$fireSense_spreadFormula, #loci = loci,
                    landscape = sim$flammableRTM,
                    annualDTx1000 = lapply(sim$fireSense_annualSpreadFitCovariates, setDF),
                    nonAnnualDTx1000 = lapply(sim$fireSense_nonAnnualSpreadFitCovariates, setDF),
                    fireBufferedListDT = lapply(sim$fireBufferedListDT, setDF),
                    historicalFires = lapply(sim$lociList, setDF),
                    tests = c("SNLL_FS"),
                    covMinMax = sim$covMinMax,
                    Nreps = P(sim)$objfunFireReps,
                    maxFireSpread = P(sim)$maxFireSpread,
                    verbose = TRUE)
    )
  )
  valsdt <- data.table(thresholds = thresholds, objFun = a)
  valsdt <- valsdt[objFun < 1e5]
  threshToUse <- min(valsdt$thresholds)
  message("  using SNLL_FS_thresh value: ", threshToUse)

  threshToUse
}
