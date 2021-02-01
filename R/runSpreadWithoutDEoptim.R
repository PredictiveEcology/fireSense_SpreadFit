runSpreadWithoutDEoptim <- function(iterThresh, lower, upper, fireSense_spreadFormula, flammableRTM,
                                    annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                                    historicalFires, covMinMax, objfunFireReps, maxFireSpread) {
  seed <- sample(1e6, 1)
  set.seed(seed)

  n <- iterThresh ## the more you do, the lower the resulting threshold
  message("SNLL_FS_thresh not specified. Self calibrating threshold value for runDEoptim (n=", n, ")")

  pars <- lapply(1:n, function(x) runif(length(lower), lower, upper))
  thresholds <- sample(2000, size = n)
  nCores <- pemisc::optimalClusterNum(10000)
  # nCores <- ceiling(parallel::detectCores() / ceiling(parallel::detectCores() / pemisc::optimalClusterNum(10000)))
  message("Using ", nCores, " cores.")

  st1 <- system.time(
    a <- mcmapply(mc.cores = min(nCores, length(pars)),
                  par = pars, FUN = .objfunSpreadFit,
                  mc.preschedule = FALSE, thresh = thresholds,
                  MoreArgs = list(
                    FS_formula = fireSense_spreadFormula, #loci = loci,
                    landscape = flammableRTM,
                    annualDTx1000 = annualDTx1000,
                    nonAnnualDTx1000 = nonAnnualDTx1000,
                    fireBufferedListDT = fireBufferedListDT,
                    historicalFires = historicalFires,
                    tests = c("SNLL_FS"),
                    covMinMax = covMinMax,
                    Nreps = objfunFireReps,
                    maxFireSpread = maxFireSpread,
                    verbose = TRUE)
    )
  )
  valsdt <- data.table(thresholds = thresholds, objFun = a)
  valsdt <- valsdt[objFun < 1e5]
  threshToUse <- min(valsdt$thresholds)
  message("  using SNLL_FS_thresh value: ", threshToUse)

  threshToUse
}
