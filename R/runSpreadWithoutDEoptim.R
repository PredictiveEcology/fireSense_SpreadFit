runSpreadWithoutDEoptim <- function(iterThresh, lower, upper, fireSense_spreadFormula, flammableRTM,
                                    annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                                    historicalFires, covMinMax, objfunFireReps, maxFireSpread,
                                    pars = NULL) {
  seed <- sample(1e6, 1)
  set.seed(seed)

  n <- iterThresh ## the more you do, the lower the resulting threshold
  message("SNLL_FS_thresh not specified. Self calibrating threshold value for runDEoptim (n=", n, ")")

  if (is.null(pars)) {
    pars <- lapply(1:n, function(x) runif(length(lower), lower, upper))
    userPars <- FALSE
  } else {
    userPars <- TRUE
  }
  if (!is(pars, "list")) pars <- list(pars)
  hfs <- rbindlist(historicalFires)[size > 1]
  hfsSizes <- hfs[, list(AAB = sum(size)), by = "date"]
  setorderv(hfsSizes, "AAB", order = -1L)
  # next is rough estimate of an SNLL value that should be "decent"
  decentEstimateThreshold <- 7 * NROW(hfs[date %in% hfsSizes$date[1:2]])

  thresholds <- sample(3 * decentEstimateThreshold, size = n)

  if (iterThresh == 1) {
    message("performing just a simple .objFunSpreadFit run, because iterThresh = 1")

    # Eliot -- this overrides the argument passed in because it is more useful to see
    #   many values go by
    if (!userPars) {
      n <- 192
      pars <- lapply(1:n, function(x) runif(length(lower), lower, upper))
    }
    a <- list()
    for (i in seq(pars)) {
      print(paste(i, "logit params:", paste(round(pars[[i]], 2), collapse = ", ")))
      a[[i]] <- .objfunSpreadFit(par = pars[[i]],
                       thresh = decentEstimateThreshold,
                       FS_formula = fireSense_spreadFormula, #loci = loci,
                       landscape = flammableRTM,
                       annualDTx1000 = annualDTx1000,
                       nonAnnualDTx1000 = nonAnnualDTx1000,
                       fireBufferedListDT = fireBufferedListDT,
                       mutuallyExclusive = list("youngAge" = c("vegPC")),
                       doAssertions = TRUE,
                       historicalFires = historicalFires,
                       tests = c("SNLL_FS", "adtest"),
                       covMinMax = covMinMax,
                       Nreps = objfunFireReps,
                       maxFireSpread = maxFireSpread,
                       verbose = TRUE, plot.it = TRUE
      )
    }
    hf <- historicalFires$year2018
    hasAnn <- annualDTx1000$year2018
    hasNonAnn <- nonAnnualDTx1000$year2011_year2012_year2013_year2014_year2015_year2016_year2017_year2018_year2019
    hf$cells %in% hasAnn$pixelID

  } else {
    nCores <- pemisc::optimalClusterNum(16000)
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
                      mutuallyExclusive = list("youngAge" = c("vegPC")),
                      doAssertions = TRUE,
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
    return(threshToUse)
  }

}
