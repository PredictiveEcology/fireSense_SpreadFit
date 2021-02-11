runSpreadWithoutDEoptim <- function(iterThresh, lower, upper, fireSense_spreadFormula, flammableRTM,
                                    annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                                    doObjFunAssertions = getOption("fireSenseUtils.assertions", TRUE),
                                    historicalFires, covMinMax, objfunFireReps, maxFireSpread,
                                    weighted = TRUE,
                                    pars = NULL, plot.it = TRUE, mode = "fit") {
  seed <- sample(1e6, 1)
  set.seed(seed)

  n <- iterThresh ## the more you do, the lower the resulting threshold
  message("SNLL_FS_thresh not specified. Self calibrating threshold value for runDEoptim (n=", n, ")")

  hfs <- rbindlist(historicalFires)[size > 1]
  hfsSizes <- hfs[, list(AAB = sum(size)), by = "date"]
  setorderv(hfsSizes, "AAB", order = -1L)
  # next is rough estimate of an SNLL value that should be "decent"
  largestYear <- hfsSizes$date[1]
  largestFireInLargestYear <- max(hfs[grep(largestYear, hfs$date)]$size)
  decentEstimateThreshold <- NROW(hfs[date %in% hfsSizes$date[1:2]]) *
    (log(largestFireInLargestYear) ^ weighted)

  if (is.null(pars)) {
    seed <- sample(1e6, 1)
    set.seed(seed)
    print(paste("seed used for runSpreadWithoutDEoptim is ", seed))
    pars <- lapply(1:n, function(x) runif(length(lower), lower, upper))
    userPars <- FALSE
    thresholds <- sample(3 * decentEstimateThreshold, size = n)
  } else {
    userPars <- TRUE
    thresholds <- 1e8
  }
  if (!is(pars, "list")) pars <- list(pars)

  if (mode %in% "debug") {
    a <- list()
    for (i in seq(pars)) {
      print(paste(i, "logit params:", paste(round(pars[[i]], 2), collapse = ", ")))
      a[[i]] <- .objfunSpreadFit(par = pars[[i]],
                                 thresh = thresholds,
                                 FS_formula = fireSense_spreadFormula, #loci = loci,
                                 landscape = flammableRTM,
                                 annualDTx1000 = annualDTx1000,
                                 nonAnnualDTx1000 = nonAnnualDTx1000,
                                 fireBufferedListDT = fireBufferedListDT,
                                 mutuallyExclusive = list("youngAge" = c("vegPC")),
                                 doAssertions = doObjFunAssertions,
                                 historicalFires = historicalFires,
                                 tests = c("SNLL_FS", "adtest"),
                                 covMinMax = covMinMax,
                                 Nreps = objfunFireReps,
                                 maxFireSpread = maxFireSpread,
                                 verbose = TRUE,
                                 weighted = weighted,
                                 plot.it = plot.it
                                 #plot.it = TRUE
      )
    }
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
                      doAssertions = doObjFunAssertions,
                      historicalFires = historicalFires,
                      tests = c("SNLL_FS", "adtest"),
                      covMinMax = covMinMax,
                      Nreps = objfunFireReps,
                      maxFireSpread = maxFireSpread,
                      weighted = weighted,
                      verbose = TRUE, plot.it = FALSE)
      )
    )

    valsdt <- data.table(thresholds = thresholds, objFun = a)
    valsdt <- valsdt[objFun < 1e5]
    threshToUse <- min(valsdt$thresholds)
    message("  using SNLL_FS_thresh value: ", threshToUse)
    return(threshToUse)
  }

}
