runSpreadWithoutDEoptim <- function(iterThresh, lower, upper, fireSense_spreadFormula, flammableRTM,
                                    annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                                    mutuallyExclusive = list("youngAge" = "vegPC"),
                                    doObjFunAssertions = getOption("fireSenseUtils.assertions", TRUE),
                                    historicalFires, covMinMax, objfunFireReps, maxFireSpread,
                                    weighted = TRUE, tests = c("snll_fs", "adtest"),
                                    formulaToFit,
                                    pars = NULL, plot.it = TRUE, mode = "fit") {
  seed <- sample(1e6, 1)
  set.seed(seed)

  n <- iterThresh ## the more you do, the lower the resulting threshold

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

    thresholds <- sample(4 * max(n, decentEstimateThreshold), size = n)
  } else {
    userPars <- TRUE
    thresholds <- 1e8
  }
  if (!is(pars, "list")) pars <- list(pars)


  if ("debug" %in% mode) {
    a <- list()
    for (i in seq(pars)) {
      print(paste(i, "logit params:", paste(round(pars[[i]], 2), collapse = ", ")))
      a[[i]] <- .objfunSpreadFit(par = pars[[i]],
                                 thresh = thresholds[i],
                                 FS_formula = fireSense_spreadFormula, #loci = loci,
                                 landscape = flammableRTM,
                                 annualDTx1000 = annualDTx1000,
                                 nonAnnualDTx1000 = nonAnnualDTx1000,
                                 fireBufferedListDT = fireBufferedListDT,
                                 mutuallyExclusive = mutuallyExclusive,
                                 doAssertions = doObjFunAssertions,
                                 historicalFires = historicalFires,
                                 formulaToFit = formulaToFit,
                                 tests = tests,
                                 covMinMax = covMinMax,
                                 Nreps = objfunFireReps,
                                 maxFireSpread = maxFireSpread,
                                 verbose = TRUE,
                                 weighted = weighted,
                                 plot.it = plot.it
      )
    }
  } else {
    message("SNLL_FS_thresh not specified. Self calibrating threshold value for runDEoptim (n=", n, ")")

    # Check for being in a future
    a <- future::plan()
    if (is(a, "FutureStrategy") && !is(a, "sequential")) {
      coresToUse <- nbrOfWorkers()
    } else {
      activeThreads <- clusters::numActiveThreads()
      detCores <- detectCores()
      # Each fork is a copy-on-write image of this process that diverges as the
      # garbage collector marks the heap, so budget one heap per fork; the gc()
      # call also leaves less garbage to copy. See thresholdForks().
      heapMB <- sum(gc()[, 2])
      availMB <- pemisc::availableMemory()
      availMB <- if (length(availMB)) availMB / 1e6 else NULL
      coresToUse <- thresholdForks(heapMB = heapMB, availMB = availMB, nPars = length(pars),
                                   detCores = detCores, activeThreads = activeThreads,
                                   mcCores = getOption("mc.cores"))
      # future::plan("multicore", workers = coresToUse)
      # on.exit(future::plan("sequential"))
      withr::local_options("mc.cores" = coresToUse)
      # nCores <- length(pars) / (ceiling(length(pars) / parallel::detectCores())) # this will limit it to
      # nCores <- ceiling(parallel::detectCores() / ceiling(parallel::detectCores() / pemisc::optimalClusterNum(10000)))
    }
    message("Using ", coresToUse, " cores",
            if (exists("heapMB", inherits = FALSE))
              paste0(" (R heap ", round(heapMB / 1024), " GB; host has ",
                     if (length(availMB)) paste(round(availMB / 1024), "GB available") else "unknown memory", ")"),
            ".")

    st1 <- system.time({
      objSpreadFit <- mcmapply(mc.cores = coresToUse,
                    mc.preschedule = FALSE,
      # a <- future.apply::future_mapply(future.scheduling = Inf, future.seed = TRUE, # mc.cores = min(c(nCores, length(pars), getOption("mc.cores"))),
                    par = pars, FUN = .objfunSpreadFit,
                    thresh = thresholds,
                    MoreArgs = list(
                      FS_formula = fireSense_spreadFormula, #loci = loci,
                      landscape = flammableRTM,
                      annualDTx1000 = annualDTx1000,
                      nonAnnualDTx1000 = nonAnnualDTx1000,
                      fireBufferedListDT = fireBufferedListDT,
                      mutuallyExclusive = mutuallyExclusive,
                      doAssertions = doObjFunAssertions,
                      historicalFires = historicalFires,
                      formulaToFit = formulaToFit,
                      tests = tests,
                      covMinMax = covMinMax,
                      Nreps = objfunFireReps,
                      maxFireSpread = maxFireSpread,
                      weighted = weighted,
                      verbose = TRUE, plot.it = FALSE)
      )
    })

    valsdt <- data.table(thresholds = thresholds, objFun = objSpreadFit)
    valsdt <- valsdt[objFun < 1e5]
    threshToUse <- min(valsdt$thresholds)
    message("  using SNLL_FS_thresh value: ", threshToUse)
    return(threshToUse)
  }
}
