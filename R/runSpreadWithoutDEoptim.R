#' Evaluate the spread objective function without DEoptim
#'
#' In "debug" mode, runs `fireSenseUtils::.objfunSpreadFit()` once per parameter set, with plots.
#' Otherwise calibrates the SNLL threshold: evaluates `iterThresh` random parameter sets, each with a
#' random candidate threshold, in forked processes, and returns `pickThreshold()` of the results.
#'
#' @param iterThresh integer; number of random parameter sets (and candidate thresholds).
#' @param lower,upper numeric; bounds the random parameter sets are drawn between.
#' @param fireSense_spreadFormula character; passed to `FS_formula`.
#' @param flammableRTM `SpatRaster`; passed to `landscape`.
#' @param annualDTx1000,nonAnnualDTx1000,fireBufferedListDT,historicalFires elements of the list made
#'   by `fireSenseUtils::covsX1000AndSetDF()`.
#' @param mutuallyExclusive named list of mutually exclusive covariates.
#' @param doObjFunAssertions logical; passed to `doAssertions`.
#' @param covMinMax `data.table` of covariate min and max, or NULL for no rescaling.
#' @param objfunFireReps integer; passed to `Nreps`, the replicates per fire.
#' @param maxFireSpread numeric; upper limit on mean spread probability.
#' @param weighted logical; weight the SNLL by log fire size. Also used in the rough threshold estimate.
#' @param tests character; objective function tests, e.g. "SNLL_FS", "adTest".
#' @param formulaToFit character; the spread formula.
#' @param pars optional numeric vector, or list of them, to evaluate instead of random sets.
#' @param plot.it passed to `.objfunSpreadFit()` in "debug" mode.
#' @param mode character; if it includes "debug", the debug branch runs.
#' @param seed integer or NULL; `set.seed()` value. NULL draws one at random.
#' @return the calibrated threshold (numeric, or NA if every trial failed); NULL in "debug" mode.
runSpreadWithoutDEoptim <- function(iterThresh, lower, upper, fireSense_spreadFormula, flammableRTM,
                                    annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                                    mutuallyExclusive = list("youngAge" = "vegPC"),
                                    doObjFunAssertions = getOption("fireSenseUtils.assertions", TRUE),
                                    historicalFires, covMinMax, objfunFireReps, maxFireSpread,
                                    weighted = TRUE, tests = c("snll_fs", "adtest"),
                                    formulaToFit,
                                    pars = NULL, plot.it = TRUE, mode = "fit",
                                    seed = NULL) {
  ## The threshold this returns becomes `thresh` in runDEoptim(), so it is part of every cached
  ## DEoptim generation's key. With a seed drawn here, a single cache miss on the estimateThreshold
  ## event re-drew the threshold and invalidated EVERY cached generation for that ELF: on 2026-09-16
  ## a restarted fit went 1236 -> 1416 and recomputed from generation 1, losing ~17 h, while one that
  ## hit the cache kept 1705 and replayed 797 generations in ~35 min. A caller that passes a seed
  ## derived from the ELF gets a reproducible threshold, so a miss costs only this estimate.
  ## NULL keeps the old behaviour for callers that do not care (e.g. the module's `debug` event).
  if (is.null(seed)) seed <- sample(1e6, 1)
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
    ## do NOT re-draw here: that discarded the seed set above, which is what made the threshold
    ## irreproducible even when the caller asked for a specific seed
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
                                 FS_formula = fireSense_spreadFormula,
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
      withr::local_options("mc.cores" = coresToUse)
    }
    message("Using ", coresToUse, " cores",
            if (exists("heapMB", inherits = FALSE))
              paste0(" (R heap ", round(heapMB / 1024), " GB; host has ",
                     if (length(availMB)) paste(round(availMB / 1024), "GB available") else "unknown memory", ")"),
            ".")

    st1 <- system.time({
      objSpreadFit <- mcmapply(mc.cores = coresToUse,
                    mc.preschedule = FALSE,
                    par = pars, FUN = .objfunSpreadFit,
                    thresh = thresholds,
                    MoreArgs = list(
                      FS_formula = fireSense_spreadFormula,
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

    threshToUse <- pickThreshold(thresholds = thresholds, objFun = objSpreadFit)
    message("  using SNLL_FS_thresh value: ", threshToUse)
    return(threshToUse)
  }
}
