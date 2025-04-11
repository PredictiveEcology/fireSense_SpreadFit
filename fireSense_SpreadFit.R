defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = paste("Fit statistical models that can be used to parameterize the",
                      "fire spread component of simulation models (e.g. fireSense).",
                      "This module implement a Pattern Oriented Modelling (POM)",
                      "approach to derive spread probabilities from final fire sizes.",
                      "Spread probabilities can vary between pixels, and thus reflect",
                      "local heterogeneity in environmental conditions."),
  keywords = c("fire", "spread", "POM", "percolation"),
  authors = c(
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut")),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = c("aut")),
    person("Alex M.", "Chubaty", email = "achubaty@for-cast.ca", role = c("ctb"))
  ),
  childModules = character(),
  version = list(fireSense_SpreadFit = "1.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  loadOrder = list(after = c("fireSense_dataPrepFit", "fireSense_ignitionFit")),
  reqdPkgs = list("data.table", "DEoptim", "fastdigest", "fpCompare", "future", "ggplot2", "kSamples",
                  "logging", "magrittr", "parallel", "raster", "terra", "tidyr", ## TODO: remove magrittr
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/clusters@main",
                  "PredictiveEcology/Require@development (>= 0.3.1)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9077)",
                  "PredictiveEcology/SpaDES.tools@development (>= 2.0.4.9002)"),
  parameters = rbind(
    defineParameter(".plots", "character|logical", default = NULL, ## TODO: use .plotInitialTime etc.
                    desc = "Should outputs be plotted?"),
    defineParameter(".plotSize", "list", default = list(height = 1600, width = 2000),
                    desc = paste("List specifying height and width of plotting device (in pixels)",
                                 "used to plot DEoptim histograms when `visualizeDEoptim` is TRUE.")),
    defineParameter(".runInitialTime", "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start time of the simulation."),
    defineParameter(".runInterval", "numeric", default = NA,
                    desc = paste("optional. Interval between two runs of this module,",
                                 "expressed in units of simulation time. By default, NA, which",
                                 "means that this module only runs once per simulation.")),
    defineParameter(".saveInitialTime", "numeric", default = NA,
                    desc = "optional. When to start saving output to a file."),
    defineParameter(".saveInterval", "numeric", default = NA,
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", "init", NA, NA,
                    desc = paste("Should this entire module be run",
                                 "with caching activated? This is generally intended for data-type",
                                 "modules, where stochasticity and time are not relevant.")),
    defineParameter("cacheId_DE", "character", default = NULL,
                    desc = paste("An optional character string representing a `cacheId` to recover from the Cache. ",
                                 "After `reproducible >= 2.0.10.9016`, this can be set to 'previous', meaning the Cache ",
                                 "will get the previous item in the Cache that matches the `P(sim)$rep` (see that param)")),
    defineParameter("cloudFolderID_DE", "character", default = NULL,
                    desc = "Passed to `cloudFolderID` in the `Cache(DEoptim...)` call."),
    defineParameter("cores", "integer", default = 1L,
                    desc = paste("non-negative integer.",
                                 "Defines the number of logical cores to be used for parallel computation.",
                                 "The default value is 1, which disables parallel computing.")),
    defineParameter("DEoptimTests", "character", default = "SNLL_FS",
                    desc = paste("Currently either `'SNLL_FS'` or `'adTest'` or a length 2 character vector of both.",
                                 "These are passed to `.objFunSpreadFit`")),
    defineParameter("doObjFunAssertions", "logical", default = TRUE,
                    desc = "This is passed to `objFunSpreadProb`; TRUE will do some diagnostics but is slower; FALSE for operational runs"),
    defineParameter("initialpop", "numeric", default = NULL,
                    desc = paste("A numeric matrix of dimensions `NCOL = length(lower)`",
                                 "and `NROW = NP`. This will be passed into DEoptim",
                                 "through `control$initialpop = P(sim)$initialpop` if it is",
                                 "not NULL")),
    defineParameter("iterDEoptim", "integer", default = 500L,
                    desc = paste("integer defining the maximum number of iterations allowed (DEoptim optimizer).")),
    defineParameter("iterStep", "integer", default = 25L,
                    desc = "Passed to runDEoptim"),
    defineParameter("iterThresh", "integer", default = 96L,
                    desc = "Number of iterations for automated threshold calibration."),
    defineParameter("libPathDEoptim", "character", default = .libPaths()[1],
                    desc = paste("Absolute path specifying R package directory location to use when running DEotpim.",
                                 "NOTE: this path must be read/write accessible on ALL machines",
                                 "used for fitting (identified in cores).",
                                 "Therefore, it's best use a directory in your user's `~` directory.",
                                 "If the directory does not exist at this path, will attempt to create it.")),
    defineParameter("lower", "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Lower limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (named in the order they",
                                 "appear in the formula).")),
    defineParameter("maxFireSpread", "numeric", default = 0.28,
                    desc = paste0("optional. Maximum fire spread average to be passed to the `.objFun`.",
                                  "This puts an upper limit on `spreadProb` during optimization.")),
    defineParameter("mode", "character", default = "fit",
                    desc = paste("Options: debug, fit, visualize. Can use multiples. 'debug' will trigger running of",
                                 "the objective function with visuals; 'fit' will trigger DEoptim; 'visualize' will trigger",
                                 "visualization after DEoptim. For 'visualize', DE object must be findable, either in sim,",
                                 "on disk or a cloud URL. These last 2 can be specified with `urlDEOptimObject` param.")),
    defineParameter("mutuallyExclusiveCols", "list", list("youngAge" = c("class", "nonForest")), NA, NA,
                    desc = "a named list of mutually exclusive covariates - see `fireSenseUtils::makeMutuallyExclusive`"),
    defineParameter("NP", "integer", default = NULL,
                    desc = "Number of Populations. See `?DEoptim.control`."),
    defineParameter("objFunCoresInternal", "integer", default = 1L,
                    desc = paste("Integer defining the number of cores to pass to `mcmapply(mc.cores = ...)`",
                                 "This will fork this many to do the years loop internally.",
                                 "This would be in addition to `cores` and is effecively a multiplier.",
                                 "The computer needs to have `cores * objFunCoresInternal` threads or it will stall.")),
    defineParameter("objfunFireReps", "integer", default = 100L,
                    desc = paste("integer defining the number of replicates the objective function",
                                 "will attempt each fire. Since the default approach is",
                                 "using `EnvStats::demp`, it should be at least 100 to get a",
                                 "smooth distribution for a likelihood.")),
    defineParameter("onlyLoadDEOptim", "logical", default = FALSE,
                    desc = paste0("optional. If TRUE, the module will skip the fitting altogether ",
                                  "and will only load the latest uploaded version of the `DEOptim` object")),
    defineParameter("rep", "integer", 1L, NA, NA,
                    desc = paste("An optional integer indicating which replicate run this represents. ",
                                 "This is used to identify unique runs of `runDEoptim`, from a Cache perspective. ",
                                 "For example, if this module is run twice with all the same data, ",
                                 "Cache will think that the second run ",
                                 "should recover the cache result, unless this `rep` is modified")),
    defineParameter(".c", "numeric", 0.5, NA, NA,
                    desc = "the `c` argument passed to DEoptim.control"),
    defineParameter("rescaleAll", "logical", TRUE, NA, NA,
                    desc = "rescale covariates for `DEOptim`"),
    defineParameter("strategy", "integer", default = 3L,
                    desc = "Passed to `DEoptim.control`"),
    defineParameter("SNLL_FS_thresh", "integer", default = NULL,
                    desc = "Threshold multiplier used in objective function SNLL fire size test."),
    defineParameter("trace", "numeric", default = 1L,
                    desc = paste("non-negative integer. If > 0, tracing information on",
                                 "the progress of the optimization are printed every",
                                 "`trace` iteration. Default is 1, i.e. every iteration. ",
                                 "Setting to 0 turns off tracing.")),
    defineParameter("upper", "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Upper limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (named in the order they",
                                 "appear in the formula).")),
    defineParameter("urlDEOptimObject", "character",
                    default = paste0("https://drive.google.com/file/d/",
                                     "1GYsEbiE60m7cmP2Hfe0WCG_ng9o-RPP9/view?usp=sharing"),
                    desc = paste0("optional. If `onlyLoadDEOptim == TRUE`, you can pass the url to the  ",
                                  "`DEOptim` object. The default is the object from the run on 11JUN20",
                                  " from the `logistic2p`")),
    defineParameter("useCache_DE", "logical", default = TRUE,
                    desc = "should `DEoptim` use `Cache`? to do multiple independent runs, use FALSE"),
    defineParameter("useCloud_DE", "logical", default = FALSE,
                    desc = "Passed to `useCloud` in the `Cache(DEoptim...)` call"),
    defineParameter("verbose", "logical", default = FALSE,
                    desc = paste0("optional. Should it calculate and print median of spread ",
                                  "Probability during calculations?")),
    defineParameter("visualizeDEoptim", "Path", default = figurePath(sim),
                    desc = paste("Passed to runDEoptim. This makes histographs at each iterStep and saves them ",
                    "to this path")),
    defineParameter("upperAndLowerVal", "numeric", default = 6,
                    desc = "This will be given to the upper and -lower values if not supplied by user")
  ),
  inputObjects = rbind(
    expectsInput("fireBufferedListDT", "list",
                 desc = "list of data.tables with fire id, pixelID, and buffer status"),
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    expectsInput("fireSense_annualSpreadFitCovariates", "data.table",
                 desc = "table of climate and/or veg covariates, burn status, polyID, and pixelID"),
    expectsInput("fireSense_nonAnnualSpreadFitCovariates", "data.table",
                 desc = "table of veg covariates, burn status, polyID, and pixelID"),
    expectsInput("fireSense_spreadFormula", "character",
                 desc = paste0("a formula that contains the annual and non-annual covariates",
                               "e.g. `~ 0 + MDC + class2 + class3 + youngAge`.")),
    expectsInput("parsKnown", "numeric",
                 desc = paste0("Optional vector of known parameters, e.g., from a previous `DEoptim` run.",
                               "If this is supplied, then 'mode' will be automatically converted to 'debug'")),
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "template raster for study area"),
    expectsInput("spreadFirePoints", "sf",
                 desc = "list of spatial points objects representing annual fire centroids"),
    expectsInput("studyArea", "sf",
                 desc = "Study area for the prediction. Defaults to NWT.",
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU")
  ),
  outputObjects = rbind(
    createsOutput("covMinMax_spread", "data.table",
                  desc = "`data.table` of covariates min and max"),
    createsOutput("DE", "data.table", desc = "`DEOptim` object"),
    createsOutput("fireSense_SpreadFitted", "fireSense_SpreadFit",
                  desc = "DEFUNCT -- A fitted model object of class fireSense_SpreadFit."),
    createsOutput("studyAreaWithSpreadParams", "sf",
                  desc = paste("This is the studyArea, but with 10 duplicated features, each",
                               "with its own set of parameters from the 10 best DEoptim runs")),
    createsOutput("fsSpreadFit_hists", "ggplot",
                  desc = "histograms of each parameter used in `DEoptim` fitting."),
    createsOutput("lociList", "list", desc = "list of fire locs")
  )
))

doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- current(sim)$moduleName
  switch(
    eventType,
    init = {
      moduleName <- currentModule(sim)
      if (!is.null(Par$debugMode)) if (Par$debugMode)
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))

      # If user supplies known DEOptim outputs as simple coefficients using parsKnown...
      if (!is.null(sim$parsKnown)) {
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))
      }

      sim <- Init(sim)

      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "spreadFitPrepare")
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "estimateThreshold")

      if ("debug" %in% P(sim)$mode) {
         sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
      } else {
        if ("fit" %in% P(sim)$mode) {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "run")
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "makefireSense_SpreadFitted")
        } else {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "retrieveDEOptim")
        }

        if ("visualize" %in% P(sim)$mode) {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "plot")
        }
      }
    },
    spreadFitPrepare = {
      sim <- spreadFitPrep(sim)
    },
    debug = {
      ## This below is to test the code without running DEOptim
      thresh <- runSpreadWithoutDEoptim(
        iterThresh = P(sim)$iterThresh, P(sim)$lower, P(sim)$upper,
        sim$fireSense_spreadFormula, sim$rasterToMatch,
        mod$dat$annualDTx1000, mod$dat$nonAnnualDTx1000, mod$dat$fireBufferedListDT,
        mutuallyExclusive = P(sim)$mutuallyExclusiveCols,
        doObjFunAssertions = P(sim)$doObjFunAssertions,
        mod$dat$historicalFires, sim$covMinMax_spread, P(sim)$objfunFireReps,
        P(sim)$maxFireSpread, pars = sim$parsKnown, plot.it = P(sim)$.plots,
        tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
        mode = Par$mode)
    },
    estimateThreshold = {
      # Estimate threshold for .objFunSpreadFit
      sim <- estimateSNLLThresholdPostLargeFires(sim)
    },
    run = {
      termsInDEoptim(sim$fireSense_spreadFormula, mod$thresh, length(P(sim)$lower))
      # termsInForm <- attr(terms(as.formula(sim$fireSense_spreadFormula, env = .GlobalEnv)), "term.labels")
      # logitNumParams <- length(P(sim)$lower) - length(termsInForm)
      # message("Using a ", logitNumParams, " parameter logistic equation")
      # message("  There will be ", length(P(sim)$lower), " terms: ")
      # message("  ", paste(c(paste0("logit", seq(logitNumParams)), termsInForm), collapse = ", "))
      # message("  objectiveFunction threshold SNLL to run all years after first 2 years: ", mod$thresh)

      useCache <- (isFALSE(getOption("fireSenseUtils.runTests")))
      # if (isRstudioServer() || any(grepl("positron", search()))) {
      #   a <- Par$cores# <- NULL
      #   Par$cores <- NULL
      # }
      if (!is.null(Par$cores) && !any(is.na(Par$cores)) && identical(sort(unique(Par$cores)), sort(Par$cores))) {
        # if (length(unique(Par$cores)) > 1) {
        #   message("Running tests on cluster to determine current speed...")
        #   best <- clusters::runTests(unique(Par$cores), repos = c("predictiveecology.r-universe.dev", getOption("repos")),
        #                              clustersBranch = "main") |> Cache(useCache = useCache)
        #   message("The following is the current speed of the cluster")
        #   messageDF(best$wholeCluster)
        #   message("")
        #   message("Using only: ")
        # } else {
          best <- list(cluster = Par$cores)
        # }
      } else {
        best <- list(cluster = Par$cores,
                     bestCluster = data.table(host = unique(Par$cores),
                                              cores = as.numeric(table(Par$cores))))
      }
      # if (isRstudioServer() || any(grepl("positron", search()))) {
      #   Par$cores <- a
      # }
      messageDF(best$bestCluster)

      fnName <- paste0("runDEoptim_", P(sim)$rep)
      sim$DE <- Cache(runDEoptim(landscape = sim$rasterToMatch,
                                 annualDTx1000 = mod$dat$annualDTx1000,
                                 nonAnnualDTx1000 = mod$dat$nonAnnualDTx1000,
                                 fireBufferedListDT = mod$dat$fireBufferedListDT,
                                 historicalFires = mod$dat$historicalFires,
                                 itermax = P(sim)$iterDEoptim,
                                 iterStep = P(sim)$iterStep,
                                 trace = P(sim)$trace,
                                 initialpop = P(sim)$initialpop,
                                 strategy = P(sim)$strategy,
                                 cores = best$cluster,
                                 doObjFunAssertions = P(sim)$doObjFunAssertions,
                                 libPath = normPath(P(sim)$libPathDEoptim),
                                 logPath = logPath(sim), ## TODO (#6): use tempdir()
                                 cachePath = cachePath(sim),
                                 lower = P(sim)$lower,
                                 upper = P(sim)$upper,
                                 mutuallyExclusive = P(sim)$mutuallyExclusiveCols, ## TODO: test
                                 FS_formula = sim$fireSense_spreadFormula,
                                 covMinMax = sim$covMinMax_spread,
                                 objFunCoresInternal = P(sim)$objFunCoresInternal,
                                 tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
                                 maxFireSpread = P(sim)$maxFireSpread,
                                 Nreps = P(sim)$objfunFireReps,
                                 thresh = mod$thresh,
                                 .c = P(sim)$.c,
                                 .verbose = P(sim)$verbose,
                                 visualizeDEoptim = P(sim)$visualizeDEoptim,
                                 .plotSize = P(sim)$.plotSize,
                                 .plots = P(sim)$.plots,
                                 rep = P(sim)$rep),
                      cacheId = P(sim)$cacheId_DE,
                      .functionName = fnName,
                      .cacheExtra = fnName,
                      useCache = P(sim)$useCache_DE,
                      useCloud = P(sim)$useCloud_DE,
                      cloudFolderID = P(sim)$cloudFolderID_DE ## Cloud cache was being a problem
      )

    },
    retrieveDEOptim = {
      browser()
      if (!is.null(Par$urlDEOptimObject))
        message("Loading ", Par$urlDEOptimObject)
      out <- Cache(loadPrevDEOptimRun, url = Par$urlDEOptimObject,
                   destinationPath = Paths$outputPath,
                   wholeSim = TRUE,
                   userTags = "What:retrieveDEOptim")
      if (is(out, "simList")) {
        sim$originalSim <- out
        sim$DE <- sim$originalSim$DE
        sim$fireSense_SpreadFitted <- sim$originalSim$fireSense_SpreadFitted
        sim$parsKnown <- sim$fireSense_SpreadFitted$meanCoef
        rm(list = "originalSim", envir = envir(sim))
      } else {
        sim$fireSense_SpreadFitted <- out
      }
    },
    makefireSense_SpreadFitted = {
      objFunValsAll <- unlist(lapply(sim$DE, function(x) x$optim$bestval))
      ordered <- order(objFunValsAll)
      outs <- rbindlist(lapply(sim$DE, function(x) data.frame(t(x$optim$bestmem))))
      set(outs, NULL, "objFunVal", objFunValsAll)
      set(outs, NULL, "iters",seq_len(length(sim$DE)))
      outs <- outs[ordered, ]

      Nkeep <- 10
      sim$studyAreaWithSpreadParams <- sim$studyArea
      for (i in 2:Nkeep) {
        sim$studyAreaWithSpreadParams <- rbind(sim$studyAreaWithSpreadParams, sim$studyArea)
      }
      sim$studyAreaWithSpreadParams[, names(outs)] <- outs[seq_len(Nkeep),]

    },
    plot = {
      browser()
      DEpop_df <- as.data.frame(sim$DE[[1]]$member$pop)
      colnames(DEpop_df) <- names(sim$fireSense_SpreadFitted$bestCoef)
      sim$fsSpreadFit_hists <- ggplot(tidyr::gather(DEpop_df), aes(value)) +
        geom_histogram(bins = 20) +
        facet_wrap(~key, scales = "free_x") +
        ggtitle(paste("distributions of SpreadFit coefficients for", basename(outputPath(sim))))

      checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
      ggsave(file.path(outputPath(sim), "figures", "spreadFit_coeffs.png"), sim$fsSpreadFit_hists)

      sim$fsSpreadFit_hists ## show plot in session
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                    "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  invisible(sim)
}

Init <- function(sim){
  ## TODO: does this module need an init?
  return(sim)
}

spreadFitPrep <- function(sim) {
  moduleName <- current(sim)$moduleName

  # Mutually Exclusive Columns -- basically no class with biomass or land cover can also be in the
  #   youngAge class. Inside the optimization function, the covariates are set to zero if
  #   youngAge is 1
  mec <- "mutuallyExclusiveCols"
  defaults <- depends(sim)@dependencies$fireSense_SpreadFit@parameters
  defaultMutuallyExclusive <- defaults[defaults$paramName %in% mec, "default"][[1]]
  if (identical(Par[[mec]], defaultMutuallyExclusive)) {
    sp_lcc <- colnames(sim$fireSense_nonAnnualSpreadFitCovariates[[1]])
    sp_lcc <- grep("pixel", sp_lcc, invert = TRUE, value = TRUE)
    P(sim)[[mec]] <- Map(l = Par[[mec]], nam = names(Par[[mec]]), function(l, nam) {
      if (identical(nam, "youngAge"))
        c(l, sp_lcc)
      else
        l
    })
    message("Mutually exclusive is now:")
    message(Par[[mec]])

  }

  # veg coefficients should probably have bounds of 4
  # however youngAge should have an upper limit of zero to prevent self-propagating fires
  # MDC should have a lower limit of zero - drought shouldn't increase spread probability
  if (is.null(P(sim)$upper) | is.na(P(sim)$upper)) {
    P(sim)$upper <- estimateSpreadParams(sim$fireSense_spreadFormula,
                                         sim$fireSense_annualSpreadFitCovariates,
                                         whichBound = "upper", upperAndLower = Par$upperAndLowerVal)
  }

  if (is.null(P(sim)$lower) | is.na(P(sim)$lower)) {
    ## TODO - figure out the 2-4 piece logistic defaults :S
    P(sim)$lower <-  estimateSpreadParams(sim$fireSense_spreadFormula,
                                          sim$fireSense_annualSpreadFitCovariates,
                                          whichBound = "lower", upperAndLower = Par$upperAndLowerVal)
  }
  ## sanity check parameters + inputs
  #cores can be NA for interactive debugging
  stopifnot(
    "parameter 'trace' must be postive" = P(sim)$trace >= 0,
    "parameter 'cores' must be a postive integer" = length(P(sim)$cores) >= 0 || isTRUE(is.na(P(sim)$cores)),
    "each non-annual spreadFit covariate cannot be all zeros" =
      all(sapply(rbindlist(sim$fireSense_nonAnnualSpreadFitCovariates), max) > 0)
  )

  if (!all(names(P(sim)$upper) == names(P(sim)$lower))) {
    stop("please ensure 'upper' and 'lower' params are named with an identical order")
  }

  if (P(sim)$rescaleAll) {
    sim$covMinMax_spread <- deriveCovMinMax(
      annualList = sim$fireSense_annualSpreadFitCovariates,
      nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates
    )
    if (any(is.na(sim$covMinMax_spread))) {
      stop("covMinMax_spread contains NA values. Check upstream for introduction of NAs.")
    }
  }

  if (anyPlotting(Par$.plots) && "debug" %in% P(sim)$mode) {
    try(histOfCovariates(annualList = sim$fireSense_annualSpreadFitCovariates,
                         nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates))
  }

  sim$lociList <- makeLociList(ras = sim$rasterToMatch, pts = sim$spreadFirePoints)

  mod$dat <- covsX1000AndSetDF(
    annualList = sim$fireSense_annualSpreadFitCovariates,
    nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates,
    fireBufferedList = sim$fireBufferedListDT,
    fireLociList = sim$lociList,
    paramOrder = P(sim)$upper)

  return(sim)
}

toX1000 <- function(lst, omitCols = "pixelID") {
  annualDTx1000 <- lapply(lst, function(dt) {
    setDT(dt)
    cns <- setdiff(colnames(dt), omitCols)
    for (colnam in cns)
      set(dt, NULL, colnam, fireSenseUtils:::asInteger(dt[[colnam]] * 1000))
    setDF(dt)
  })
}

loadPrevDEOptimRun <- function(url, destinationPath, wholeSim = TRUE) {
  # Check to see if it is a local file first
  if (file.exists(url)) {
    sim2 <- try(Cache(readRDS, url))
    if (is(sim2, "try-error")) {
      sim2 <- try(Cache(qs::qread, url))
    }
  } else {
    sim2 <- try(Cache(prepInputs, url = url,
                      destinationPath = destinationPath,
                      fun = "qs::qread"))
    if (is(sim2, "try-error"))
      sim2 <- try(Cache(prepInputs, url = url,
                        destinationPath = destinationPath,
                        fun = "base::readRDS"))
  }
  sim2
}

deriveCovMinMax <- function(annualList, nonAnnualList) {

  nonAnnRescales <- rbindlist(nonAnnualList)
  vals1 <- setdiff(colnames(nonAnnRescales), "pixelID")

  #Biomass columns should be normalized together
  #else 140 Mg/ha pine is treated the same as e.g. 80 Mg/ha White spruce
  # assuming those were the fuel classes and respective maximum observed biomass
  minMax <- nonAnnRescales[, lapply(.SD, range), .SDcols = vals1]
  names(minMax) <- vals1
  biomassCols <- names(minMax)[minMax[2, ] %>>% 1]
  sharedRange <- range(minMax[, .SD, .SDcols = biomassCols])

  biomassMax <- minMax[, lapply(.SD,FUN = function(x){return(sharedRange)}), .SDcols = biomassCols]

  #override the min and max
  coverCols <- setdiff(vals1, biomassCols)
  covMinMax1 <- cbind(biomassMax, minMax[, .SD, .SDcols = coverCols])
  #just in case covMinMax must respect original order
  setcolorder(covMinMax1, vals1)

  #annual covariates (climate/youngAge)
  annRescales <- rbindlist(annualList)
  vals2 <- setdiff(colnames(annRescales), c("buffer", "pixelID", "ids"))
  covMinMax2 <- annRescales[, lapply(.SD, range), .SDcols = vals2]
  covMinMax <- cbind(covMinMax1, covMinMax2)
  covMinMax
}

histOfCovariates <- function(annualList, nonAnnualList) {
  annualCols <- colnames(annualList[[1]])
  annualColsToPlot <- setdiff(annualCols, "pixelID")
  nonAnnualCols <- colnames(nonAnnualList[[1]])
  nonAnnualColsToPlot <- setdiff(nonAnnualCols, "pixelID")

  nplots <- length(annualColsToPlot) * length(annualList) +
    length(nonAnnualColsToPlot) * length(nonAnnualList)
  ncols <- ceiling(sqrt(nplots))
  nrows <- ceiling(nplots/ncols)
  par(mfrow = c(ncols, nrows))
  ann <- rbindlist(annualList, idcol = "year")
  set(ann, NULL, "pixelID", NULL)
  out <- ann[, Map(dt = .SD, colname = names(.SD), function(dt, colname)
    hist(dt, main = paste(.BY, " ", colname), xlab = "")), by = "year"]
  nonAnn <- rbindlist(nonAnnualList, idcol = "year")
  set(nonAnn, NULL, "pixelID", NULL)
  out <- nonAnn[, Map(dt = .SD, colname = names(.SD), function(dt, colname)
    hist(dt, main = paste(.BY, " ", colname), xlab = "")), by = "year"]
}

covsX1000AndSetDF <- function(annualList, nonAnnualList, fireBufferedList, fireLociList, paramOrder) {

  annualCols <- colnames(annualList[[1]])
  nonAnnualCols <- colnames(nonAnnualList[[1]])
  annualCols <- annualCols[annualCols %in% names(paramOrder)]
  nonAnnualCols <- nonAnnualCols[nonAnnualCols %in% names(paramOrder)]

  annualList <- lapply(annualList, setcolorder, neworder = c("pixelID", annualCols))
  nonAnnualCols <- lapply(nonAnnualList, setcolorder, neworder = c("pixelID", nonAnnualCols))

  annualDT <- lapply(annualList, setDF)
  annualDTx1000 <- toX1000(annualDT)
  nonAnnualDT <- lapply(nonAnnualList, setDF)
  nonAnnualDTx1000 <- toX1000(nonAnnualDT)
  fireBufferedListDT <- lapply(fireBufferedList, setDF)
  historicalFires <- lapply(fireLociList, setDF)
  list(annualDTx1000 = annualDTx1000,
       nonAnnualDTx1000 = nonAnnualDTx1000,
       fireBufferedListDT = fireBufferedListDT,
       historicalFires = historicalFires)
}

estimateSNLLThresholdPostLargeFires <- function(sim) {
  thresh <- if (is.null(Par$SNLL_FS_thresh) || is.na(Par$SNLL_FS_thresh)) {
    message("Estimating threshold for inside .objFunSpreadFit -- This can be supplied via SNLL_FS_thresh parameter")

    # Took 50 minutes using 10 cores for Taiga studyArea
    Cache(runSpreadWithoutDEoptim(
          iterThres = P(sim)$iterThresh,
          lower = P(sim)$lower, upper = P(sim)$upper,
          fireSense_spreadFormula = sim$fireSense_spreadFormula,
          flammableRTM = sim$rasterToMatch,
          mutuallyExclusive =  P(sim)$mutuallyExclusiveCols,
          doObjFunAssertions = P(sim)$doObjFunAssertions,
          annualDTx1000 = mod$dat$annualDTx1000,
          nonAnnualDTx1000 = mod$dat$nonAnnualDTx1000,
          fireBufferedListDT = mod$dat$fireBufferedListDT,
          historicalFires = mod$dat$historicalFires,
          covMinMax = sim$covMinMax_spread,
          objfunFireReps = P(sim)$objfunFireReps,
          tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
          mode = Par$mode,
          maxFireSpread = P(sim)$maxFireSpread),
          omitArgs = c("objfunFireReps", "mode")
    )
  } else {
    P(sim)$SNLL_FS_thresh
  }
  mod$thresh <- thresh
  return(sim)
}

asFireSense_SpreadFitted <- function(DE, DEformulaChar, lower) {
  browser()
  DE2 <- if (is(DE, "list")) {
    DE2 <- tail(DE, 1)[[1]]
  } else {
    DE
  }

  # DE1 <- tail(DE, 1)[[1]]
    objFunValsAll <- unlist(lapply(DE, function(x) x$optim$bestval))
    ordered <- order(objFunValsAll)
    outs <- rbindlist(lapply(DE, function(x) data.frame(t(x$optim$bestmem))))
    set(outs, NULL, "objFunVal", objFunValsAll)
    set(outs, NULL, "iters",seq_len(length(DE)))
    outs <- outs[ordered, ]

    # head(outs[ordered,])

    # bestvals <- which.min(objFunValsAll)
    # DE1$optim$bestmem <- DE[[bestvals]]$optim$bestmem
    # DE1$optim$bestval <- DE[[bestvals]]$optim$bestval
    # DE1$optim$iter <- sum(unlist(lapply(DE, function(x) x$optim$iter)))
    # DE1$member$bestmemit <- as.matrix(rbindlist(lapply(DE, function(x) as.data.table(x$member$bestmemit))))
    # DE1$member$bestvalit <- rbindlist(lapply(DE, function(x) as.data.table(x$member$bestvalit)))[[1]]
  # DE1$member <- as.matrix(rbindlist(lapply(DE, function(x) as.data.table(x$member$bestmemit))))

  # options(opts)

  ## TODO: use native R pipe
  valAverage <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = median)
  valSD <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = sd)
  valBest <- DE2 %>% `[[`("optim") %>% `[[`("bestmem")
  bestFit <- DE2$optim$bestval
  terms <- terms(as.formula(DEformulaChar, env = .GlobalEnv))
  # Identifying the number of parameters of the logistic function and names
  nParsLogistic <- length(lower) - length(attributes(terms)[["term.labels"]])
  if (nParsLogistic == 5) {
    nms <- c("inflectionPoint1", "inflectionPoint2",
             "maxAsymptote", "hillSlope1", "hillSlope2")
  } else if (nParsLogistic == 4) {
    nms <- c("inflectionPoint1", "inflectionPoint2",
             "maxAsymptote", "hillSlope1")
  } else if (nParsLogistic == 3) {
    nms <- c("maxAsymptote", "hillSlope1", "inflectionPoint1")
  } else if (nParsLogistic == 2) {
    nms <- c("maxAsymptote", "hillSlope1")
  }
  # Giuseppe Cardillo (2020). Three parameters logistic regression -
  # There and back again (https://www.github.com/dnafinder/logistic3),
  # GitHub. Retrieved June 11, 2020.

  fireSense_SpreadFitted <- list(
    formula = DEformulaChar,
    bestCoef = setNames(valBest,
                        nm = c(nms,
                               if (attr(terms, "intercept") != 0) "Intercept" else NULL,
                               attr(terms, "term.labels")
                        )
    ),
    meanCoef = setNames(valAverage,
                        nm = c(nms,
                               if (attr(terms, "intercept") != 0) "Intercept" else NULL,
                               attr(terms, "term.labels")
                        )
    ),
    sdCoef = setNames(valSD,
                      nm = c(nms,
                             if (attr(terms, "intercept") != 0) "Intercept" else NULL,
                             attr(terms, "term.labels")
                      )
    ),
    bestFit = bestFit
  )

  class(fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  fireSense_SpreadFitted
}

estimateSpreadParams <- function(fireSense_spreadFormula, anyAnnualCovariates, whichBound,
                                 upperAndLower) {

  stopifnot(whichBound %in% c("upper", "lower"))

  formulaTerms <- attr(terms(as.formula(fireSense_spreadFormula, env = .GlobalEnv)), "term.labels")
  termLength <- length(formulaTerms)
  if (whichBound == "upper") {
    newParams <- rep(upperAndLower, times = termLength)
  } else {
    newParams <- rep(-(upperAndLower), termLength)
  }
  newParams <- as.vector(newParams)
  whAnnual <- formulaTerms %in% colnames(anyAnnualCovariates[[1]])
  whYA <- formulaTerms[whAnnual] %in% "youngAge"
  newParams[whAnnual] <- ifelse(whichBound == "upper", upperAndLower, 0)
  newParams[whAnnual][whYA] <- ifelse(whichBound == "upper", 0, -(upperAndLower))

  names(newParams) <- formulaTerms

  if (whichBound == "upper") {
    newParams <- c("maxAsymptote" = 0.276, "hillSlope1" = 2, "inflectionPoint1" = 4, newParams)
  } else {
    newParams <- c("maxAsymptote" = 0.25, "hillSlope1" = 0.2, "inflectionPoint1" = 0.1, newParams)
  }

  return(newParams)
}

.inputObjects <- function(sim) {
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dPath,
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }

  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"),
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif",
                               destinationPath = dPath,
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID",
                                            "useCloud", "overwrite", "filename2"))
  }

  if (!suppliedElsewhere("fireSense_spreadFormula", sim)) {
    stop("fireSense_spreadFormula must be supplied.")
  }

  return(invisible(sim))
}
