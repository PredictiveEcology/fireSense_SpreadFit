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
  version = list(fireSense_SpreadFit = "1.0.3"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  loadOrder = list(after = c("fireSense_dataPrepFit", "fireSense_ignitionFit")),
  reqdPkgs = list("data.table", "DEoptim", "fastdigest", "fpCompare", "future", "ggplot2", "kSamples",
                  "logging", "magrittr", "parallel", "raster", "terra", "tidyr", ## TODO: remove magrittr
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/clusters@main (>=0.0.19)",
                  "PredictiveEcology/Require@development (>= 0.3.1)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.6.9008)",
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
    defineParameter(".useCache", c("logical", "character"), "init", NA, NA,
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
    # This was KNN drive URL
    # defineParameter("spreadFitGoogleDriveFolder", "character", "https://drive.google.com/drive/u/0/folders/1spxq7CnL4kNcJoUQlRek2CmBJ1InAmbP",
    #                 NA, NA, "A Googledrive folder url where a file with fireSense studyArea exists as an 'sf' class object"),
    defineParameter("spreadFitGoogleDriveFolder", "character", "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf",
                    NA, NA, "A Googledrive folder url where a file with fireSense studyArea exists as an 'sf' class object"),
    defineParameter("spreadFitFilename", "character", "fireSenseParams.rds",
                    NA, NA, "A Googledrive folder url where a file with fireSense studyArea exists as an 'sf' class object"),
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
    defineParameter("verbose", "numeric", default = 1,
                    desc = paste0("optional. With increasing number, more verbosity. Level 1 is ",
                                  "normal reproducible (e.g., Cache), level 2 includes objective function ",
                                  "e.g., print median of spreadProb during calculations")),
    defineParameter("visualizeDEoptim", "Path", default = asPath(figurePath(sim)),
                    desc = paste("Passed to runDEoptim. This makes histographs at each iterStep and saves them ",
                                 "to this path")),
    defineParameter("upperAndLowerVal", "numeric", default = 9,
                    desc = "This will be given to the upper and -lower values if not supplied by user")
  ),
  inputObjects = rbind(
    expectsInput(".runName", "character", "Some descriptive, short name for this fitting, e.g., ELF14.1"),
    expectsInput("fireBufferedListDT", "list",
                 desc = "list of data.tables with fire id, pixelID, and buffer status"),
    # expectsInput("rasterToMatch", "SpatRaster",
    #              desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    expectsInput("fireSense_annualSpreadFitCovariates", "data.table",
                 desc = "table of climate and/or veg covariates, burn status, polyID, and pixelID"),
    expectsInput("fireSense_nonAnnualSpreadFitCovariates", "data.table",
                 desc = "table of veg covariates, burn status, polyID, and pixelID"),
    # expectsInput("fireSense_spreadLogisticTermNames", "character",
    #              desc = paste0("The term names for the logistic terms in the spread fit")),
    expectsInput("spreadFitAdditionalColNames", "character",
                 desc = paste0("The column names used to attach the spreadFit object and several ancilliary objects")),
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
      if (!is.null(Par$debugMode)) if (Par$debugMode)
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))

      # If user supplies known DEOptim outputs as simple coefficients using parsKnown...
      if (!is.null(sim$parsKnown)) {
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))
      }
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "spreadFitPrepare")
      if (!(is(sim$studyAreaWithSpreadParams, "sf") || is(sim$studyAreaWithSpreadParams, "data.frame"))) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "estimateThreshold")
      }

      if ("debug" %in% P(sim)$mode) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
      } else {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "run")
        if ("visualize" %in% P(sim)$mode) {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "plot")
        }
      }
    },
    spreadFitPrepare = {
      sim <- spreadFitPrep(sim) # makes the covariates into the x1000 integers
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
      if (is.null(sim$studyAreaWithSpreadParams)) {

        termsInDEoptim(sim$fireSense_spreadFormula, mod$thresh, length(P(sim)$lower))
        useCache <- (isFALSE(getOption("fireSenseUtils.runTests")))
        if (!is.null(Par$cores) && !any(is.na(Par$cores)) && identical(sort(unique(Par$cores)), sort(Par$cores))) {
          best <- list(cluster = Par$cores)
        } else {

          best <- list(cluster = Par$cores,
                       bestCluster = as.data.table(table(Par$cores)))
        }
        messageDF(best$bestCluster)
        fnName <- paste0("runDEoptim_", P(sim)$rep)
        stop("Don't RUN DEOPTIM YET")

        # stop("Ended just before the runDEoptim")
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
                                   paths = getPaths(),
                                   libPath = normPath(P(sim)$libPathDEoptim),
                                   logPath = logPath(sim), ## TODO (#6): use tempdir()
                                   lower = P(sim)$lower,
                                   upper = P(sim)$upper,
                                   mutuallyExclusive = P(sim)$mutuallyExclusiveCols, ## TODO: test
                                   formulaToFit = sim$fireSense_spreadFormula,
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
                        omitArgs = c(".verbose", "cores"),
                        useCache = P(sim)$useCache_DE#,
        )
        objFunVal <- vapply(sim$DE, function(D) D$member$bestvalit, FUN.VALUE = numeric(1))
        ord <- order(objFunVal, decreasing = TRUE)
        DEBest <- head(sim$DE[ord], 5)
        terms <- fireSenseUtils:::termsInDEoptim(sim$fireSense_spreadFormula, mod$thresh, length(P(sim)$lower))
        paramsBest <- lapply(DEBest, function(D) as.data.table(D$member$bestmemit))#, FUN.VALUE = numeric(length(terms)))
        paramsBest <- rbindlist(paramsBest)


        df <- data.frame(I(list(paramsBest)),
                         I(list(sim$sppEquiv)),
                         I(list(sim$nonForestedLCCGroups)),
                         I(list(sim$missingLCCgroup))) |> setNames(sim$spreadFitAdditionalColNames)
        df <- data.frame(df, "polygonID" = sim$.runName)

        saHere <- if (is(sim$studyArea, "SpatVector")) sf::st_as_sf(sim$studyArea) else sim$studyArea
        saHere <- sf::st_as_sf(sf::st_geometry(saHere))
        sf::st_geometry(saHere) <- "geometry"
        sim$studyAreaWithSpreadParams <- saHere |>
          dplyr::mutate(df)
        le <- function(x) {x}
        sim$studyAreaWithSpreadParams <- CacheGeo(cloudFolderID = Par$spreadFitGoogleDriveFolder,
                                                  targetFile = Par$spreadFitFilename,
                                                  domain = saHere,
                                                  destinationPath = inputPath(sim),
                                                  FUN = le(studyAreaFireSense),
                                                  le = le, purge = 7,
                                                  studyAreaFireSense = sim$studyAreaWithSpreadParams,
                                                  action = "update")
      }
    },
    retrieveDEOptim = {
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
    # makefireSense_SpreadFitted = {
    #
    #   objFunValsAll <- unlist(lapply(sim$DE, function(x) x$optim$bestval))
    #   ordered <- order(objFunValsAll)
    #   outs <- rbindlist(lapply(sim$DE, function(x) data.frame(t(x$optim$bestmem))))
    #   set(outs, NULL, "objFunVal", objFunValsAll)
    #   set(outs, NULL, "iters",seq_len(length(sim$DE)))
    #   outs <- outs[ordered, ]
    #
    #   Nkeep <- 10
    #   sim$studyAreaWithSpreadParams <- sim$studyArea
    #   for (i in 2:Nkeep) {
    #     sim$studyAreaWithSpreadParams <- rbind(sim$studyAreaWithSpreadParams, sim$studyArea)
    #   }
    #   sim$studyAreaWithSpreadParams[, names(outs)] <- outs[seq_len(Nkeep),]
    #
    # },
    plot = {
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

# Init <- function(sim){
#   ## TODO: does this module need an init?
#   return(sim)
# }

spreadFitPrep <- function(sim) {
  # moduleName <- current(sim)$moduleName

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
      if (identical(nam, youngAge))
        c(l, sp_lcc)
      else
        l
    })
    message("Mutually exclusive is now:")
    message(Par[[mec]]) # message doesn't show name of list

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

  IDvar <- grep("ID", names(sim$spreadFirePoints[[1]]), value = TRUE) |> setdiff("GID")
  sim$lociList <- makeLociList(ras = sim$rasterToMatch, pts = sim$spreadFirePoints, idsCol = IDvar,
                               yearPrefix = fireSenseUtils::yearChar)

  keepNames <- intersect(names(sim$fireSense_annualSpreadFitCovariates), names(sim$fireBufferedListDT))
  mod$dat <- covsX1000AndSetDF(
    annualList = sim$fireSense_annualSpreadFitCovariates[keepNames],
    nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates,
    fireBufferedList = sim$fireBufferedListDT[keepNames],
    fireLociList = sim$lociList,
    paramOrder = P(sim)$upper)

  namesWithGTZeroRows <- lapply(mod$dat, function(x) names(x[sapply(x, function(y) NROW(y)) > 0]))
  annualDataNames <- grep("nonAnnual", names(namesWithGTZeroRows), invert = TRUE, value = TRUE)
  keepYearsNamed <- table(unname(unlist(namesWithGTZeroRows[annualDataNames]))) == length(annualDataNames)
  keepYears <- names(keepYearsNamed)[keepYearsNamed]
  mod$dat[annualDataNames] <- lapply(mod$dat[annualDataNames], function(x) x[keepYears])

  return(sim)
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
  annRescales <- rbindlist(annualList, fill = TRUE)
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


estimateSNLLThresholdPostLargeFires <- function(sim) {
  thresh <- if (is.null(Par$SNLL_FS_thresh) || is.na(Par$SNLL_FS_thresh)) {
    message("Estimating threshold for inside .objFunSpreadFit -- This can be supplied via SNLL_FS_thresh parameter")

    # Took 50 minutes using 10 cores for Taiga studyArea
    runSpreadWithoutDEoptim(
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
      formulaToFit = sim$fireSense_spreadFormula,
      objfunFireReps = P(sim)$objfunFireReps,
      tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
      mode = Par$mode,
      maxFireSpread = P(sim)$maxFireSpread) |>
      Cache(omitArgs = c("objfunFireReps", "mode"))
  } else {
    P(sim)$SNLL_FS_thresh
  }
  mod$thresh <- thresh
  return(sim)
}

# asFireSense_SpreadFitted <- function(DE, DEformulaChar, lower) {
#   DE2 <- if (is(DE, "list")) {
#     DE2 <- tail(DE, 1)[[1]]
#   } else {
#     DE
#   }
#
#   # DE1 <- tail(DE, 1)[[1]]
#     objFunValsAll <- unlist(lapply(DE, function(x) x$optim$bestval))
#     ordered <- order(objFunValsAll)
#     outs <- rbindlist(lapply(DE, function(x) data.frame(t(x$optim$bestmem))))
#     set(outs, NULL, "objFunVal", objFunValsAll)
#     set(outs, NULL, "iters",seq_len(length(DE)))
#     outs <- outs[ordered, ]
#
#     # head(outs[ordered,])
#
#     # bestvals <- which.min(objFunValsAll)
#     # DE1$optim$bestmem <- DE[[bestvals]]$optim$bestmem
#     # DE1$optim$bestval <- DE[[bestvals]]$optim$bestval
#     # DE1$optim$iter <- sum(unlist(lapply(DE, function(x) x$optim$iter)))
#     # DE1$member$bestmemit <- as.matrix(rbindlist(lapply(DE, function(x) as.data.table(x$member$bestmemit))))
#     # DE1$member$bestvalit <- rbindlist(lapply(DE, function(x) as.data.table(x$member$bestvalit)))[[1]]
#   # DE1$member <- as.matrix(rbindlist(lapply(DE, function(x) as.data.table(x$member$bestmemit))))
#
#   # options(opts)
#
#   ## TODO: use native R pipe
#   valAverage <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = median)
#   valSD <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = sd)
#   valBest <- DE2 %>% `[[`("optim") %>% `[[`("bestmem")
#   bestFit <- DE2$optim$bestval
#   terms <- terms(as.formula(DEformulaChar, env = .GlobalEnv))
#   # Identifying the number of parameters of the logistic function and names
#   nParsLogistic <- length(lower) - length(attributes(terms)[["term.labels"]])
#   if (nParsLogistic == 5) {
#     nms <- sim$fireSense_spreadLogisticTermNames
#     # nms <- c("inflectionPoint1", "inflectionPoint2",
#     #          "maxAsymptote", "hillSlope1", "hillSlope2")
#   } else if (nParsLogistic == 4) {
#     nms <- sim$fireSense_spreadLogisticTermNames[1:4]
#     # nms <- c("inflectionPoint1", "inflectionPoint2",
#     #          "maxAsymptote", "hillSlope1")
#   } else if (nParsLogistic == 3) {
#     nms <- sim$fireSense_spreadLogisticTermNames[c(3, 4, 1)]
#     # nms <- c("maxAsymptote", "hillSlope1", "inflectionPoint1")
#   } else if (nParsLogistic == 2) {
#     nms <- sim$fireSense_spreadLogisticTermNames[c(3, 4)]
#     # nms <- c("maxAsymptote", "hillSlope1")
#   }
#   # Giuseppe Cardillo (2020). Three parameters logistic regression -
#   # There and back again (https://www.github.com/dnafinder/logistic3),
#   # GitHub. Retrieved June 11, 2020.
#
#   fireSense_SpreadFitted <- list(
#     formula = DEformulaChar,
#     bestCoef = setNames(valBest,
#                         nm = c(nms,
#                                if (attr(terms, "intercept") != 0) "Intercept" else NULL,
#                                attr(terms, "term.labels")
#                         )
#     ),
#     meanCoef = setNames(valAverage,
#                         nm = c(nms,
#                                if (attr(terms, "intercept") != 0) "Intercept" else NULL,
#                                attr(terms, "term.labels")
#                         )
#     ),
#     sdCoef = setNames(valSD,
#                       nm = c(nms,
#                              if (attr(terms, "intercept") != 0) "Intercept" else NULL,
#                              attr(terms, "term.labels")
#                       )
#     ),
#     bestFit = bestFit
#   )
#
#   class(fireSense_SpreadFitted) <- "fireSense_SpreadFit"
#   fireSense_SpreadFitted
# }

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
  whYA <- formulaTerms[whAnnual] %in% youngAge
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

  # if (!suppliedElsewhere("fireSense_spreadLogisticTermNames")) {
  #   sim$fireSense_spreadLogisticTermNames <- c("inflectionPoint1", "inflectionPoint2",
  #                                              "maxAsymptote", "hillSlope1", "hillSlope2")
  #
  # }

  if (!suppliedElsewhere("spreadFitAdditionalColNames")) {
    sim$spreadFitAdditionalColNames <- fireSenseUtils::spreadFitAdditionalColNames
  }

  return(invisible(sim))
}


plotParamsBest <- function(paramsBest) {
  gg <- melt(paramsBest, measure.vars = colnames(paramsBest)) |>
    ggplot() + geom_histogram(aes_string("value")) + facet_wrap("variable", ncol=3)
  gg$plot_env <- new.env(parent = emptyenv())
  gg
}


youngAge <- fireSenseUtils::youngAgeName
