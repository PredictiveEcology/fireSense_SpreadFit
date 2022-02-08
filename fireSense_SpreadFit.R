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
  version = list(fireSense_SpreadFit = "0.0.1.9001", SpaDES.core = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "fastdigest", "future", "ggplot2", "kSamples", "logging",
                  "magrittr", "parallel", "raster", "rgeos", "tidyr",
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/Require@development",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9008)",
                  "PredictiveEcology/SpaDES.tools@development (>= 0.3.7.9007)"),
  parameters = rbind(
    defineParameter(name = ".plot", class = "logical", default = FALSE, ## TODO: use .plotInitialTime etc.
                    desc = "Should outputs be plotted?"),
    defineParameter(name = ".plotSize", class = "list", default = list(height = 1600, width = 2000),
                    desc = paste("List specifying height and width of plotting device (in pixels)",
                                 "used to plot DEoptim histograms when visualizeDEoptim is TRUE.")),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA,
                    desc = paste("optional. Interval between two runs of this module,",
                                 "expressed in units of simulation time. By default, NA, which",
                                 "means that this module only runs once per simulation.")),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA,
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA,
                    desc = "optional. Interval between save events."),
    defineParameter(name = ".useCache", "logical", FALSE, NA, NA,
                    desc = paste("Should this entire module be run",
                                 "with caching activated? This is generally intended for data-type",
                                 "modules, where stochasticity and time are not relevant.")),
    defineParameter(name = "cacheId_DE", class = "character", default = NULL,
                    desc = "An optional character string representing a cacheId to recover from the Cache"),
    defineParameter(name = "cloudFolderID_DE", class = "character", default = NULL,
                    desc = "Passed to cloudFolderID in the Cache(DEoptim...) call"),
    defineParameter(name = "cores", class = "integer", default = 1,
                    desc = paste("non-negative integer.",
                                 "Defines the number of logical cores to be used for parallel computation.",
                                 "The default value is 1, which disables parallel computing.")),
    defineParameter(name = "DEoptimTests", class = "character", default = "SNLL_FS",
                    desc = paste("Currently either SNLL_FS or adTest or a length 2 character vector of both. ",
                                 "These are passed to .objFunSpreadFit")),
    defineParameter(name = "doObjFunAssertions", class = "logical", default = TRUE,
                    desc = "This is passed to objFunSpreadProb; TRUE will do some diagnostics but is slower; FALSE for operational runs"),
    defineParameter(name = "initialpop", class = "numeric", default = NULL,
                    desc = paste("A numeric matrix of dimensions NCOL = length(lower)",
                                 "and NROW = NP. This will be passed into DEoptim",
                                 "through control$initialpop = P(sim)$initialpop if it is",
                                 "not NULL")),
    defineParameter(name = "iterDEoptim", class = "integer", default = 500,
                    desc = paste("integer defining the maximum number of iterations",
                                 "allowed (DEoptim optimizer). Default is 500.")),
    defineParameter(name = "iterStep", class = "integer", default = 25L,
                    desc = "Passed to runDEoptim"),
    defineParameter(name = "iterThresh", class = "integer", default = 96L,
                    desc = "Number of iterations for automated threshold calibration."),
    defineParameter(name = "lower", class = "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Lower limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (in the order they",
                                 "appear in the formula).")),
    defineParameter(name = "maxFireSpread", class = "numeric", default = 0.28,
                    desc = paste0("optional. Maximum fire spread average to be passed to the ",
                                  ".objFun for optimimzation. This puts an upper limit on spreadProb")),
    defineParameter(name = "mode", class = "character", default = "fit",
                    desc = paste("Options: debug, fit, visualize. Can use multiples. 'debug' will trigger running of",
                                 "the objective function with visuals; 'fit' will trigger DEoptim; 'visualize' will trigger",
                                 "visualization after DEoptim. For 'visualize', DE object must be findable, either in sim,",
                                 "on disk or a cloud URL. These last 2 can be specified with urlDEOptimObject param.")),
    defineParameter(name = "mutuallyExclusiveCols", "list", list("youngAge" = "vegPC"), NA, NA,
                    desc = "a named list of mutually exclusive covariates - see fireSenseUtils::makeMutuallyExclusive"),
    defineParameter(name = "NP", class = "integer", default = NULL,
                    desc = "Number of Populations. See DEoptim.control"),
    defineParameter(name = "objFunCoresInternal", class = "integer", default = 1L,
                    desc = paste("integer defining the number of cores to pass to mcmapply(mc.cores = ...)",
                                 "This will fork this many to do the years loop internally.",
                                 "This would be in addition to P(sim)$cores and is effecively a multiplier.",
                                 "The computer needs to have P(sim)$cores * objFunCoresInternal threads or it will stall")),
    defineParameter(name = "objfunFireReps", class = "integer", default = 100,
                    desc = paste("integer defining the number of replicates the objective function",
                                 "will attempt each fire. Since the default approach is",
                                 "using EnvStats::demp, it should be at least 100 to get a",
                                 "smooth distribution for a likelihood")),
    defineParameter(name = "onlyLoadDEOptim", class = "logical", default = FALSE,
                    desc = paste0("optional. If TRUE, the module will skip the fitting altogether ",
                                  "and will only load the latest uploaded version of the DEOptim object")),
    defineParameter(name = "parallelMachinesIP", class = "character", default = NULL,
                    desc = paste0("optional. If not NULL, will try to create a cluster using the ",
                                  "IP's addresses provided. It will devide the cores between all",
                                  "machines as equaly as possible. Currently, supports only 2 machines")),
    defineParameter(name = "rescaleAll", class = "logical", TRUE, NA, NA,
                    "rescale covariates for DEOptim"),
    defineParameter(name = "strategy", class = "integer", default = 6,
                    desc = "Passed to DEoptim.control"),
    defineParameter(name = "SNLL_FS_thresh", class = "integer", default = 550L,
                    desc = "Threshold multiplier used in objective function SNLL fire size test."),
    defineParameter(name = "trace", class = "numeric", default = 0,
                    desc = paste("non-negative integer. If > 0, tracing information on",
                                 "the progress of the optimization are printed every",
                                 "`trace` iteration. Default is 0, which turns off tracing.")),
    defineParameter(name = "upper", class = "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Upper limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (in the order they",
                                 "appear in the formula).")),
    defineParameter(name = "urlDEOptimObject", class = "character",
                    default = paste0("https://drive.google.com/file/d/",
                                     "1GYsEbiE60m7cmP2Hfe0WCG_ng9o-RPP9/view?usp=sharing"),
                    desc = paste0("optional. If onlyLoadDEOptim == TRUE, you can pass the url to the  ",
                                  "DEOptim object. The default is the object from the run on 11JUN20",
                                  " from the logistic2p")),
    defineParameter(name = "useCache_DE", class = "logical", default = TRUE,
                    desc = "should DEoptim use cache? to do multiple independent runs, use FALSE"),
    defineParameter(name = "useCloud_DE", class = "logical", default = FALSE,
                    desc = "Passed to useCloud in the Cache(DEoptim...) call"),
    defineParameter(name = "verbose", class = "logical", default = FALSE,
                    desc = paste0("optional. Should it calculate and print median of spread ",
                                  "Probability during calculations?")),
    defineParameter(name = "visualizeDEoptim", class = "logical", default = TRUE,
                    desc = "Passed to runDEoptim")
  ),
  inputObjects = rbind(
    expectsInput(objectName = "fireBufferedListDT", objectClass = "list",
                 desc = "list of data.tables with fire id, pixelID, and buffer status"),
    expectsInput(objectName = "flammableRTM", objectClass = "RasterLayer",
                 desc = paste0("RasterToMatch where non-flammable pixels (LCC05 %in% c(33,36:39)) ",
                               "Defaults to NWT"), sourceURL = NA),
    expectsInput(objectName = "fireSense_annualSpreadFitCovariates", objectClass = "data.table",
                 desc = "table of climate PCA components, burn status, polyID, and pixelID"),
    expectsInput(objectName = "fireSense_nonAnnualSpreadFitCovariates", objectClass = "data.table",
                 desc = "table of veg PCA components, burn status, polyID, and pixelID"),
    expectsInput(objectName = "fireSense_spreadFormula", objectClass = "character",
                 desc = paste0("a formula that contains the annual and non-annual covariates",
                               "e.g. ~ 0 + MDC + vegPC1 + vegPC2")),
    expectsInput(objectName = "parsKnown", objectClass = "numeric",
                 desc = paste0("Optional vector of known parameters, e.g., from a previous ",
                               "DEoptim run. If this is supplied, then 'mode' will be automatically ",
                               "converted to 'debug'")),
    expectsInput(objectName = "PCAveg", objectClass = "prcomp",
                 desc = "if covariates were created using PCA, the PCA object will be added to fireSense_spreadFitted"),
    #expectsInput(objectName = "polyCentroids", objectClass = "list", sourceURL = NA_character_,
    #             desc = "list of years of SpatialPoints representing fire polygon's centroids."),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "template raster for study area"),
    expectsInput(objectName = "spreadFirePoints", objectClass = "SpatialPointsDataFrame",
                 desc = "ist of spatialPolygonDataFrame objects representing annual fire centroids"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame",
                 desc = "Study area for the prediction. Defaults to NWT",
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU")
  ),
  outputObjects = rbind(
    createsOutput("covMinMax_spread", objectClass = "data.table",
                  desc = "data.table of covariates min and max"),
    createsOutput("DE", objectClass = "data.table", desc = "DEOptim object"),
    createsOutput("fireSense_SpreadFitted", objectClass = "fireSense_SpreadFit",
                  desc = "A fitted model object of class fireSense_SpreadFit."),
    createsOutput("fsSpreadFit_hists", objectClass = "ggplot",
                  desc = "histograms of each parameter used in DEoptim fitting."),
    createsOutput(objectName = "lociList", objectClass = "list", desc = "list of fire locs")
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

      if ("debug" %in% P(sim)$mode) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
      } else {
        if ("fit" %in% P(sim)$mode) {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "estimateThreshold")
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
    debug = {
      ## This below is to test the code without running DEOptim
      thresh <- runSpreadWithoutDEoptim(
        iterThresh = P(sim)$iterThresh, P(sim)$lower, P(sim)$upper,
        sim$fireSense_spreadFormula, sim$flammableRTM,
        mod$dat$annualDTx1000, mod$dat$nonAnnualDTx1000, mod$dat$fireBufferedListDT,
        mutuallyExclusive = P(sim)$mutuallyExclusiveCols,
        doObjFunAssertions = P(sim)$doObjFunAssertions,
        mod$dat$historicalFires, sim$covMinMax_spread, P(sim)$objfunFireReps,
        P(sim)$maxFireSpread, pars = sim$parsKnown, plot.it = P(sim)$.plot,
        tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
        mode = "debug")
    },
    estimateThreshold = {
      # Estimate threshold for .objFunSpreadFit
      sim <- estimateSNLLThresholdPostLargeFires(sim)
    },
    run = {
      termsInForm <- attr(terms(as.formula(sim$fireSense_spreadFormula)), "term.labels")
      logitNumParams <- length(lower) - length(termsInForm)
      message("Using a ", logitNumParams, " parameter logistic equation")
      message("  There will be ", length(lower), " terms: ")
      message("  ", paste(c(paste0("logit", seq(logitNumParams)), termsInForm), collapse = ", "))
      message("  objectiveFunction threshold SNLL to run all years after first 2 years: ", mod$thresh)

      opts <- options(parallelly.makeNodePSOCK.setup_strategy = "sequential") ## default 'parallel' not working
      sim$DE <- Cache(runDEoptim,
                      landscape = sim$flammableRTM,
                      annualDTx1000 = mod$dat$annualDTx1000,
                      nonAnnualDTx1000 = mod$dat$nonAnnualDTx1000,
                      fireBufferedListDT = mod$dat$fireBufferedListDT,
                      historicalFires = mod$dat$historicalFires,
                      itermax = P(sim)$iterDEoptim,
                      iterStep = P(sim)$iterStep,
                      trace = P(sim)$trace,
                      initialpop = P(sim)$initialpop,
                      strategy = P(sim)$strategy,
                      cores = P(sim)$cores,
                      doObjFunAssertions = P(sim)$doObjFunAssertions,
                      logPath = outputPath(sim),
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
                      .verbose = P(sim)$verbose,
                      visualizeDEoptim = P(sim)$visualizeDEoptim,
                      .plotSize = P(sim)$.plotSize,
                      cacheId = P(sim)$cacheId_DE,
                      useCache = P(sim)$useCache_DE,
                      useCloud = P(sim)$useCloud_DE,
                      cloudFolderID = P(sim)$cloudFolderID_DE ## Cloud cache was being a problem
      )
      options(opts)
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
    makefireSense_SpreadFitted = {
      sim$fireSense_SpreadFitted <- asFireSense_SpreadFitted(sim$DE, sim$fireSense_spreadFormula,
                                                             lower = P(sim)$lower,
                                                             PCAveg = sim$PCAveg)
    },
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

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dataPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }

  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"),
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", destinationPath = dataPath(sim),
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID",
                                            "useCloud", "overwrite", "filename2"))
  }

  if (!suppliedElsewhere("flammableRTM", sim)) {
    rstLCC <- prepInputsLCC(year = 2010,
                            destinationPath = tempdir(),
                            rasterToMatch = sim$rasterToMatch)
    sim$flammableRTM <- LandR::defineFlammable(LandCoverClassifiedMap = rstLCC,
                                               mask = sim$rasterToMatch,
                                               filename2 = NULL)
  }

  return(invisible(sim))
}

Init <- function(sim) {
  moduleName <- current(sim)$moduleName
  # Checking parameters
  stopifnot(P(sim)$trace >= 0)
  stopifnot(P(sim)$cores >= 0)

  if (anyNA(P(sim)$lower))
    stop(moduleName, "> The 'lower' parameter should be supplied.")

  if (anyNA(P(sim)$upper))
    stop(moduleName, "> The 'upper' parameter should be supplied.")

  if (P(sim)$rescaleAll) {
    sim$covMinMax_spread <- deriveCovMinMax(annualList = sim$fireSense_annualSpreadFitCovariates,
                                            nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates)
    if (any(is.na(sim$covMinMax_spread))) {
      stop("covMinMax_spread contains NA values. Check upstream for introduction of NAs.")
    }
  }
  if (Par$.plot && "debug" %in% P(sim)$mode) {
    try(histOfCovariates(annualList = sim$fireSense_annualSpreadFitCovariates,
                         nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates))
  }

  sim$lociList <- makeLociList(ras = sim$flammableRTM, pts = sim$spreadFirePoints)

  mod$dat <- covsX1000AndSetDF(
    annualList = sim$fireSense_annualSpreadFitCovariates,
    nonAnnualList = sim$fireSense_nonAnnualSpreadFitCovariates,
    fireBufferedList = sim$fireBufferedListDT,
    fireLociList = sim$lociList)

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
  covMinMax1 <- nonAnnRescales[, lapply(.SD, range), .SDcols = vals1]

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

covsX1000AndSetDF <- function(annualList, nonAnnualList, fireBufferedList, fireLociList) {
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
  thresh <- if (is.null(P(sim)$SNLL_FS_thresh)) {
    message("Estimating threshold for inside .objFunSpreadFit -- This can be supplied via SNLL_FS_thresh parameter")

    Cache(runSpreadWithoutDEoptim,
          iterThres = P(sim)$iterThresh,
          lower = P(sim)$lower, upper = P(sim)$upper,
          fireSense_spreadFormula = sim$fireSense_spreadFormula,
          flammableRTM = sim$flammableRTM,
          mutuallyExclusive =  P(sim)$mutuallyExclusiveCols,
          doObjFunAssertions = P(sim)$doObjFunAssertions,
          annualDTx1000 = mod$dat$annualDTx1000,
          nonAnnualDTx1000 = mod$dat$nonAnnualDTx1000,
          fireBufferedListDT = mod$dat$fireBufferedListDT,
          historicalFires = mod$dat$historicalFires,
          covMinMax = sim$covMinMax_spread,
          objfunFireReps = P(sim)$objfunFireReps,
          tests = P(sim)$DEoptimTests, # c("mad", "SNLL_FS")
          maxFireSpread = P(sim)$maxFireSpread)
  } else {
    P(sim)$SNLL_FS_thresh
  }
  mod$thresh <- thresh
  return(sim)
}

asFireSense_SpreadFitted <- function(DE, DEformulaChar, lower, PCAveg = NULL) {
  DE2 <- if (is(DE, "list")) {
    DE2 <- tail(DE, 1)[[1]]
  } else {
    DE
  }
  valAverage <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = median)
  valSD <- DE2 %>% `[[`("member") %>% `[[`("pop") %>% apply(MARGIN = 2, FUN = sd)
  valBest <- DE2 %>% `[[`("optim") %>% `[[`("bestmem")
  bestFit <- DE2$optim$bestval
  terms <- terms(as.formula(DEformulaChar))
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
    bestFit = bestFit,
    PCAveg = PCAveg
  )

  if (is.null(PCAveg)) {
    fireSense_SpreadFitted <- fireSense_SpreadFitted[!names(fireSense_SpreadFitted) == "PCAveg"]
  }

  class(fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  fireSense_SpreadFitted
}
