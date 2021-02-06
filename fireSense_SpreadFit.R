defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = "Fit statistical models that can be used to parameterize the
  fire spread component of simulation models (e.g. fireSense).
  This module implement a Pattern Oriented Modelling (POM)
  approach to derive spread probabilities from final fire sizes.
  Spread probabilities can vary between pixels, and thus reflect
  local heterogeneity in environmental conditions.",
  keywords = c("fire", "spread", "POM", "percolation"),
  authors = c(
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut")),
    person("Eliot", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut")),
    person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut")),
    person("Alex M.", "Chubaty", email = "achubaty@for-cast.ca", role = c("ctb"))
  ),
  childModules = character(),
  version = list(fireSense_SpreadFit = "0.0.1", SpaDES.core = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "fastdigest", "kSamples", "magrittr", "parallel", "raster",
                  "rgeos","future", "logging",
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/Require@development",
                  "PredictiveEcology/fireSenseUtils@development (>=0.0.4.9021)",
                  "PredictiveEcology/SpaDES.tools@development (>=0.3.7)"),
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
    defineParameter(name = "debugMode", class = "logical", default = FALSE,
                    desc = "Set this to TRUE to run the .objfun manually without DEoptim"),
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
                    desc = "see `?DEoptim`. Upper limits for the logistic function
                    parameters (lower bound, upper bound, slope, asymmetry)
                    and the statistical model parameters (in the order they
                    appear in the formula)."),
    defineParameter(name = "urlDEOptimObject", class = "character",
                    default = paste0("https://drive.google.com/file/d/",
                                     "1GYsEbiE60m7cmP2Hfe0WCG_ng9o-RPP9/view?usp=sharing"),
                    desc = paste0("optional. If onlyLoadDEOptim == TRUE, you can pass the url to the  ",
                                  "DEOptim object. The default is the object from the run on 11JUN20",
                                  " from the logistic2p")),
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
                               "DEoptim run. If this is supplied, then debugMode will be automatically ",
                               "converted to TRUE")),
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
    createsOutput(objectName = "covMinMax", objectClass = "data.table",
                  desc = "data.table of covariates min and max"),
    createsOutput(objectName = "DE", objectClass = "data.table", desc = "DEOptim object"),
    createsOutput(objectName = "fireSense_SpreadFitted", objectClass = "fireSense_SpreadFit",
                  desc = "A fitted model object of class fireSense_SpreadFit."),
    createsOutput(objectName = "lociList", objectClass = "list", desc = "list of fire locs")
  )
))

doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE) {

  moduleName <- current(sim)$moduleName
  switch(
    eventType,
    init = {
      # Data sanity checks
      moduleName <- current(sim)$moduleName
      # Checking parameters
      stopifnot(P(sim)$trace >= 0)
      stopifnot(P(sim)$cores >= 0)

      if (anyNA(P(sim)$lower))
        stop(moduleName, "> The 'lower' parameter should be supplied.")

      if (anyNA(P(sim)$upper))
        stop(moduleName, "> The 'upper' parameter should be supplied.")

      if (P(sim)$onlyLoadDEOptim) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "retrieveDEOptim")
      } else {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "run")
      }
      if (!P(sim)$debugMode)
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "makefireSense_SpreadFitted")

      if (P(sim)$.plot && !P(sim)$debugMode)
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "plot")
    },
    run = {
      # Create buffers ##################################

      ###################################################
      # re-add pixelID to objects for join with fireBufferedListDT
      ###################################################

      # NPar <- length(P(sim)$lower)
      # NP <- NPar * 10
      # bestParsSoFar <- c(0.296, 1.50, 1.73, 2.79, 0.31, 0.22, 0.43, 1.73, 1.77)
      # bestParsSoFar <- c(0.298, 1.60, 1.94, 2.52, 0.34, 0.24, 0.54, 1.69, 1.82) # MAD = 512
      # bestParsSoFar <- c(0.298, 1.68, 2.04, 2.84, 0.27, 0.23, 0.54, 1.29, 1.51) # MAD = 515
      # bestParsSoFar <- c(0.258, 4.70, 1.60, 1.30, 2.75, 1.02, 2.84, 2.82, 2.08) # MAD = 517
      # bestParsSoFar <- c(0.264, 6.28, 0.79, 1.26, 0.68, 1.84, 0.39, 1.65, 2.23) # MAD = 506
      # #bestParsSoFar <- c(0.262, 5.34, 3.78, 2.36, 1.51, 1.49, 2.83, 1.72, 0.01) # MAD = 511
      # #bestParsSoFar <- c(0.253, 6.05, 2.60, 0.33, 1.42, 0.73, 2.50, 1.01, 0.43) # MAD = 498
      # bestParsSoFar <- c(0.261, 3.55, 3.25, 4.40, 0.92, 0.59, 0.04, 0.45, 0.27) # MAD 483
      # bestParsSoFar <- c(0.245, 6.77, 1.90, 4.24, 1.10, 0.45, 0.026, 0.45, 0.31) # SNLL 8082
      #
      # #Iteration: 37 bestvalit: 8011.000000 bestmemit:    0.252165    2.759695    2.560914    4.611344    1.250372    0.462948    0.032621    0.559400    0.460017
      # bestParsSoFar <- c(0.2432273, 8.1549921, 2.3991438, 4.7441794, 1.0863016, 1.7622997, 1.7679929, 2.1236577, 2.4785546) # "  22017 mad: 576.4 ;   SNLL_FSTest: 7522.5 ; "
      # #bestParsSoFar <- c(0.2853945, 2.5878158, 1.9309804, 0.3124527, 1.1377146, 1.9889269, 0.4055745, 2.3172345, 0.6643902) # After 2014 and 1995 only: mad: 3748.2 ;   SNLL_FSTest: 989.4
      # bestParsSoFar <- c(0.277, 1.568, 2.029, 5.79, 2.85, 0.19, 0.11, 2.785, 0.77) # mad: 467.7; SNLL_FSTest: 7560.7
      # bestParsSoFar <- c(0.2661138, 2.5002498, 3.7850266, 3.8305076, 2.5334282, 0.8581038, 1.3695494, 0.7245241, 1.0475760)#mad: 491.9 ;   SNLL_FSTest: 7432.7 ; SNLL_FSTest initial: 1067.1 ; "
      # bestParsSoFar <- c(0.2567797, 6.9269317, 1.0723940, 2.9256403, 1.4193608, 1.1382110, 2.8699367, 2.8805758, 2.1623407) # "  22017 mad: 2384.8 ;   SNLL_FSTest: 1098.6 ; " "  22017 mad: 535.8 ;   SNLL_FSTest: 7287.9 ; "
      #
      # betaVals <- data.frame(l = P(sim)$lower, u = P(sim)$upper, m = bestParsSoFar)
      # initialpop <- as.matrix(as.data.table(
      #   purrr::pmap(betaVals, function(l, u, m) rbetaBetween(NP, l = l, u = u, m = m, shape1 = 345))
      # ))

      ####################################################################
      # Final preparations of objects for objective function
      ####################################################################
      if (P(sim)$rescaleAll) {
        nonAnnRescales <- rbindlist(sim$fireSense_nonAnnualSpreadFitCovariates)
        vals1 <- setdiff(colnames(nonAnnRescales), "pixelID")
        covMinMax1 <- nonAnnRescales[, lapply(.SD, range), .SDcols = vals1]

        annRescales <- rbindlist(sim$fireSense_annualSpreadFitCovariates)
        vals2 <- setdiff(colnames(annRescales), c("buffer", "pixelID", "ids"))
        covMinMax2 <- annRescales[, lapply(.SD, range), .SDcols = vals2]
        sim$covMinMax <- cbind(covMinMax1, covMinMax2)

        if (any(is.na(sim$covMinMax))) {
          stop("covMinMax contains NA values. Check upstream for introduction of NAs.")
        }

        if (FALSE) { # This will plot histograms -- should evaluate
          extreme <- 4;
          for (v in vals1) {
            #v <- vals1[i]
            co <- nonAnnRescales[[v]]
            extreme <- 4
            mn <- mean(co)
            estd <- extreme * sd(co)
            extremeVals <- co[co > mn + estd | co < mn - estd]
            print(paste(v, ": ", length(extremeVals)))
          }

          par(mfrow = c(3,3))
          lapply(vals2, function(v) hist(annRescales[[v]], main = v))
          lapply(vals1, function(v) hist(nonAnnRescales[[v]], main = v))
        }
      } else {
        sim$covMinMax <- NULL
      }

      sim$lociList <- makeLociList(ras = sim$flammableRTM, pts = sim$spreadFirePoints)
      landscape <- sim$flammableRTM

      annualDT <- lapply(sim$fireSense_annualSpreadFitCovariates, setDF)

      annualDTx1000 <- toX1000(annualDT)
      nonAnnualDT <- lapply(sim$fireSense_nonAnnualSpreadFitCovariates, setDF)
      nonAnnualDTx1000 <- toX1000(nonAnnualDT)

      fireBufferedListDT <- lapply(sim$fireBufferedListDT, setDF)
      historicalFires <- lapply(sim$lociList, setDF)

      if (!is.null(sim$parsKnown)) {
        params(sim)[[currentModule(sim)]][["debugMode"]] <- TRUE
      }
      ## This below is to test the code without running DEOptim
      if (isTRUE(P(sim)$debugMode)) {
        thresh <- runSpreadWithoutDEoptim(
          1, P(sim)$lower, P(sim)$upper,
          sim$fireSense_spreadFormula, sim$flammableRTM,
          annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
          historicalFires, sim$covMinMax, P(sim)$objfunFireReps,
          P(sim)$maxFireSpread, pars = sim$parsKnown)
      } else {
        thresh <- if (is.null(P(sim)$SNLL_FS_thresh)) {
          Cache(runSpreadWithoutDEoptim,
                P(sim)$iterThresh, P(sim)$lower, P(sim)$upper,
                sim$fireSense_spreadFormula, sim$flammableRTM,
                annualDTx1000, nonAnnualDTx1000, fireBufferedListDT,
                historicalFires, sim$covMinMax, P(sim)$objfunFireReps,
                P(sim)$maxFireSpread)
          # runSpreadWithoutDEoptim(sim)
        } else {
          P(sim)$SNLL_FS_thresh
        }
        mod$thresh <- thresh

        termsInForm <- attr(terms(as.formula(sim$fireSense_spreadFormula)), "term.labels")
        logitNumParams <- length(lower) - length(termsInForm)
        message("Using a ", logitNumParams, " parameter logistic equation")
        message("  There will be ", length(lower), " terms: ")
        message("  ", paste(c(paste0("logit", seq(logitNumParams)), termsInForm), collapse = ", "))
        message("  objectiveFunction threshold SNLL to run all years after first 2 years: ", thresh)
        sim$DE <- Cache(runDEoptim,
                        landscape = landscape,
                        annualDTx1000 = annualDTx1000,
                        nonAnnualDTx1000 = nonAnnualDTx1000,
                        fireBufferedListDT = fireBufferedListDT,
                        historicalFires = historicalFires,
                        itermax = P(sim)$iterDEoptim,
                        iterStep = P(sim)$iterStep,
                        trace = P(sim)$trace,
                        initialpop = P(sim)$initialpop,
                        strategy = P(sim)$strategy,
                        cores = P(sim)$cores,
                        logPath = outputPath(sim),
                        cachePath = cachePath(sim),
                        lower = P(sim)$lower,
                        upper = P(sim)$upper,
                        FS_formula = sim$fireSense_spreadFormula,
                        covMinMax = sim$covMinMax,
                        objFunCoresInternal = P(sim)$objFunCoresInternal,
                        tests = c("SNLL_FS"), # c("mad", "SNLL_FS")
                        maxFireSpread = P(sim)$maxFireSpread,
                        Nreps = P(sim)$objfunFireReps,
                        thresh = thresh,
                        .verbose = P(sim)$verbose,
                        visualizeDEoptim = P(sim)$visualizeDEoptim,
                        .plotSize = P(sim)$.plotSize,
                        cacheId = P(sim)$cacheId_DE,
                        useCloud = P(sim)$useCloud_DE,
                        cloudFolderID = P(sim)$cloudFolderID_DE # Cloud cache was being a problem
        )

        if (isTRUE(P(sim)$visualizeDEoptim)) {
          if (!isRstudioServer()) {
            png(filename = file.path(outputPath(sim),
                                     paste0("DE_pars", as.character(Sys.time()), "_",
                                            Sys.getpid(), ".png")),
                width = P(sim)$.plotSize$width, height = P(sim)$.plotSize$height)
          }
          visualizeDE(sim$DE, cachePath(sim))
          if (!isRstudioServer()) {
            dev.off()
          }
        }

        if (isTRUE(P(sim)$visualizeDEoptim)) {
          fireBufferedListDT <- lapply(sim$fireBufferedListDT, setDF)
          hfs <- rbindlist(historicalFires)
          sam <- hfs[, list(keepInd = .I[SpaDES.tools:::resample(1:.N, min(.N, 49))]),
                     by = "date"]$keepInd
          hfs <- hfs[sam]
          fbl <- rbindlist(fireBufferedListDT, idcol = "date")
          hfs[, date := as.integer(gsub("year", "", date))]
          fbl[, date := as.integer(gsub("year", "", date))]
          fbl <- fbl[hfs[, c("size", "date", "ids")], on = c("date", "ids")]
          fbl <- split(fbl, by = "date")
          # fbl <- fbl[order(names(fbl))]
          hfs[, ids := as.character(ids)]
          hfs <- split(hfs, by = "date")
          hfs <- hfs[order(names(hfs))]
          # annualDTx1000 <- annualDTx1000[names(hfs)]

          DE2 <- if (is(sim$DE, "list")) {
            DE2 <- tail(sim$DE, 1)[[1]]
          } else {
            sim$DE
          }

          if (FALSE) { ## TODO: fix error here
            ### Error in nonAnnualDTx1000[[indexNonAnnual[date == yr]$ind]] :
            ###   attempt to select less than one element in get1index
            pdf(file.path(outputPath(sim), "FireHistsYr_Test.pdf"), width = 10, height = 7)
            out <- fireSenseUtils::.objfunSpreadFit(par = DE2$optim$bestmem,
                                                    landscape = sim$flammableRTM,
                                                    annualDTx1000 = annualDTx1000,
                                                    nonAnnualDTx1000 = nonAnnualDTx1000,
                                                    fireBufferedListDT = fbl,
                                                    historicalFires = hfs,
                                                    FS_formula = sim$fireSense_spreadFormula,
                                                    covMinMax = sim$covMinMax,
                                                    maxFireSpread = 0.28, # 0.257 makes gigantic fires
                                                    minFireSize = 2,
                                                    tests = "SNLL", # "SNLL_FS",
                                                    Nreps = P(sim)$objfunFireReps,
                                                    plot.it = TRUE,
                                                    #bufferedRealHistoricalFiresList,
                                                    thresh = P(sim)$SNLL_FS_thresh,
                                                    verbose = TRUE) #fireSense_SpreadFitRaster
            dev.off()
          }
        }
      }
    },
    retrieveDEOptim = {
      sim$DE <- Cache(prepInputs, url = P(sim)$urlDEOptimObject,
                      destinationPath = Paths$outputPath,
                      userTags = c("What:shortcutDEOptim"),
                      fun = "qs::qread")
    },
    makefireSense_SpreadFitted = {
      DE2 <- if (is(sim$DE, "list")) {
        DE2 <- tail(sim$DE, 1)[[1]]
      } else {
        sim$DE
      }
      valAverage <- DE2 %>% `[[`("member") %>% `[[`("bestmemit") %>%
        apply(MARGIN = 2, FUN = mean)
      valSD <- DE2 %>% `[[`("member") %>% `[[`("bestmemit") %>%
        apply(MARGIN = 2, FUN = sd)
      valBest <- DE2 %>% `[[`("optim") %>% `[[`("bestmem")
      bestFit <- DE2$optim$bestval
      terms <- terms(as.formula(sim$fireSense_spreadFormula))
      # Identifying the number of parameters of the logistic function and names
      nParsLogistic <- length(P(sim)$lower) - length(attributes(terms)[["term.labels"]])
      if (nParsLogistic == 5) {
        nms <- c("inflectionPoint1", "inflectionPoint2",
                 "maxAsymptote", "hillSlope1", "hillSlope2")
      } else if (nParsLogistic == 4) {
        nms <- c("inflectionPoint1", "inflectionPoint2",
                 "maxAsymptote", "hillSlope1")
      } else if (nParsLogistic == 3) {
        nms <- c("inflectionPoint1",
                 "maxAsymptote", "hillSlope1")
      } else if (nParsLogistic == 2) {
        nms <- c("maxAsymptote", "inflectionPoint1")
      }
      # Giuseppe Cardillo (2020). Three parameters logistic regression -
      # There and back again (https://www.github.com/dnafinder/logistic3),
      # GitHub. Retrieved June 11, 2020.

      sim$fireSense_SpreadFitted <- list(
        formula = sim$fireSense_spreadFormula,
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

      class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"
    },
    plot = {
      if (P(sim)$.plot) {
        if (FALSE) {
          DEout <- sim$fireSense_SpreadFitted
          par(mfrow = c(2,9));
          out <- lapply(seq(NCOL(sim$DE[[1]]$member$pop)), function(nc) {
            hist(sim$DE[[1]]$member$pop[, nc], main = names(DEout$bestCoef)[nc], xlab = "")
          })
          out <- Map(x = DEout$bestCoef, sds = DEout$sdCoef, nam = names(DEout$bestCoef),
                     function(x, sds, nam) hist(rnorm(1e5, x, sds), main = nam, xlab = ""))

        }



        historicalFires <- lapply(sim$lociList, setDF)
        hfs <- rbindlist(historicalFires)
        sam <- sample(NROW(hfs), 20)
        hfs <- hfs[sam]
        fbl <- rbindlist(fireBufferedListDT, idcol = "date")
        # hfs[, date := as.integer(date)]
        fbl[, date := as.integer(date)]
        fbl <- fbl[hfs, on = c("date", "ids")]
        fbl <- split(fbl, by = "ids")
        fbl <- fbl[order(names(fbl))]
        hfs[, ids := as.character(ids)]
        setorder(hfs, ids)
        r <- sim$flammableRTM
        # setDT(annualFireBufferedDT)
        starts <- unique(hfs$cells)
        names(starts) <- hfs$ids

        spreadState <- lapply(seq_len(Nreps), function(i) {
          SpaDES.tools::spread(
            landscape = r,
            maxSize = maxSizes,
            loci = loci,
            spreadProb = cells,
            returnIndices = TRUE,
            allowOverlap = FALSE,
            quick = TRUE)
        })

        out <- purrr::pmap(list(size = hfs$size, date = hfs$date,
                                ids = hfs$ids, cells = hfs$cells,
                                fbl = fbl),
                           function(size, date, ids, cells, fbl) {
                             ss <- spread(r, spreadProb = 1, loci = cells, returnIndices = TRUE)
                             out <- data.table(pixelID = ss$indices, ids = ss$id, prob = cells[ss$indices])
                           })
        out <- annualFireBufferedDT[out, on = "pixelID"]
        out[, burnedClass := buffer]

        r <- raster(sim$flammableRTM)
        r[out$pixelID] <- out$prob
        #clearPlot();Plot(r)
        #ex <- new("Extent", xmin = -1130927.72835113, xmax = -1029209.34163701,
        #          ymin = 8098144.00948992, ymax = 8224186.35824437)
        #exOther <- new("Extent", xmin = -1295020.59748428, xmax = -1180126.3836478,
        #               ymin = 8093087.29559748, ymax = 8233774.08805031)
        #exVSmall <- new("Extent", xmin = -1090977.9019513, xmax = -1070305.44912111,
        #                ymin = 8150890.10159652, ymax = 8173152.74310595)
        bigFire <- raster(r)
        bigFire[out$pixelID] <- out$ids
        keepFire <- tail(sort(table(out$ids)),1)
        keepFire <- as.numeric(names(keepFire))
        # keepFire <- 65
        bigFire[bigFire != keepFire] <- NA
        bf <- trim(bigFire)
        ex <- extent(bf)
        # ex <- exVSmall
        # ex <- clickExtent()
        # ex <- new("Extent", xmin = -1098283.46889952, xmax = -1037633.32535885,
        #            ymin = 7969991.96172249, ymax = 8030642.10526316)
        predictedFireProb <- crop(r, ex)
        # clearPlot();Plot(r)
        actualFire <- raster(r)
        actualFire[out$pixelID] <- out$burnedClass
        actualFire <- crop(actualFire, ex)
        levels(actualFire) <- data.frame(ID = 0:2, class = c("unburned", "burned", "ignited"))
        predictedLiklihood <- dbinom(prob = out$prob, size = 1, x = out$burned, log = TRUE)
        spreadProbMap <- raster(r)
        spreadProbMap[out$pixelID] <- cells[out$pixelID]
        spreadProbMap <- crop(spreadProbMap, ex)
        spreadProbMap[spreadProbMap >= par[1]] <- par[1]
        ccc <- cells[out$pixelID];
        ccc <- ccc[ccc > 0];
        lowerLim <- quantile(ccc, 0.05);
        ccc <- ccc[ccc > lowerLim];
        spreadProbMap[spreadProbMap <= lowerLim] <- lowerLim
        predLiklihood <- raster(r)
        predLiklihood[out$pixelID] <- predictedLiklihood
        predLiklihood <- crop(predLiklihood, ex)
        spIgnits <- SpatialPoints(coords = raster::xyFromCell(r, loci[36]))
        spIgnits <- buffer(spIgnits, width = 5000)
        spIgnits <- crop(spIgnits, ex)
        clearPlot(); Plot(actualFire, predictedFireProb, predLiklihood, spreadProbMap)
        Plot(spIgnits, addTo = "spreadProbMap", gp = gpar(fill = rep("black", 10)))
        Plot(spIgnits, addTo = "actualFire", gp = gpar(fill = rep("black", 10)))
        Plot(spIgnits, addTo = "predictedFireProb", gp = gpar(fill = rep("black", 10)))
        Plot(predLiklihood, cols = "RdYlGn", new = TRUE, legendRange = range(round(predLiklihood[], 0), na.rm = TRUE))
      }
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
    rstLCC <- prepInputsLCC(destinationPath = tempdir(),
                            rasterToMatch = sim$rasterToMatch)
    sim$flammableRTM <- LandR::defineFlammable(LandCoverClassifiedMap = rstLCC,
                                               mask = sim$rasterToMatch,
                                               filename2 = NULL)
  }

  return(invisible(sim))
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
