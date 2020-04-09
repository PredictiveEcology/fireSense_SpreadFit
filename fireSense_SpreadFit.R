# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
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
    person("Eliot", "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))
  ),
  childModules = character(),
  version = list(fireSense_SpreadFit = "0.0.1", SpaDES.core = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "fastdigest", "kSamples", "magrittr", "parallel", "raster",
                  "rgeos","future",
                  "PredictiveEcology/fireSenseUtils@development (>=0.0.0.9008)",
                  "PredictiveEcology/SpaDES.tools@allowOverlap (>=0.3.4.9002)"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NA,
                    desc = 'a formula describing the model to be fitted. Only
                    the RHS needs to be provided.'),
    defineParameter(name = "data", class = "character",
                    default = "dataFireSense_SpreadFit",
                    desc = "a character vector indicating the names of objects in
                    the `simList` environment in which to look for variables
                    present in the model formula. `data` objects can be
                    RasterLayers, RasterStacks or RasterBricks. RasterStacks
                    and RasterBricks can be used in cases where fires have
                    started at different times and should not be spread at
                    the same time interval, but are still used to describe
                    the same fire size distribution. In this case, the
                    number of layers in the RasterStack should equal the
                    number of distinct dates in column 'date'."),
    defineParameter(name = "fireYears", class = "integer",
                    default = 1991:2017,
                    desc = "A numeric vector indicating which years should be extracted
                    from the fire databases to use for fitting"),
    defineParameter(name = "fireAttributes", class = "character",
                    default = "fireAttributesFireSense_SpreadFit",
                    desc = "a character vector indicating the name of an object of
                    class `SpatialPointsDataFrame` describing
                    fires starting locations, final sizes ('size'
                    column), and possibly the starting dates ('date'
                    column) if fires are to be spread at different time
                    steps. If the 'date' column is not present, all
                    fires are assumed to have started at the same time
                    interval."),
    defineParameter(name = "useCentroids", class = "logical", default = TRUE,
                    desc = paste("Should fire ignitions start at the sim$firePolygons",
                                 "centroids (TRUE) or at the ignition points in the",
                                 "sim$fireAttributesFireSense_SpreadFit")),
    defineParameter(name = "rescaleAll", class = "logical", default = TRUE,
                    desc = paste("Should all covariates to globally rescaled from 0 to 1;",
                                 "this allows covariate estimates to be on the same scale",
                                 "and will likely speed up convergence")),
    defineParameter(name = "lower", class = "numeric", default = NA,
                    desc = "see `?DEoptim`. Lower limits for the logistic function
                    parameters (lower bound, upper bound, slope, asymmetry)
                    and the statistical model parameters (in the order they
                    appear in the formula)."),
    defineParameter(name = "upper", class = "numeric", default = NA,
                    desc = "see `?DEoptim`. Upper limits for the logistic function
                    parameters (lower bound, upper bound, slope, asymmetry)
                    and the statistical model parameters (in the order they
                    appear in the formula)."),
    defineParameter(name = "iterDEoptim", class = "integer", default = 500,
                    desc = "integer defining the maximum number of iterations
                    allowed (DEoptim optimizer). Default is 500."),
    defineParameter(name = "strategy", class = "integer", default = 6,
                    desc = "Passed to DEoptim.control"),
    defineParameter(name = "cores", class = "integer", default = 1,
                    desc = "non-negative integer. Defines the number of logical
                    cores to be used for parallel computation. The
                    default value is 1, which disables parallel
                    computing."),
    defineParameter(name = "rescaleAll", class = "logical", default = TRUE,
                    desc = paste0("Should all covariates to globally rescaled from 0 to 1;",
                                  "this allows covariate estimates to be on the same scale",
                                  "and will likely speed up convergence")),
    defineParameter(name = "clusterEvalExpr", class = "expression", default = expression(),
                    desc = paste0("optional. An expression to evaluate on each cluster node. ",
                                  "Ignored when parallel computing is disabled.")),
    defineParameter(name = "trace", class = "numeric", default = 0,
                    desc = "non-negative integer. If > 0, tracing information on
                    the progress of the optimization are printed every
                    `trace` iteration. Default is 0, which turns off
                    tracing."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start
                    time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA,
                    desc = paste0("optional. Interval between two runs of this module,",
                                  "expressed in units of simulation time. By default, NA, which ",
                                  "means that this module only runs once per simulation.")),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA,
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA,
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    desc = paste0("Should this entire module be run",
                                  " with caching activated? This is generally intended for data-type ",
                                  "modules, where stochasticity and time are not relevant")),
    defineParameter(name = "termsNAtoZ", class = "character", default = NULL,
                    desc = paste0("If your data has terms that have NA (i.e. rasters that were ",
                                  "not zeroed) you can pass the names of these terms and the ",
                                  "module will convert those to 0's internally")),
    defineParameter(name = "toleranceFireBuffer", class = "numeric", default = c(3.8, 4.2),
                    desc = paste0("Lower and upper tolerance for fire buffering. ",
                                  "This is used for the function makeBufferedFires, and used ",
                                  "to generate the probability of distribution of fires for ",
                                  "the negative likelihood (in the objective function of the ",
                                  "optimizer). For now, we believe that the buffer needs to be @x4 ",
                                  "bigger than the fire")),
    defineParameter(name = "verbose", class = "logical", default = FALSE,
                    desc = paste0("optional. Should it calculate and print median of spread ",
                                  "Probability during calculations?")),
    defineParameter(name = "maxFireSpread", class = "numeric", default = 2.55,
                    desc = paste0("optional. Maximum fire spread average to be passed to the ",
                                  ".objFun for optimimzation. Default is 0.255")),
    defineParameter(name = "parallelMachinesIP", class = "character", default = NULL,
                    desc = paste0("optional. If not NULL, will try to create a cluster using the ",
                                  "IP's addresses provided. It will devide the cores between all",
                                  "machines as equaly as possible. Currently, supports only ",
                                  "2 machines"))
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fireAttributesFireSense_SpreadFit",
      objectClass = "SpatialPointsDataFrame",
      sourceURL = NA_character_,
      desc = "An object of class SpatialPointsDataFrame describing fires
      starting locations, final sizes ('size' column), and possibly the
      starting dates ('date' column) if fires are to be spread at
      different time intervals. If the 'date' column is not present, all
      fires are assumed to have started at the same time interval."
    ),
    expectsInput(
      objectName = "firePolys",
      objectClass = "list",
      sourceURL = NA_character_,
      desc = paste0("List of years of SpatialPolygonsDataFrame representing fire polygons.",
                    "This defaults to https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/ and uses ",
                    "the most current versions of the database (Nov or Sept 2019)")
    ),
    expectsInput(
      objectName = "polyCentroids",
      objectClass = "list",
      sourceURL = NA_character_,
      desc = paste0("List of years of SpatialPoints representing fire polygon's centroids.")
    ),
    expectsInput(
      objectName = "dataFireSense_SpreadFit",
      objectClass = "RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "One or more objects of class 'RasterLayer', 'RasterStack'
      or 'RasterBrick', in which to look for variables present
      in the model formula. RasterStacks and RasterBricks can
      be used in cases where fires have started at different
      times and should not be spread at the same time interval,
      but are still used to describe the same fire size
      distribution. In this case, the number of layers in the
      RasterStack should equal the number of distinct dates in column 'date'."),
    expectsInput(
      objectName = "studyArea",
      objectClass = "SpatialPolygonDataFrame",
      desc = "Study area for the prediction. Defaults to NWT",
      sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
    ),
    expectsInput(
      objectName = "rasterToMatch",
      objectClass = "RasterLayer",
      desc = paste0("All spatial outputs will be reprojected and resampled ",
                    "to it. Defaults to NWT"),
      sourceURL = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df"
    ),
    expectsInput(
      objectName = "flammableRTM",
      objectClass = "RasterLayer",
      desc = paste0("RasterToMatch where non-flammable pixels (LCC05 %in% c(33,36:39)) ",
                    "Defaults to NWT"),
      sourceURL = NA
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_SpreadFitted",
    objectClass = "fireSense_SpreadFit",
    desc = "A fitted model object of class fireSense_SpreadFit."
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE)
{
  moduleName <- current(sim)$moduleName

  switch(
    eventType,
    init = {
      sim <- spreadFitInit(sim)

      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "run")

      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
    },
    run = {
      sim <- spreadFitRun(sim)

      if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run")
    },
    save = {
      sim <- spreadFitSave(sim)

      if (!is.na(P(sim)$.saveInterval))
        sim <- scheduleEvent(sim, currentTime + P(sim)$.saveInterval, moduleName, "save", .last())

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initialization;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
spreadFitInit <- function(sim)
{
  moduleName <- current(sim)$moduleName
  # Checking parameters
  stopifnot(P(sim)$trace >= 0)
  stopifnot(P(sim)$cores >= 0)
  if (!is(P(sim)$formula, "formula"))
    stop(moduleName, "> The supplied object for the 'formula' parameter is not of class formula.")

  if (anyNA(P(sim)$lower))
    stop(moduleName, "> The 'lower' parameter should be supplied.")

  if (anyNA(P(sim)$upper))
    stop(moduleName, "> The 'upper' parameter should be supplied.")

  ####################### Assertions class 5
  # TODO
  # browser()
  # Wherever we have class 5 pixels, these are 1 and the sum of the other classes == 0
  # All class5 pixels are either 1 or 0
  #
  #######################

  invisible(sim)
}

spreadFitRun <- function(sim)
{
  moduleName <- current(sim)$moduleName

  hash <- fastdigest(sim$annualStacks)
  whNotNA <- which(!is.na(sim$flammableRTM[]))

  system.time(annualDTx1000 <- Cache(annualStacksToDTx1000, sim$annualStacks,
                                     whNotNA = whNotNA,
                                     .fastHash = hash,
                                     omitArgs = c("annualStacks", "rasterToMatch")))

  hashNonAnnual <- fastdigest(sim$nonAnnualStacks)
  system.time({
    nonAnnualDTx1000 <- Cache(annualStacksToDTx1000, sim$nonAnnualStacks,
                              whNotNA = whNotNA,
                              .fastHash = hashNonAnnual,
                              omitArgs = c("annualStacks", "rasterToMatch"))
  })

  # TODO HAVE A SHAPEFILE of the ecoregions/ecodistricts and make this optimization perform in
  #each ecoregion
  finalCols <- c("size", "date", "cells")
  if (isTRUE(P(sim)$useCentroids)) {2
    keepCols <- c("POLY_HA", "YEAR")
    lociDF <- purrr::map(sim$polyCentroids, ras = sim$flammableRTM,
                         function(.x, ras) {
                           raster::extract(x = ras,
                                           y = spTransform(.x[, keepCols], crs(ras)),
                                           cellnumbers = TRUE,
                                           sp = TRUE,
                                           df = TRUE) %>%
                             as.data.table()
    }) %>%
      rbindlist()
    set(lociDF, NULL, "size", round(lociDF$POLY_HA / (prod(res(sim$rasterToMatch))/1e4)))
    set(lociDF, NULL, setdiff(colnames(lociDF), c("size", "YEAR", "cells")), NULL)
    setnames(lociDF, "YEAR", "date")


  } else {
    lociDF <- raster::extract(x = sim$flammableRTM,
                              y = sim[["fireAttributesFireSense_SpreadFit"]],
                              cellnumbers = TRUE,
                              df = TRUE,
                              sp = TRUE) %>%
      as.data.table() %>%
      set(NULL, setdiff(colnames(.), finalCols), NULL)
  }
  lociList <- split(lociDF, f = lociDF$date, keep.by = FALSE)

  fireBuffered <- Cache(makeBufferedFires, fireLocationsPolys = sim$firePolys,
                        rasterToMatch = sim$flammableRTM, useParallel = TRUE,
                        omitArgs = "useParallel", verbose = TRUE,
                        lowerTolerance = P(sim)$toleranceFireBuffer[1],
                        upperTolerance = P(sim)$toleranceFireBuffer[2])
  names(fireBuffered) <- names(lociList)

  # All being passed should be lists of tables
  fireBufferedListDT <- Cache(simplifyFireBuffered, fireBuffered)

  # re-add pixelID to objects for join with fireBufferedListDT
  annualDTx1000 <- lapply(annualDTx1000, function(x) {
    setDT(x)
    set(x, NULL, "pixelID", whNotNA)
    x
  })
  annualDTx1000 <- Map(merge, fireBufferedListDT, annualDTx1000, MoreArgs = list(by = "pixelID"))
  nonAnnualDTx1000 <- lapply(nonAnnualDTx1000, function(x) {
    setDT(x)
    set(x, NULL, "pixelID", whNotNA)
    x
  })

  yearSplit <- strsplit(names(nonAnnualDTx1000), "_")
  names(yearSplit) <- as.character(seq_along(nonAnnualDTx1000))
  indexNonAnnual <- rbindlist(
    Map(ind = seq_along(nonAnnualDTx1000), date = yearSplit,
        function(ind, date) data.table(ind = ind, date = date))
  )

  # Take only pixels that burned during the years contained within each group of
  #   nonAnnualDTx1000
  nonAnnualDTx1000 <- Map(nonAnnDTx1000 = nonAnnualDTx1000,
                          index = seq_along(nonAnnualDTx1000),
                          MoreArgs = list(indexNonAnnual, fireBufferedListDT),
                          function(index, nonAnnDTx1000, indexNonAnnual, fireBufferedListDT) {
                            subDTs <- fireBufferedListDT[indexNonAnnual[index == ind]$date]
                            pixelIDs <- rbindlist(subDTs)$pixelID
                            nonAnnDTx1000[pixelID %in% pixelIDs]
                          })

  covMinMax <- if (P(sim)$rescaleAll) {
    nonAnnRescales <- rbindlist(nonAnnualDTx1000)
    vals <- setdiff(colnames(nonAnnRescales), "pixelID")
    covMinMax1 <- nonAnnRescales[, lapply(.SD, range), .SDcols = vals]

    annRescales <- rbindlist(annualDTx1000)
    vals <- setdiff(colnames(annRescales), c("buffer", "pixelID"))
    covMinMax2 <- annRescales[, lapply(.SD, range), .SDcols = vals]
    cbind(covMinMax1, covMinMax2)
  } else {
    NULL
  }

  # This below is to test the code without running DEOptim
  # Make a cluster accross machines
  if (FALSE) {
    for (i in 1:100) {
      seed <- sample(1e6, 1)
      set.seed(seed)
      (pars <- runif(length(P(sim)$lower), P(sim)$lower, P(sim)$upper))
      #pars <- runif(length(P(sim)$lower), lower, upper)
      #pars <- best
      system.time(a <- .objfun(par = pars,
                               formula = formula, #loci = loci,
                               landscape = sim$flammableRTM,
                               annualDTx1000 = lapply(annualDTx1000, setDF),
                               nonAnnualDTx1000 = lapply(nonAnnualDTx1000, setDF),
                               fireBufferedListDT = lapply(fireBufferedListDT, setDF),
                               historicalFires = lapply(lociList, setDF),
                               covMinMax = covMinMax,
                               maxFireSpread = P(sim)$maxFireSpread,
                               verbose = TRUE
      ))
    }
  }
  ####################################################################
  # Final preparations of objects for .objfun
  ####################################################################

  landscape <- sim$flammableRTM
  annualDTx1000 <- lapply(annualDTx1000, setDF)
  nonAnnualDTx1000 <- lapply(nonAnnualDTx1000, setDF)
  fireBufferedListDT <- lapply(fireBufferedListDT, setDF)
  historicalFires <- lapply(lociList, setDF)

  # source any functions that are needed into .GlobalEnv so it doesn't have sim env
  #source(file.path("~/GitHub/NWT/modules/fireSense_SpreadFit/R/objfun.R"))
  #logistic4p <- get("logistic4p", envir = .GlobalEnv, inherits = FALSE)

  ####################################################################
  #  Cluster
  ####################################################################

  control <- list(itermax = P(sim)$iterDEoptim,
                  trace = P(sim)$trace,
                  strategy = P(sim)$strategy)
  if (!is.null(P(sim)$parallelMachinesIP)){
    message("Starting ", P(sim)$cores, " clusters on ", paste(P(sim)$parallelMachinesIP,
                                                              collapse = ", "))
    if ((P(sim)$cores %% 2) != 0) params(sim)$cores <- P(sim)$cores - 1
    clusters <- c(rep("localhost", P(sim)$cores/2),
                  rep(P(sim)$parallelMachinesIP, P(sim)$cores/2))
    logPath <- file.path(Paths$outputPath,
                         paste0("fireSense_SpreadFit_log", Sys.getpid()))
    message(crayon::blurred(paste0("Starting parallel model fitting for ",
                                   "fireSense_SpreadFit. Log: ", logPath)))
    st <- system.time(cl <- makeCluster(clusters, outfile = logPath))
    hosts <- if (length(P(sim)$cores) > 1) unique(P(sim)$cores) else "this machine"

  } else {
    message("Starting ", paste(paste(unique(P(sim)$cores)), "x", table(P(sim)$cores),
                               collapse = ", "), " clusters")
    logPath <- file.path(Paths$outputPath,
                         paste0("fireSense_SpreadFit_log", Sys.getpid()))
    message(crayon::blurred(paste0("Starting parallel model fitting for ",
                                   "fireSense_SpreadFit. Log: ", logPath)))

    # Make sure logPath can be written in the workers -- need to create the dir
    st <- system.time(cl <- future::makeClusterPSOCK(unique(P(sim)$cores), revtunnel = TRUE))
    clusterExport(cl, list("logPath"), envir = environment())
    parallel::clusterEvalQ(
      cl, {
        reproducible::checkPath(dirname(logPath), create = TRUE)
      }
    )
    stopCluster(cl)


    st <- system.time(cl <- future::makeClusterPSOCK(P(sim)$cores, revtunnel = TRUE, outfile = logPath))

    # st <- system.time(cl <- makeCluster(P(sim)$cores, outfile = logPath))
  }
  on.exit(stopCluster(cl))
  message("it took ", round(st[3],2), "s to start ",
          paste(paste(unique(P(sim)$cores)), "x", table(P(sim)$cores),
                collapse = ", "), " threads")
  clusterExport(cl, list("landscape",
                         "annualDTx1000",
                         "nonAnnualDTx1000",
                         "fireBufferedListDT",
                         "historicalFires",
                         "logistic4p"), envir = environment())
  parallel::clusterEvalQ(
    cl, {
      for (i in c("kSamples", "magrittr", "raster", "data.table",
                  "SpaDES.tools", "fireSenseUtils"))
        library(i, character.only = TRUE)
    }
  )
  parallel::clusterCall(cl, eval, P(sim)$clusterEvalExpr, env = .GlobalEnv)
  control$cluster <- cl

  #####################################################################
  # DEOptim call
  #####################################################################
  data.table::setDTthreads(1)
  st1 <- system.time(DE <- Cache(DEoptim,
                                 fireSenseUtils::.objfun,
                                 lower = P(sim)$lower,
                                 upper = P(sim)$upper,
                                 control = do.call("DEoptim.control", control),
                                 formula = P(sim)$formula,
                                 covMinMax = covMinMax,
                                 maxFireSpread = P(sim)$maxFireSpread,
                                 verbose = P(sim)$verbose,
                                 omitArgs = c("verbose")
  ))

  val <- DE %>% `[[` ("optim") %>% `[[` ("bestmem")
  AD <- DE$optim$bestval

  terms <- terms(formula)
  sim$fireSense_SpreadFitted <- list(
    formula = P(sim)$formula,
    coef = setNames(
      val,
      nm = c(
        "d", "a", "b", "g",
        if (attr(terms, "intercept") != 0) "Intercept" else NULL,
        attr(terms, "term.labels")
      )
    ),
    AD = AD
  )

  class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"

  invisible(sim)
}

spreadFitSave <- function(sim)
{
  moduleName <- current(sim)$moduleName
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)

  saveRDS(
    sim$fireSense_SpreadFitted,
    file = file.path(paths(sim)$out, paste0("fireSense_SpreadFitted_", timeUnit, currentTime, ".rds"))
  )

  invisible(sim)
}

.inputObjects <- function(sim) {


  if (length(P(sim)$parallelMachinesIP) > 1){
    warning("Currently, only 2 machines (local and one more) can ",
            "be use to parallelize this module. Only first one will be used",
            immediate. = TRUE)
    params(sim)$parallelMachinesIP <- P(sim)$parallelMachinesIP[1]
  }

  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("firePolys", sim)){
    sim$firePolys <- Cache(getFirePolygons, years = P(sim)$fireYears,
                           studyArea = sim$studyArea,
                           pathInputs = Paths$inputPath, userTags = paste0("years:", range(P(sim)$fireYears)))
  }
  if (isTRUE(P(sim)$useCentroids)) {
    if (!suppliedElsewhere("polyCentroids", sim)){
      message("... preparing polyCentroids")
      yr <- min(P(sim)$fireYears)
      sim$polyCentroids <- Cache(lapply, X = sim$firePolys,
                                 function(X){
                                   print(yr)
                                   ras <- X
                                   ras$ID <- 1:NROW(ras)
                                   cent <- sf::st_centroid(sf::st_as_sf(ras), of_largest_polygon = TRUE)
                                   cent <- as(cent, "Spatial")
                                   # cent <- rgeos::gCentroid(ras, byid = TRUE)
                                   yr <<- yr + 1

                                   return(cent)
                                 })
      names(sim$polyCentroids) <- names(sim$firePolys)
    }
  } else {

    if (!suppliedElsewhere("fireAttributesFireSense_SpreadFit", sim)){

      # 1. To get the origin of the fire:
      # source(file.path(getwd(), "functions/getFirePoints_NFDB.R"))
      fireLocationsPoints <- Cache(getFirePoints_NFDB,
                                   url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$rasterToMatch,
                                   NFDB_pointPath = file.path(Paths$inputPath, "NFDB_point"),
                                   years = P(sim)$fireYears,

                                   userTags = c("what:firePoints", "forWhat:fireSense_SpreadFit"))
      # fireLocationsPoints <- fireLocationsPoints[fireLocationsPoints$YEAR <= max(fireYears) &
      #                                              fireLocationsPoints$YEAR >= min(fireYears),]
      # browser()
      # fireLocationsPoints <- fireLocationsPoints[, c("YEAR", "SIZE_HA")]
      # fireLocationsPoints$fireSize <- asInteger(fireLocationsPoints$SIZE_HA / prod(res(rasterToMatch)) * 1e4)
      # names(fireLocationsPoints) <- c("date", "size_ha", "size")
      #
      # # bigger than 1 pixel
      # fireLocationsPoints <- fireLocationsPoints[fireLocationsPoints$size > 1,]
      fireAttributesFireSense_SpreadFit <- fireLocationsPoints
      #
      # #    rasterTemp <- setValues(pixelGroupMap2001, values = 1:ncell(pixelGroupMap2001))
      crs(fireAttributesFireSense_SpreadFit) <- crs(sim$rasterToMatch)
      sim$fireAttributesFireSense_SpreadFit <- fireAttributesFireSense_SpreadFit

    }
  }

  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dataPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }

  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"),
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", destinationPath = dataPath(sim),
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID",
                                            "useCloud", "overwrite", "filename2"))
  }

  if (!suppliedElsewhere("flammableRTM", sim)){
    waterRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim),
                             studyArea = sim$studyArea, lccLayer = P(sim)$baseLayer,
                             rasterToMatch = sim$rasterToMatch,
                             userTags = c("objectName:wetLCC"))

    waterVals <- raster::getValues(waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
    waterVals[!is.na(waterVals) & waterVals != 1] <- NA
    waterRaster <- raster::setValues(waterRaster, waterVals)

    rstLCC <- Cache(prepInputs,
                    targetFile = file.path(dPath, "LCC2005_V1_4a.tif"),
                    archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                    url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
                                 "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
                    destinationPath = dPath,
                    studyArea = sim$studyArea,
                    rasterToMatch = sim$rasterToMatch,
                    maskWithRTM = TRUE,
                    method = "bilinear",
                    datatype = "INT2U",
                    filename2 = TRUE, overwrite = TRUE,
                    userTags = c("prepInputsrstLCC_rtm", currentModule(sim)),
                    omitArgs = c("destinationPath", "targetFile", "userTags"))

    # Ice/snow = 39
    # Water (LCC05) = 37:38
    # Rocks = 33
    # Urban = 36

    nonFlammClass <- c(33, 36:39)
    flammableRTM <- sim$rasterToMatch
    # Remove LCC non flammable classes first
    flammableRTM[rstLCC[] %in% nonFlammClass] <- NA
    # Remove more detailed water from DUCKS layer
    flammableRTM[waterRaster[] == 1] <- NA
    sim$flammableRTM <- flammableRTM
  }

  return(invisible(sim))
}
