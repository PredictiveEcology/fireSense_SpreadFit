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
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = list(SpaDES.core = "0.1.0", fireSense_SpreadFit = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("DEoptim", "kSamples", "magrittr", "parallel", "raster", "data.table",
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
    defineParameter(name = "cores", class = "integer", default = 1,
                    desc = "non-negative integer. Defines the number of logical
                            cores to be used for parallel computation. The
                            default value is 1, which disables parallel 
                            computing."),
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
    defineParameter(name = "verbose", class = "logical", default = FALSE, 
                    desc = paste0("optional. Should it calculate and print median of spread ",
                                  "Probability during calculations?")),
    defineParameter(name = "maxFireSpread", class = "numeric", default = 2.55, 
                    desc = paste0("optional. Maximum fire spread average to be passed to the ",
                                  ".objFun for optimimzation. Default is 0.255")),
    defineParameter(name = "parallelMachinesIP", class = "character", default = NULL, 
                    desc = paste0("optional. If not NULL, will try to create a cluster using the ",
                                  "IP's addresses provided. It will devide the cores between all", 
                                  "machines as equaly as possible"))
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
              RasterStack should equal the number of distinct dates in column 'date'."
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_SpreadFitted",
    objectClass = "fireSense_SpreadFit",
    desc = "A fitted model object of class fireSense_SpreadFit."
  )
))

## event types
#   - type `init` is required for initialiazation

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
#   - `modulenameInit()` function is required for initiliazation;
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
  {
    stop(moduleName, "> The 'lower' parameter should be supplied.")
  }
  
  if (anyNA(P(sim)$upper))
  {
    stop(moduleName, "> The 'upper' parameter should be supplied.")
  }
  
  invisible(sim)
} 

spreadFitRun <- function(sim)
{
  moduleName <- current(sim)$moduleName
  
  hash <- fastdigest(sim$annualStacks)
  whNotNA <- which(!is.na(rasterToMatch[]))
  system.time(annualDTx1000 <- Cache(annualStacksToDTx1000, sim$annualStacks, 
                                 whNotNA = whNotNA,
                                 .fastHash = hash,
                                 omitArgs = c("annualStacks", "rasterToMatch")))
  
  hashNonAnnual <- fastdigest(sim$nonAnnualStacks)
  system.time(nonAnnualDTx1000 <- Cache(annualStacksToDTx1000, sim$nonAnnualStacks, 
                                 whNotNA = whNotNA,
                                 .fastHash = hashNonAnnual,
                                 omitArgs = c("annualStacks", "rasterToMatch")))
  
# TODO HAVE A SHAPEFILE of the ecoregions/ecodistricts and make this optimization perform in 
  #each ecoregion
  lociDF <- raster::extract(x = sim$rasterToMatch, 
                            y = sim[["fireAttributesFireSense_SpreadFit"]],
                            cellnumbers = TRUE,
                            df = TRUE,
                            sp = TRUE) %>% 
    as.data.table() %>%
    set(NULL, setdiff(colnames(.), c("size", "date", "cells")), NULL)
  lociList <- split(lociDF, f = lociDF$date, keep.by = FALSE)
  print("before makeBufferedFires")
  browser()
  fireBuffered <- Cache(makeBufferedFires, fireLocationsPolys = sim$firePolys,
                        rasterToMatch = rasterToMatch, useParallel = FALSE, 
                        omitArgs = "useParallel")
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

  # This below is to test the code without running DEOptim
  if (FALSE) {
    for (i in 1:100) {
      landscape = sim$rasterToMatch
      annualDTx1000 = lapply(annualDTx1000, setDF)
      nonAnnualDTx1000 = lapply(nonAnnualDTx1000, setDF)
      fireBufferedListDT = lapply(fireBufferedListDT, setDF)
      historicalFires = lapply(lociList, setDF)
      seed <- sample(1e6, 1)
      set.seed(seed)
      pars <- runif(length(P(sim)$lower), P(sim)$lower, P(sim)$upper)
      system.time(a <- .objfun(par = pars,
                               formula = formula, #loci = loci,
                               landscape = sim$rasterToMatch,
                               annualDTx1000 = lapply(annualDTx1000, setDF),
                               nonAnnualDTx1000 = lapply(nonAnnualDTx1000, setDF),
                               fireBufferedListDT = lapply(fireBufferedListDT, setDF),
                               historicalFires = lapply(lociList, setDF),
                               verbose = TRUE
      ))
    }
  }
  ####################################################################  
  # Final preparations of objects for .objfun
  ####################################################################  
  
  landscape <- sim$rasterToMatch
  annualDTx1000 <- lapply(annualDTx1000, setDF)
  nonAnnualDTx1000 <- lapply(nonAnnualDTx1000, setDF)
  fireBufferedListDT <- lapply(fireBufferedListDT, setDF)
  historicalFires <- lapply(lociList, setDF)

  # source any functions that are needed into .GlobalEnv so it doesn't have sim env
  source(file.path("~/GitHub/NWT/modules/fireSense_SpreadFit/R/objfun.R"))
  logistic4p <- get("logistic4p", envir = .GlobalEnv, inherits = FALSE)
  
  ####################################################################  
  #  Cluster
  ####################################################################  
  
  control <- list(itermax = P(sim)$iterDEoptim, 
                  trace = P(sim)$trace)
  logPath <- file.path(Paths$outputPath, 
                       paste0("fireSense_SpreadFit_log", Sys.getpid()))
  message(crayon::blurred(paste0("Starting parallel model fitting for ",
                                 "fireSense_SpreadFit. Log: ", logPath)))
  
  browser() # Make a cluster accross machines
  cl <- makeCluster(P(sim)$cores, outfile = logPath)
  # cl <- makeCluster(2, outfile = logPath)
  on.exit(stopCluster(cl))
  
  clusterExport(cl, list("landscape", 
                         "annualDTx1000",
                         "nonAnnualDTx1000",
                         "fireBufferedListDT",
                         "historicalFires", 
                         "logistic4p"), envir = environment())
  parallel::clusterEvalQ(
    cl, 
    for (i in c("kSamples", "magrittr", "raster", "data.table",
                "SpaDES.tools")) 
      library(i, character.only = TRUE)
    )
  parallel::clusterCall(cl, eval, P(sim)$clusterEvalExpr, env = .GlobalEnv)
  control$cluster <- cl
  
  #####################################################################
  # DEOptim call
  #####################################################################
  data.table::setDTthreads(1)
  DE <- Cache(DEoptim,
    get(".objfun", envir = .GlobalEnv), 
    lower = P(sim)$lower,
    upper = P(sim)$upper,
    control = do.call("DEoptim.control", control),
    formula = P(sim)$formula, 
    verbose = P(sim)$verbose,
    omitArgs = c("verbose")
  )
  
  val <- DE %>% `[[` ("optim") %>% `[[` ("bestmem")
  AD <- DE$optim$bestval
  browser()
  
  sim$fireSense_SpreadFitted <- list(
    formula = P(sim)$formula,
    coef = setNames(
      val,
      nm = c(
        "d", "a", "b", "g", 
        if (!is.null(attr(terms, "intercept"))) "Intercept" else NULL,
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
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("firePolys", sim)){
    sim$firePolys <- Cache(getFirePolygons, years = 1991:2017, studyArea = sim$studyArea, 
                           pathInputs = Paths$inputPath, userTags = c("years:1991_2017"))
  }
  
  return(invisible(sim))
}

annualStacksToDTx1000 <- function(annualStacks, whNotNA, ...) {
  # whNotNA <- which(!is.na(rasterToMatch[]))
  rastersDT <- lapply(annualStacks, whNotNA = whNotNA, function(x, whNotNA) {
    a <- as.data.table(x[])[whNotNA]
    a <- dtReplaceNAwith0(a)
    a
  })
  lapply(rastersDT, function(x) {
    for (col in colnames(x)) {
      set(x, NULL, col, asInteger(x[[col]]*1000))  
    }
  })

  rastersDT  
}


simplifyFireBuffered <- function(fireBuffered) {
  lapply(fireBuffered, function(r) {
    ras <- raster(r)
    nonNA <- which(!is.na(r[]))
    ras[r[] == 1] <- 0L
    ras[r[] == 0] <- 1L
    data.table(buffer = ras[][nonNA], pixelID = nonNA)
  })
}


logistic5p <- function(x, par) {
  par[1L] + (par[2L] - par[1L]) / (1 + (x/par[3L])^(-par[4L])) ^ par[5L]
}
