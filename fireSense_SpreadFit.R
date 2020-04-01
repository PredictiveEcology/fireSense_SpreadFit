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
                    desc = "optional. An expression to evaluate on each cluster node. Ignored when parallel computing is disabled."),
    defineParameter(name = "trace", class = "numeric", default = 0,
                    desc = "non-negative integer. If > 0, tracing information on
                            the progress of the optimization are printed every
                            `trace` iteration. Default is 0, which turns off
                            tracing."),    
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time. By default, NA, which means that this module only runs once per simulation."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter(name = "termsNAtoZ", class = "character", default = NULL, 
                    desc = paste0("If your data has terms that have NA (i.e. rasters that were ",
                                  "not zeroed) you can pass the names of these terms and the ",
                                  "module will convert those to 0's internally")),
    defineParameter(name = "verbose", class = "logical", default = FALSE, 
                    desc = "optional. Should it calculate and print median of spread Probability during calculations?")
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
  
  if (FALSE) {
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  mod_env <- new.env(parent = emptyenv()) # 'emptyenv()' Avoid memory leak and C recursive problem
  ## Map the "fireAttributesFireSense_SpreadFit" parameter of this module to the "fireAttributesFireSense_SpreadFit" object in the module environment
  assign("fireAttributesFireSense_SpreadFit", value = sim[[P(sim)$fireAttributes]], envir = mod_env)
  .doDataChecks(moduleName = moduleName, envir = mod_env, attribs = P(sim)$fireAttributes, fml = P(sim)$formula)
  
  # Recovering fire sizes
  sizes <- mod_env[["fireAttributesFireSense_SpreadFit"]][["size"]]
  
  terms <- P(sim)$formula %>% terms.formula %>% delete.response ## If the formula has a LHS remove it
  allxy <- all.vars(terms)
  if (is.null(mod_env[["fireAttributesFireSense_SpreadFit"]][["date"]])) ## All fires started during the same time interval
  {
    for(x in P(sim)$data)
    {
      if (!is.null(sim[[x]]))
      {
        if (is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick"))
        {
          list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = mod_env)
        } 
        else if (is(sim[[x]], "RasterLayer")) 
        {
          mod_env[[x]] <- sim[[x]]
        } 
        else stop(moduleName, "> '", x, "' is not a RasterLayer, a RasterStack or a RasterBrick.")
      }
    }
    
# TODOAdd test for firePolygon to be a list of shapefiles
    
    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    rasters <- mget(allxy, envir = mod_env, inherits = FALSE) %>% raster::brick
    
    list2env(
      with(
        slot(
          ## Get loci from the raster sim$landscape for the fire locations
          raster::extract(rasters[[1L]], fireAttributesFireSense_SpreadFit, cellnumbers = TRUE, df = TRUE, sp = TRUE),
          "data"
        ),
        chk_duplicatedStartPixels(cells, sizes)
      ),
      envir = environment()
    )
    
    objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster)
    {
      r <- predict(rasters, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
        calc(function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
      
      spreadState <- SpaDES.tools::spread(
        landscape = r,
        loci = loci, 
        spreadProb = r,
        returnIndices = TRUE
      )
      
      spreadState[ , fire_id := .GRP, by = "initialLocus"] # Add an fire_id column
      
      first <- ad.test(
        list(
          tabulate( # Here tabulate() is equivalent to table() but faster
            spreadState[["fire_id"]]
          ),
          sizes
        )
      )[["ad"]][1,1]
      #second <- liklihood here
      scale(first) # + scale(second)
    }
  }
  else ## Fires started at different time intervals
  {
    for(x in P(sim)$data)
    {
      if (!is.null(sim[[x]]))
      {
        if (is(sim[[x]], "RasterLayer") || is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick"))
        {
          mod_env[[x]] <- sim[[x]]
        } 
        else 
          stop(moduleName, "> '", x, "' is not a RasterLayer, a RasterStack or a RasterBrick.")
      }
    }
    
    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (any(missing))
      stop(moduleName, "> '", paste(allxy[missing], collapse = "', '"), "' not found in data objects nor in the simList environment.")
    
    badClass <- !unlist(
      lapply(
        allxy, 
        function(x) is(sim[[x]], "RasterLayer") || is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick")
      )
    )
    
    if (any(badClass))
      stop(moduleName, "> '", paste(allxy[badClass], collapse = "', '"), "' does not match a RasterLayer, a RasterStack or a RasterBrick.")

    # move to RAM
    
    out <- lapply(mget(allxy, envir = envir(sim)), function(x) {
      raster::unstack(x)
    })
    out2 <- purrr::transpose(out)
    names(out2) <- as.character(1991:2017)
    
    rasters <- mget(allxy, envir = mod_env, inherits = FALSE) %>%
      lapply(function(x) if( is(x, "RasterStack") || is(x, "RasterBrick") ) unstack(x) else list(x)) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)
    names(rasters) <- as.character(1991:2017)
    rastersDT <- rbindlist(lapply(rasters[1:2], function(x) {
      a <- as.data.table(x[])
      }), use.names = TRUE, idcol = "year")
    set(rastersDT, NULL, "pixelID", rep(1:ncell(rasters[[1]]), length.out = NROW(rastersDT)))
    
    
    rastersDT1 <- na.omit(rastersDT, cols = c(names(rasters[[1]])))
    
    ## Get loci from the raster sim$landscape for the fire locations
    lociDF <- raster::extract(x = rasters[[1L]], 
                              y = sim[["fireAttributesFireSense_SpreadFit"]],
                         cellnumbers = TRUE,
                         df = TRUE,
                         sp = TRUE)
    
    # This has lots of NAs (probably the 0's in the original data), these should be converted to/ensured that are zeros 
    lociData <- data.table(slot(object = lociDF, name = "data"))
    
    dtReplaceNAwith0(DT = lociData, colsToUse = P(sim)$termsNAtoZ)
    #TODO Functions temporarily in R folder of the module. Will be moved to a package
    lociPerDate <- split(x = lociData, mod_env[["fireAttributesFireSense_SpreadFit"]][["date"]])
    
    #NOTE: somehow we might still have some NA's coming from the weather data. We should exclude these points, BUT warn the user
    originalNROW <- sum(unlist(lapply(X = lociPerDate, FUN = NROW)))
    lociPerDate <- lapply(X = split(x = lociData, mod_env[["fireAttributesFireSense_SpreadFit"]][["date"]]), FUN = na.omit)
    # Assertion:
    currentNROW <- sum(unlist(lapply(X = lociPerDate, FUN = NROW)))
    if (currentNROW != originalNROW)
      warning(paste0("There are ", originalNROW - currentNROW,
                     " rows that contain NA's in the dataset. These will be excluded for ",
                     "the fitting, but should be revised", immediate. = TRUE))
    
    # Removing duplicated fires on the same pixel on the same year
    lociPerDate <- lapply(lociPerDate, function(x){
      with(x, chk_duplicatedStartPixels(cells, size))
      })

    list2env(with(lociPerDate, list(loci = eapply(environment(), FUN = function(x) x[["loci"]]),
                                    sizes = eapply(environment(), FUN = function(x) x[["sizes"]]))),
             envir = environment())
    
  control <- list(itermax = P(sim)$iterDEoptim, trace = P(sim)$trace)
  
  if (P(sim)$cores > 1) # Creates cluster
  {
    if (.Platform$OS.type == "unix")
      mkCluster <- parallel::makeForkCluster
    else
      mkCluster <- parallel::makePSOCKcluster
    
    message(crayon::blurred(paste0("Starting parallel model fitting for ",
                                   "fireSense_SpreadFit. Log: ", file.path(Paths$outputPath, 
                                                                           "fireSense_SpreadFit_log"))))
    
    cl <- mkCluster(P(sim)$cores, outfile = file.path(Paths$outputPath, "fireSense_SpreadFit_log"))
    on.exit(stopCluster(cl))
    parallel::clusterEvalQ(cl, for (i in c("kSamples", "magrittr", "raster")) library(i, character.only = TRUE))
    parallel::clusterCall(cl, eval, P(sim)$clusterEvalExpr, env = .GlobalEnv)
    control$cluster <- cl
  }
  }
}
  
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
  
  fireBuffered <- Cache(makeBufferedFires, fireLocationsPolys = sim$firePolys,
                        rasterToMatch = rasterToMatch, useParallel = TRUE, 
                        omitArgs = "useParallel")
  names(fireBuffered) <- names(lociList)
  
  # nonNA <- which(!is.na(bufferedRealHistoricalFiresList[]))
  # return(bufferedRealHistoricalFiresList, nonNA = nonNA)
  # nonNAList
  
  # This up, is this: bufferedRealHistoricalFiresList
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
  # 13 iterations hit at 1:21pm
  control <- list(itermax = P(sim)$iterDEoptim, 
                  trace = P(sim)$trace)
  logPath <- file.path(Paths$outputPath, 
                       paste0("fireSense_SpreadFit_log", Sys.getpid()))
  message(crayon::blurred(paste0("Starting parallel model fitting for ",
                                 "fireSense_SpreadFit. Log: ", logPath)))
  browser()
  cl <- makeCluster(P(sim)$cores, outfile = logPath)
  on.exit(stopCluster(cl))
  landscape = sim$rasterToMatch
  annualDTx1000 = lapply(annualDTx1000, setDF)
  nonAnnualDTx1000 = lapply(nonAnnualDTx1000, setDF)
  fireBufferedListDT = lapply(fireBufferedListDT, setDF)
  historicalFires = lapply(lociList, setDF)
  
  clusterExport(cl, list("landscape", 
                         "annualDTx1000",
                         "nonAnnualDTx1000",
                         "fireBufferedListDT",
                         "historicalFires"), envir = environment())
  parallel::clusterEvalQ(
    cl, 
    for (i in c("kSamples", "magrittr", "raster", "data.table",
                "SpaDES.tools")) 
      library(i, character.only = TRUE)
    )
  parallel::clusterCall(cl, eval, P(sim)$clusterEvalExpr, env = .GlobalEnv)
  control$cluster <- cl
  
  DE <- Cache(DEoptim,
    .objfun, 
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
  whNotNA <- which(!is.na(rasterToMatch[]))
  rastersDT <- #rbindlist(
                     lapply(annualStacks, whNotNA = whNotNA, function(x, whNotNA) {
                       a <- as.data.table(x[])[whNotNA]
        #               set(a, NULL, "pixelID", whNotNA)
                       a <- dtReplaceNAwith0(a)
                       a
                     })
  lapply(rastersDT, function(x) {
    for (col in colnames(x)) {
      set(x, NULL, col, asInteger(x[[col]]*1000))  
    }
  })

  rastersDT  
  #, use.names = TRUE, idcol = "year")
  
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

logistic4p <- function(x, par) {
  par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]
}

logistic5p <- function(x, par) {
  par[1L] + (par[2L] - par[1L]) / (1 + (x/par[3L])^(-par[4L])) ^ par[5L]
}
