# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = "This module use Pattern Oriented Modelling (POM) to fit a fire landscape model where the probability of spread is heteregeneous, i.e. vary among pixels.",
  keywords = c("fire", "POM", "percolation"),
  authors=c(person("Jean", "Marchal", email="jean.d.marchal@gmail.com", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "kSamples", "magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NA,
      desc = 'an object of class formula: a symbolic description of the model to be fitted. Only the RHS needs to be provided'),
    defineParameter(name = "data", class = "character", default = NA,
      desc = "optional. A character vector indicating the names of objects present in the sim environment, in which to look for variables
      with which to predict. Objects can be named lists of RasterLayers, or RasterStacks (for time series). However, objects of different 
      classes cannot be mixed. For example, variables cannot be searched simultaneously within an object of class RasterLayer and within an
      object of class RasterStack. If omitted, or if variables are not found in the data objects, variables are searched in the sim environment."),
    defineParameter(name = "mapping", class = "character", default = NA, 
      desc = "optional. Named character vector to map variable names in the formula to those in the data objects.
        Names of unmapped variables are used directly to look for variables in data objects or in the sim environment."),
    defineParameter(name = "control", class = "list", default = NA,
      desc = 'optional. List of control parameters to be passed to DEoptim. see DEoptim.control')),
  inputObjects = data.frame(
    objectName = c("landscape", "fireSense_SpreadFitFireLocation", "fireSense_SpreadFitStack"),
    objectClass = c("RasterLayer", "SpatialPoints", "RasterStack"),
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## Toolbox: set of functions used internally by the module
  ## Raster predict function
    fireSense_SpreadFitRaster <- function(model, data, par) {

      model %>%
        model.matrix(data) %>%
        `%*%` (par) %>%
        drop

    }

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$fireSense_SpreadFitInit(sim)

    # schedule future event(s)
    # sim <- scheduleEvent(sim, params(sim)$fireSense_SpreadFit$.plotInitialTime, "fireSense_SpreadFit", "plot")
    # sim <- scheduleEvent(sim, params(sim)$fireSense_SpreadFit$.saveInitialTime, "fireSense_SpreadFit", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, params(sim)$fireSense_SpreadFit$.plotInitialTime, "fireSense_SpreadFit", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SpreadFit", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "run") {
    
    sim <- sim$fireSense_SpreadFitRun(sim)
    
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SpreadFit", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
fireSense_SpreadFitInit <- function(sim) {
  
  sim <- scheduleEvent(sim, time(sim), "fireSense_SpreadFit", "run")
  
  invisible(sim)
} 

### template for save events
fireSense_SpreadFitSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
fireSense_SpreadFitPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

fireSense_SpreadFitRun <- function(sim) {

  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))
  list2env(as.list(envir(sim)), envir = envData)
  
  if (!is.na(params(sim)$fireSense_SpreadFit$data[1]))
    lapply(params(sim)$fireSense_SpreadFit$data, function(x, envData) if (is.list(sim[[x]])) list2env(sim[[x]], envir = envData), envData = envData)
  
  ## In case there is a response in the formula remove it
  terms <- params(sim)$fireSense_SpreadFit$formula %>% terms.formula %>% delete.response
  
  ## Mapping variables names to data
  if (!is.na(params(sim)$fireSense_SpreadFit$mapping[1])) {
    
    for (i in 1:length(params(sim)$fireSense_SpreadFit$mapping)) {
      
      attr(terms, "term.labels") <- gsub(pattern = names(params(sim)$fireSense_SpreadFit$mapping[i]),
                                         replacement = params(sim)$fireSense_SpreadFit$mapping[i], x = attr(terms, "term.labels"))
      
    }
    
  }

  formula <- reformulate(attr(terms, "term.labels"), intercept = attr(terms, "intercept"))
  allVars <- all.vars(formula)
  
  if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterLayer"))))) {
    
  } else if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterStack"))))) {
  
    timeSeries <- mget(allVars, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)
    
    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(timeSeries[[1L]], sim$firesLocations, cellnumbers = TRUE, df = TRUE)[,"cells"]

    if (anyDuplicated(loci)) stop("fireSense_SpreadFit> Several fires share the same starting location (raster cell).")
    
    loci %<>% split(sim$firesLocations$time)
    
    objFun <- function(par, formula) {

      browser()
      
      timeSeries %>%
        mapply(FUN = function(x, loci) {
          
          r <- predict(x, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
            calc(function(x) par[3L] + par[1L] / (1 + x^(-par[2L])))
          
          ## 10 replicates to better an estimate of the median
          lapply(1:10, function(i) tabulate(spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
            do.call("rbind", .) %>%
            apply(2, median)
        }, loci = loci, SIMPLIFY = FALSE)
    }
      
  } else {
    
    varsExist <- allVars %in% ls(envData)
    varsClass <- unlist(lapply(allVars, function(x) is(envData[[x]], "RasterLayer") || is(envData[[x]], "RasterStack")))
    
    if (any(!varsExist)) {
      stop(paste0("fireSense_SizePredict> Variable '", allVars[which(!varsExist)[1L]], "' not found."))
    } else if (any(varsClass)) {
      stop("fireSense_SizePredict> Variables are not of the same class.")
    } else {
      stop(paste0("fireSense_SizePredict> Variable '", allVars[which(!varsClass)[1L]], "' is not a RasterLayer or a RasterStack."))
    }
  }
  
  DEop <- DEoptim(objFun, lower = c(.2, .1, .01, .3, 0.0001, 0.001), upper = c(.5, 5, .2, 4, .4, .3),
                  control = DEoptim::DEoptim.control(itermax = 2000, trace = TRUE), formula = formula)
                  #, parallelType = 0, packages = c("kSamples", "raster", "magrittr", "data.table"),
                  #                                   parVar = c("locis", "IDs", "v", "rt", "ad.test", "obsFS", "wrapLoad", ".data.dir")))
  
  
  invisible(sim)
}

### template for your event2
fireSense_SpreadFitEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
