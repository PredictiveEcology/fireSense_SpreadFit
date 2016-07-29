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
    defineParameter(name = "formula", class = "formula", default = NULL,
      desc = 'an object of class formula: a symbolic description of the model to be fitted. Only the RHS needs to be provided'),
    defineParameter(name = "data", class = "character", default = NA,
      desc = "optional. A character vector indicating the names of objects present in the sim environment, in which to look for variables
      with which to predict. Objects can be named lists of RasterLayers, or RasterStacks (for time series). However, objects of different 
      classes cannot be mixed. For example, variables cannot be searched simultaneously within an object of class RasterLayer and within an
      object of class RasterStack. If omitted, or if variables are not found in the data objects, variables are searched in the sim environment."),
    defineParameter(name = "mapping", class = "character", default = NA, 
      desc = "optional. Named character vector to map variable names in the formula to those in the data objects.
        Names of unmapped variables are used directly to look for variables in data objects or in the sim environment."),
    defineParameter(name = "control", class = "list", default = NULL,
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
  } else if (eventType == "fit") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SpreadFit", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
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
fireSense_SpreadFitInit <- function(sim) scheduleEvent(sim, time(sim), "fireSense_SpreadFit", "fit", 0)

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

fireSense_SpreadFitFit <- function(sim) {
  
  ## Get the corresponding loci from raster landscape for the locations in input$location
  loci <- raster::extract(sim$landscape, sim$fireSense_SpreadFitFireLocation, cellnumbers = TRUE)[,"cells"]

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
  
  formula <- reformulate(terms, intercept = attr(terms, "intercept"))
  


  
  objFun <- function(par, formula, envData) {
    
    model.matrix(formula, envData)
    
    sim$fireSense_SizePredictTheta <- mget(varsTheta, envir = envir(sim), inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .) %>%
      lapply(function(x, sim)
        predict(x, model = formulaTheta, fun = fireSense_SizePredictThetaRaster, 
                na.rm = TRUE, sim = sim), sim = sim) %>%
      stack
    
    res <- lapply(1:10, function(i) {
      tabulate(SpaDES::spread(sim$landscape, loci = locis[[Y]], spreadProb=rt, mask=NULL, persistence=0L,
                              maxSize=rep_len(2e5L, length(locis[[Y]])), directions=8L, iterations=Inf, mapID=TRUE, 
                              returnIndices = TRUE)[["eventID"]])
    })
    
    apply(do.call("rbind", res), 2, median)    
  }
  
  DEop <- DEoptim(fOptim, lower=c(.2, .1, .01, .3, 0.0001, 0.001), upper=c(.5, 5, .2, 4, .4, .3),
                  control = DEoptim::DEoptim.control(itermax = 2000, trace = TRUE, parallelType = 1, packages = c("kSamples", "raster", "magrittr", "data.table"),
                                                     parVar = c("locis", "IDs", "v", "rt", "ad.test", "obsFS", "wrapLoad", ".data.dir")))
  
  
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
