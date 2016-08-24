# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = "Fit statistical models that can be used to parametrize 
                 (calibrate) the fire spread component of simulation models (e.g.
                 fireSense). This module makes use of Pattern Oriented Modelling
                 (POM) to derive spread probabilities that can vary among pixels,
                 i.e. reflecting heterogeneity in local environmental conditions.",
  keywords = c("fire spread", "POM", "percolation"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "kSamples", "magrittr", "parallel", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NULL,
      desc = 'an object of class formula: a symbolic description of the model to
              be fitted. Only the RHS needs to be provided.'),
    defineParameter(name = "lower", class = "numeric", default = NULL, desc = "see ?DEoptim."),
    defineParameter(name = "upper", class = "numeric", default = NULL, desc = "see ?DEoptim."),
    defineParameter(name = "data", class = "character", default = NULL,
      desc = "optional. A character vector indicating the names of objects in the
              simList environment in which to look for variables in the model. 
              Data objects can be named lists of RasterLayers or RasterStacks
              (for time series), but should all be of one unique class, e.g. 
              RasterLayer. If omitted, or if variables are not found in data
              objects, variables are searched in the simList environment."),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
      desc = "optional. Named character vector or list mapping the names of data
              objects required by the module to those in the simList environment."),
    defineParameter(name = "trace", class = "numeric", default = 0,
      desc = "non-negative integer. If > 0, tracing information on the progress of the 
              optimization is produced every trace iteration. Defaults to 0 which indicates no
              trace information is to be printed."),
    defineParameter(name = "parallel", class = "logical", default = FALSE, 
      desc = 'Should the optimization be parallelized ? (all detected cores will be used)'),
    defineParameter(name = "initialRunTime", class = "numeric", default = start(sim),
      desc = "optional. Simulation time at which to start this module. Defaults 
              to simulation start time."),
    defineParameter(name = "intervalRunModule", class = "numeric", default = NA, 
      desc = "optional. Interval in simulation time units between two runs of 
              this module.")
  ),
  inputObjects = data.frame(
    objectName = "fires",
    objectClass = "SpatialPointsDataFrame",
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = "fireSense_SpreadFitted",
    objectClass = "fireSense_SpreadFit",
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- sim$fireSense_SpreadFitInit(sim)

  } else if (eventType == "run") {
    sim <- sim$fireSense_SpreadFitRun(sim)

  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    
    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function
    
    # schedule future event(s)
    
    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "fireSense_SpreadFit", "save")
    
    # ! ----- STOP EDITING ----- ! #
      
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
fireSense_SpreadFitInit <- function(sim) {
  
  sim <- scheduleEvent(sim, eventTime = p(sim)$initialRunTime, "fireSense_SpreadFit", "run")
  sim
  
} 

fireSense_SpreadFitRun <- function(sim) {

  ## Toolbox: set of functions used internally by fireSense_SpreadFitRun
    ## Raster predict function
      fireSense_SpreadFitRaster <- function(model, data, par) {
        
        model %>%
          model.matrix(data) %>%
          `%*%` (par) %>%
          drop
        
      }

  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))
  list2env(as.list(envir(sim)), envir = envData)
  
  if (!is.null(p(sim)$data)) ## Handling data arg
    lapply(p(sim)$data, function(x, envData) if (is.list(sim[[x]])) list2env(sim[[x]], envir = envData), envData = envData)

  ## Mapping data objects required by the module to those in the simList environment
  if (!is.null(p(sim)$mapping)) {

    envData[["fires"]] <- envData[[p(sim)$mapping[["fires"]]]]
    rm(list = p(sim)$mapping[["fires"]], envir = envData)
    
  }
  
  if (is.empty.model(p(sim)$formula))
    stop("fireSense_SpreadFit> The formula describes an empty model.")

  terms <- p(sim)$formula %>% terms.formula %>% delete.response ## If the formula has a LHS remove it
  allxy <- all.vars(terms)

  if (all(unlist(lapply(allxy, function(x) is(envData[[x]], "RasterStack"))))) {

    rasters <- mget(allxy, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)

    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(rasters[[1L]], sim$fires, cellnumbers = TRUE, df = TRUE)[["cells"]]

    loci %<>% split(sim$fires$date)
    lapply(loci, function(x) if (anyDuplicated(x)) stop("fireSense_SpreadFit> No more than one fire can start in a given pixel."))
    sizes <- sim$fires$size
    
    objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster) {
      
      (rasters %>%
         mapply(FUN = function(x, loci) {
           
           r <- predict(x, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
             calc(function(x) par[3L] + par[1L] / (1 + x^(-par[2L])) ^ par[4L]) ## 5-parameters logistic
           
           ## 10 replicates to better estimate the median
           lapply(1:10, function(i) tabulate(SpaDES::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
             do.call("rbind", .) %>%
             apply(2L, median)
         }, loci = loci, SIMPLIFY = FALSE) %>%
         unlist %>% list(sizes) %>% ad.test %>% `[[` ("ad"))[1L, 1L]
      
    }
    
  } else if (all(unlist(lapply(allxy, function(x) is(envData[[x]], "RasterLayer"))))) {
    
    rasters <- mget(allxy, envir = envData, inherits = FALSE) %>% stack
    
    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(rasters, sim$fires, cellnumbers = TRUE, df = TRUE)[["cells"]]
    
    if (anyDuplicated(loci)) stop("fireSense_SpreadFit> No more than one fire can start in a given pixel.")
    
    sizes <- sim$fires$size
    
    objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster) {
      
      r <- predict(rasters, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
        calc(function(x) par[3L] + par[1L] / (1 + x^(-par[2L]))) ## 5-parameters logistic
      
      ## 10 replicates to better estimate the median
      (lapply(1:10, function(i) tabulate(SpaDES::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
          do.call("rbind", .) %>%
          apply(2L, median) %>%
          list(sizes) %>%
          ad.test %>%
          `[[` ("ad"))[1L, 1L]
      
    }
      
  } else {
    
    exist <- allxy %in% ls(envData)
    class <- unlist(lapply(allxy, function(x) is(envData[[x]], "RasterLayer") || is(envData[[x]], "RasterStack")))
    
    if (any(!exist)) {
      stop(paste0("fireSense_SpreadFit> Variable '", allxy[which(!exist)[1L]], "' not found."))
    } else if (any(class)) {
      stop("fireSense_SpreadFit> Data objects are not of the same class (e.g. data.frames).")
    } else {
      stop(paste0("fireSense_SpreadFit> Variable '", allxy[which(!class)[1L]], "' does not match a RasterLayer or a RasterStack."))
    }
  }
  
  
  control <- list(itermax = 2000, trace = p(sim)$trace)
  
  if (p(sim)$parallel) {
    
    control$parallelType <- 1
    control$packages <- c("data.table", "kSamples", "magrittr", "raster")
    
  }
  
  val <- DEoptim(objfun, lower = p(sim)$lower, upper = p(sim)$upper, control = do.call("DEoptim.control", control),
                 rasters = rasters, formula = p(sim)$formula, loci = loci, sizes = sizes, fireSense_SpreadFitRaster = fireSense_SpreadFitRaster) %>%
    `[[` ("optim") %>% `[[` ("bestmem")

  sim$fireSense_SpreadFitted <- 
    list(formula = p(sim)$formula,
         coef = val %>% setNames(nm = c("A", "B", "D", "G", if (attr(terms, "intercept")) "Intercept" else NULL, attr(terms, "term.labels"))))
  class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  
  if (!is.na(p(sim)$intervalRunModule))
    sim <- scheduleEvent(sim, time(sim) + p(sim)$intervalRunModule, "fireSense_SpreadFit", "run")
  
  sim
  
}


### template for save events
fireSense_SpreadFitSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
