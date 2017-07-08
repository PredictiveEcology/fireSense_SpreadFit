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
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "kSamples", "magrittr", "parallel", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NULL,
                    desc = 'an object of class formula: a symbolic description 
                            of the model to be fitted. Only the RHS needs to be 
                            provided.'),
    defineParameter(name = "lower", class = "numeric", default = NULL, desc = "see `?DEoptim`."),
    defineParameter(name = "upper", class = "numeric", default = NULL, desc = "see `?DEoptim`."),
    defineParameter(name = "data", class = "character", default = "dataFireSense_SpreadFit",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for
                            variables in the model. `data` objects can be
                            RasterLayers or RasterStacks (for time series), or
                            named list of either several RasterLayers or several
                            RasterStacks, but RasterLayers and RasterStack can not
                            be mixed together. If omitted, or if variables are
                            not found in `data` objects, variables are searched
                            in the `simList` environment."),
    defineParameter(name = "fires", class = "SpatialPointsDataFrame", default = "fires",
                    desc = "a character vector indicating the name of a 
                            SpatialPointsDataFrame describing fires starting
                            dates and locations, and final sizes. The 
                            SpatialPointsDataFrame must have a column called
                            'date' and one called 'size'."),
    defineParameter(name = "trace", class = "numeric", default = 0,
                    desc = "non-negative `integer`. If > 0, tracing information
                            on the progress of the optimization are printed
                            every `trace` iteration. Default is 0, which turns off
                            tracing."),
    defineParameter(name = "nCores", class = "integer", default = 0, 
                    desc = "non-negative `integer`. Defines the number of 
                            logical cores to be used for parallel computation.
                            The default value is 0, which disables parallel
                            computing."),
    defineParameter(name = "initialRunTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = "intervalRunModule", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time.")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fires",
      objectClass = "SpatialPointsDataFrame",
      sourceURL = NA_character_,
      desc = "An object of class SpatialPointsDataFrame describing fires
              starting dates and locations, and final sizes. It must have a 
              'date' column and a 'size' column."
    ),
    expectsInput(
      objectName = "dataFireSense_SpreadFit",
      objectClass = "RasterLayer, RasterStack, list",
      sourceURL = NA_character_,
      desc = "An object of class RasterLayer or RasterStack or named lists of 
              RasterLayers or RasterStacks, in which to look for variables with
              which to predict. RasterLayers and RasterStacks can not be mixed
              together. RasterStacks can be used in cases where fires have
              started at different years and should not be spread in the same
              year of simulation, but are still used to describe a single fire
              size distribution."
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
  
  # Checking parameters
  stopifnot(P(sim)$trace >= 0)
  stopifnot(P(sim)$nCores >= 0)
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$initialRunTime, current(sim)$moduleName, "run")
  sim
  
} 

fireSense_SpreadFitRun <- function(sim) {
  
  moduleName <- current(sim)$moduleName

  ## Toolbox: set of functions used internally by fireSense_SpreadFitRun
    ## Raster predict function
    fireSense_SpreadFitRaster <- function(model, data, par) {
      
      model %>%
        model.matrix(data) %>%
        `%*%` (par) %>%
        drop
      
    }

  # Create a container to hold the data
  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))

  # Load data in the container
  list2env(as.list(envir(sim)), envir = envData)
  
  lapply(P(sim)$data, function(x, envData) {
    
    if (!is.null(sim[[x]])) {
      
      if (is.list(sim[[x]]) && !is.null(names(sim[[x]]))) {
        
        list2env(sim[[x]], envir = envData)
        
      } else stop(paste0(moduleName, "> '", x, "' is not a named list."))
      
    }
    
  }, envData = envData)

  ## Mapping required object "fires" to one in the simList environment
  envData[["fires"]] <- envData[[P(sim)$fires]]

  if (is.empty.model(P(sim)$formula))
    stop(paste0(moduleName, "> The formula describes an empty model."))

  terms <- P(sim)$formula %>% terms.formula %>% delete.response ## If the formula has a LHS remove it
  allxy <- all.vars(terms)
  
  if (all(unlist(lapply(allxy, function(x) is(envData[[x]], "RasterStack"))))) {

    rasters <- mget(allxy, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)

    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(rasters[[1L]], envData[["fires"]], cellnumbers = TRUE, df = TRUE)[["cells"]]

    loci %<>% split(envData[["fires"]][["date"]]) %>% lapply(na.omit)
    lapply(loci, function(x) if (anyDuplicated(x)) stop(paste0(moduleName, "> No more than one fire can start in a given pixel.")) )
    sizes <- envData[["fires"]][["size"]]

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
    loci <- raster::extract(rasters, envData[["fires"]], cellnumbers = TRUE, df = TRUE)[["cells"]]
    
    if (anyDuplicated(loci)) stop(paste0(moduleName, "> No more than one fire can start in a given pixel."))
    
    sizes <- envData[["fires"]][["size"]]
    
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
      stop(paste0(moduleName, "> Variable '", allxy[which(!exist)[1L]], "' not found."))
    } else if (any(class)) {
      stop(paste0(moduleName, "> Data objects are not of the same class (e.g. data.frames)."))
    } else {
      stop(paste0(moduleName, "> Variable '", allxy[which(!class)[1L]], "' does not match a RasterLayer or a RasterStack."))
    }
  }
  
  
  control <- list(itermax = 2000, trace = P(sim)$trace)
  
  if (P(sim)$parallel) {
    
    packages <- c("data.table", "kSamples", "magrittr", "raster")
    
    cl <- parallel::makeCluster(P(sim)$parallel)
    clusterEvalQ(cl, for (i in packages) library(i, character.only = TRUE))
    control$cluster <- cl
    
  }
  
  val <- DEoptim(objfun, lower = P(sim)$lower, upper = P(sim)$upper, control = do.call("DEoptim.control", control),
                 rasters = rasters, formula = P(sim)$formula, loci = loci, sizes = sizes, fireSense_SpreadFitRaster = fireSense_SpreadFitRaster) %>%
    `[[` ("optim") %>% `[[` ("bestmem")

  sim$fireSense_SpreadFitted <- 
    list(formula = P(sim)$formula,
         coef = val %>% setNames(nm = c("A", "B", "D", "G", if (attr(terms, "intercept")) "Intercept" else NULL, attr(terms, "term.labels"))))
  class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  
  if (!is.na(P(sim)$intervalRunModule))
    sim <- scheduleEvent(sim, time(sim) + P(sim)$intervalRunModule, moduleName, "run")
  
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
