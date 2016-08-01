# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = "This module use Pattern Oriented Modelling (POM) to fit a fire landscape model where the probability of spread is heteregeneous, i.e. vary among pixels.",
  keywords = c("fire spread", "POM", "percolation"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  reqdPkgs = list("data.table", "DEoptim", "kSamples", "magrittr", "parallel", "raster"),
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
    defineParameter(name = "lower", class = "numeric", default = NA, desc = "see DEoptim."),
    defineParameter(name = "upper", class = "numeric", default = NA, desc = "see DEoptim."),
    defineParameter(name = "parallel", class = "logical", default = FALSE, desc = 'Should the optimization be parallelized ?'),
    defineParameter(name = "initialRunTime", class = "numeric", default = NA, desc = "optional. Simulation time at which to start this module. If omitted, start at start(sim)."),
    defineParameter(name = "intervalRunModule", class = "numeric", default = NA, desc = "optional. Interval in simulation time units between two module runs.")),
  inputObjects = data.frame(
    objectName = c("landscape", "firesLocations", "fireSense_SpreadFitStack"),
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

    sim <- sim$fireSense_SpreadFitInit(sim)

  } else if (eventType == "run") {
    
    sim <- sim$fireSense_SpreadFitRun(sim)
    
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
  
  sim <- scheduleEvent(sim, eventTime = if (is.na(p(sim)$initialRunTime)) start(sim) else p(sim)$initialRunTime, "fireSense_SpreadFit", "run")
  
  invisible(sim)
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
  
  if (!is.na(p(sim)$data[1]))
    lapply(p(sim)$data, function(x, envData) if (is.list(sim[[x]])) list2env(sim[[x]], envir = envData), envData = envData)
  
  ## In case there is a response in the formula remove it
  terms <- p(sim)$formula %>% terms.formula %>% delete.response
  
  ## Mapping variables names to data
  if (!is.na(p(sim)$mapping[1])) {
    
    for (i in 1:length(p(sim)$mapping)) {
      
      attr(terms, "term.labels") <- gsub(pattern = names(p(sim)$mapping[i]),
                                         replacement = p(sim)$mapping[i], x = attr(terms, "term.labels"))
      
    }
    
  }

  formula <- reformulate(attr(terms, "term.labels"), intercept = attr(terms, "intercept"))
  allVars <- all.vars(formula)
  
  if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterLayer"))))) {
    
    rasters <- mget(allVars, envir = envData, inherits = FALSE) %>% stack
    
    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(rasters, sim$firesLocations, cellnumbers = TRUE, df = TRUE)[,"cells"]
    
    if (anyDuplicated(loci)) stop("fireSense_SpreadFit> No more than one fire can start in a given pixel.")
    
    sizes <- sim$firesLocations$size
    
    objFun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster) {

      r <- predict(rasters, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
        calc(function(x) par[3L] + par[1L] / (1 + x^(-par[2L]))) ## Logistic 5p
      
      ## 10 replicates to better estimate the median
      (lapply(1:10, function(i) tabulate(SpaDES::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
        do.call("rbind", .) %>%
        apply(2L, median) %>%
        list(sizes) %>%
        ad.test %>%
        `[[` ("ad"))[1L, 1L]
      
    }
    
  } else if (all(unlist(lapply(allVars, function(x) is(envData[[x]], "RasterStack"))))) {

    rasters <- mget(allVars, envir = envData, inherits = FALSE) %>%
      lapply(unstack) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)

    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- raster::extract(rasters[[1L]], sim$firesLocations, cellnumbers = TRUE, df = TRUE)[,"cells"]

    if (anyDuplicated(loci)) stop("fireSense_SpreadFit> No more than one fire can start in a given pixel.")
    
    loci %<>% split(sim$firesLocations$date)
    sizes <- sim$firesLocations$size
    
    objFun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster) {

      (rasters %>%
        mapply(FUN = function(x, loci) {

          r <- predict(x, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
            calc(function(x) par[3L] + par[1L] / (1 + x^(-par[2L]))) ## Logistic 5p
          
          ## 10 replicates to better estimate the median
          lapply(1:10, function(i) tabulate(SpaDES::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
            do.call("rbind", .) %>%
            apply(2L, median)
        }, loci = loci, SIMPLIFY = FALSE) %>%
        unlist %>% list(sizes) %>% ad.test %>% `[[` ("ad"))[1L, 1L]

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
  
  
  control <- list(itermax = 2000)
  #control$trace <- TRUE ## debug
  
  if (p(sim)$parallel) {
    
    control$parallelType <- 1
    control$packages <- c("data.table", "kSamples", "magrittr", "raster")
    
  }
  
  val <- DEoptim(objFun, lower = p(sim)$lower, upper = p(sim)$upper, control = do.call("DEoptim.control", control),
                 rasters = rasters, formula = formula, loci = loci, sizes = sizes, fireSense_SpreadFitRaster = fireSense_SpreadFitRaster) %>%
    `[[` ("optim") %>% `[[` ("bestmem")
  
  sim$fireSense_SpreadFitted <- val %>% as.list %>% setNames(nm = c("A", "B", "D", "G", if (attr(terms, "intercept")) "Intercept" else NULL, attr(terms, "term.labels")))
  class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  
  if (!is.na(p(sim)$intervalRunModule))
    sim <- scheduleEvent(sim, time(sim) + p(sim)$intervalRunModule, "fireSense_SpreadFit", "run")
  
  invisible(sim)
}
