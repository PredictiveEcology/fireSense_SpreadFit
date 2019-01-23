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
  reqdPkgs = list("DEoptim", "kSamples", "magrittr", "parallel", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NA,
                    desc = 'a formula describing the model to be fitted. Only 
                            the RHS needs to be provided.'),
    defineParameter(name = "data", class = "character", 
                    default = "dataFireSense_SpreadFit",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for
                            variables present in the model formula. `data`
                            objects can be RasterLayers, or RasterStacks for
                            time series (one layer per time unit). If variables
                            are not found in `data` objects, they are searched 
                            in the `simList` environment."),
    defineParameter(name = "fireLocations", class = "character", 
                    default = "fireLoc_FireSense_SpreadFit",
                    desc = "an object of class SpatialPointsDataFrame describing
                            fires starting locations, final sizes ('size'
                            column), and possibly the starting dates ('date'
                            column) if fires are to be spread at different time
                            intervals. If the 'date' column is not present, all
                            fires are assumed to have started at the same time
                            interval."),
    defineParameter(name = "lower", class = "numeric", default = NULL,
                    desc = "see `?DEoptim`. Lower bounds should be supplied for the lower bound, upper bound, slope, asymmetry, Then in the order they appear in the formula. Lower bounds for the parameters of the logistic function should be supplied first, while the model parameters need to be supplied after."),
    defineParameter(name = "upper", class = "numeric", default = NULL,
                    desc = "see `?DEoptim`."),
    defineParameter(name = "itermax", class = "integer", default = 500,
                    desc = "integer defining the maximum number of iterations 
                            allowed (DEoptim optimizer). Default is 500."),
    defineParameter(name = "nCores", class = "integer", default = 1,
                    desc = "non-negative integer. Defines the number of logical
                            cores to be used for parallel computation. The
                            default value is 1, which disables parallel 
                            computing."),
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
                            expressed in units of simulation time."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fireLoc_FireSense_SpreadFit",
      objectClass = "SpatialPointsDataFrame",
      sourceURL = NA_character_,
      desc = "An object of class SpatialPointsDataFrame describing fires
              starting locations, final sizes ('size' column), and possibly the
              starting dates ('date' column) if fires are to be spread at
              different time intervals. If the 'date' column is not present, all
              fires are assumed to have started at the same time interval."
    ),
    expectsInput(
      objectName = "dataFireSense_SpreadFit",
      objectClass = "RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "One or more objects of class 'RasterLayer' or 'RasterStack', in
              which to look for variables present in the model formula.
              RasterStacks can be used in cases where fires have started at
              different times and should not be spread at the same time 
              interval, but are still used to describe the same fire size
              distribution. In this case, the number of layers in the
              RasterStack should equal the number of distinct dates in column
              'date'."
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
  switch(
    eventType,
    init = { sim <- spreadFitInit(sim) },
    run = { sim <- spreadFitRun(sim) },
    save = { sim <- spreadFitSave(sim) },
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
  stopifnot(P(sim)$nCores >= 0)
  if (!is(P(sim)$formula, "formula")) stop(moduleName, "> The supplied object for the 'formula' parameter is not of class formula.")
  
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")
  
  if (!is.na(P(sim)$.saveInitialTime))
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
  
  invisible(sim)
} 

spreadFitRun <- function(sim)
{
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))
  endTime <- end(sim, timeunit(sim))
  
  ## Toolbox: set of functions used internally by spreadFitRun
    ## Raster predict function
    fireSense_SpreadFitRaster <- function(model, data, par)
    {
      model %>%
        model.matrix(data) %>%
        `%*%` (par) %>%
        drop
    }

  # Create a container to hold the data
  envData <- new.env(parent = envir(sim))
  on.exit(rm(envData))

  # Load inputs in the data container
  list2env(as.list(envir(sim)), envir = envData)
  
  ## Map the "fireLoc_FireSense_SpreadFit" parameter of this module to the "fireLoc_FireSense_SpreadFit" object in the simList environment
  envData[["fireLoc_FireSense_SpreadFit"]] <- envData[[P(sim)$fireLocations]]
  
  if (is.null(envData[["fireLoc_FireSense_SpreadFit"]]))
    stop(moduleName, "> '", P(sim)$fireLocations, "' not found in data objects or NULL.")
  
  if (!is(envData[["fireLoc_FireSense_SpreadFit"]], "SpatialPointsDataFrame"))
    stop(moduleName, "> '", P(sim)$fireLocations, "' is not a SpatialPointsDataFrame.")
  
  if (is.null(envData[["fireLoc_FireSense_SpreadFit"]][["size"]]))
    stop(moduleName, "> The SpatialPointsDataFrame '", P(sim)$fireLocations, "' must have a 'size' column.")
  
  sizes <- envData[["fireLoc_FireSense_SpreadFit"]][["size"]]
  
  if (is.empty.model(P(sim)$formula))
    stop(moduleName, "> The formula describes an empty model.")

  terms <- P(sim)$formula %>% terms.formula %>% delete.response ## If the formula has a LHS remove it
  allxy <- all.vars(terms)
  
  if (is.null(envData[["fireLoc_FireSense_SpreadFit"]][["date"]])) ## All fires started during the same time interval
  {
    for(x in P(sim)$data)
    {
      if (!is.null(sim[[x]]))
      {
        if (is(sim[[x]], "RasterStack"))
        {
          list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = envData)
        } 
        else if (is(sim[[x]], "RasterLayer")) 
        {
          next
        } 
        else stop(moduleName, "> '", x, "' is not a RasterLayer or a RasterStack.")
      }
    }

    missing <- !allxy %in% ls(envData, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    rasters <- mget(allxy, envir = envData, inherits = FALSE) %>% stack
    
    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- slot(
      raster::extract(rasters[[1L]], envData[["fireLoc_FireSense_SpreadFit"]], cellnumbers = TRUE, df = TRUE, sp = TRUE),
      "data"
    ) %>%
      with(., 
           {
             if (anyDuplicated(cells))
             {
               warning(moduleName, "> No more than one fire can start in a given pixel during",
                       " the same time interval, keeping the largest fire.", immediate. = TRUE)
               
               cells[-unlist(
                 lapply(
                   unique(cells[duplicated(cells)]), 
                   function(locus)
                   {
                     wh <- which(cells == locus)
                     sizes <- size[wh]
                     wh[-which.max(sizes)]
                   }
                 )
               )]
             }
             else cells
           }
      )
    
    objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster)
    {
      r <- predict(rasters, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
        calc(function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
      
      ## 10 replicates to better estimate the median
      (lapply(1:10, function(i) tabulate(SpaDES.tools::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
          do.call("rbind", .) %>%
          apply(2L, median) %>%
          list(sizes) %>%
          ad.test %>%
          `[[` ("ad")
      )[1L, 1L]
    }
  }
  else ## Fires started at different time intervals
  {
    for(x in P(sim)$data)
    {
      if (!is.null(sim[[x]]))
      {
        if (is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterLayer"))
        {
          next
        } 
        else 
          stop(moduleName, "> '", x, "' is not a RasterLayer or a RasterStack.")
      }
    }
    
    missing <- !allxy %in% ls(envData, all.names = TRUE)
    
    if (any(missing))
      stop(moduleName, "> '", paste(allxy[missing], collapse = "', '"), "' not found in data objects nor in the simList environment.")
    
    badClass <- !unlist(lapply(allxy, function(x) is(sim[[x]], "RasterLayer") || is(sim[[x]], "RasterStack")))
    
    if (any(badClass))
      stop(moduleName, "> '", paste(allxy[badClass], collapse = "', '"), "' does not match a RasterLayer or a RasterStack.")
    
    rasters <- mget(allxy, envir = envData, inherits = FALSE) %>%
      lapply(function(x) if( is(x, "RasterStack")) unstack(x) else list(x)) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)
    
    ## Get the corresponding loci from the raster sim$landscape for the fire locations
    loci <- slot(
      raster::extract(rasters[[1L]], envData[["fireLoc_FireSense_SpreadFit"]], cellnumbers = TRUE, df = TRUE, sp = TRUE),
      "data"
    )
    
    loci %<>% 
      split(envData[["fireLoc_FireSense_SpreadFit"]][["date"]]) %>% 
      lapply(na.omit) %>%
      lapply(
        function(x)
        {
          loci <- x[["cells"]]
          if (anyDuplicated(loci))
          {
            warning(moduleName, "> No more than one fire can start in a given pixel during",
                    "the same time interval, keeping the largest fire.", immediate. = TRUE)
            
            return(
              loci[-unlist(
                lapply(
                  unique(loci[duplicated(loci)]), 
                  function(locus)
                  {
                    wh <- which(loci == locus)
                    sizes <- x[wh, "size"]
                    wh[-which.max(sizes)]
                  }
                )
              )]
            )
          }
          else
            return (loci)
        }
      )
    
    sizes <- envData[["fireLoc_FireSense_SpreadFit"]][["size"]]
    
    objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster)
    {
      (rasters %>%
         mapply(FUN = function(x, loci)
         {
           r <- predict(x, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]) %>%
             calc(function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
           
           ## 10 replicates to better estimate the median
           lapply(1:10, function(i) tabulate(SpaDES.tools::spread(r, loci = loci, spreadProb = r, returnIndices = TRUE)[["id"]])) %>%
             do.call("rbind", .) %>%
             apply(2L, median)
         }, loci = loci, SIMPLIFY = FALSE) %>%
         unlist %>% list(sizes) %>% ad.test %>% `[[` ("ad"))[1L, 1L]
    }
  }
  
  control <- list(itermax = P(sim)$itermax, trace = P(sim)$trace)
  
  if (P(sim)$nCores > 1) 
  {
    cl <- parallel::makePSOCKcluster(names = P(sim)$nCores)
    on.exit(stopCluster(cl))
    parallel::clusterEvalQ(cl, for (i in c("kSamples", "magrittr", "raster")) library(i, character.only = TRUE))
    control$cluster <- cl
  }
  
  DE <- DEoptim(objfun, lower = P(sim)$lower, upper = P(sim)$upper, control = do.call("DEoptim.control", control),
                 rasters = rasters, formula = P(sim)$formula, loci = loci, sizes = sizes, fireSense_SpreadFitRaster = fireSense_SpreadFitRaster)
  
  val <- DE %>% `[[` ("optim") %>% `[[` ("bestmem")
  AD <- DE %>% `[[` ("optim") %>% `[[` ("bestval")
  
  sim$fireSense_SpreadFitted <- list(
    formula = P(sim)$formula,
    coef = val %>% setNames(nm = c("d", "a", "b", "g", if (attr(terms, "intercept")) "Intercept" else NULL, attr(terms, "term.labels"))),
    AD = AD
  )
  class(sim$fireSense_SpreadFitted) <- "fireSense_SpreadFit"
  
  if (!is.na(P(sim)$.runInterval)) # Assumes time only moves forward
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "run")
  
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
  
  if (!is.na(P(sim)$.saveInterval))
    sim <- scheduleEvent(sim, currentTime + P(sim)$.saveInterval, moduleName, "save", .last())
  
  invisible(sim)
}
