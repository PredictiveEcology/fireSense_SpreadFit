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
                                  "module will convert those to 0's internally"))
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
  
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  mod_env <- new.env(parent = emptyenv()) # 'emptyenv()' Avoid memory leak and C recursive problem
  ## Map the "fireAttributesFireSense_SpreadFit" parameter of this module to the "fireAttributesFireSense_SpreadFit" object in the module environment
  assign("fireAttributesFireSense_SpreadFit", value = sim[[P(sim)$fireAttributes]], envir = mod_env)
  
  .doDataChecks(env = mod_env, attribs = P(sim)$fireAttributes, fml = P(sim)$formula)
  
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

    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    rasters <- mget(allxy, envir = mod_env, inherits = FALSE) %>% stack
    
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
      # spreadState <- SpaDES.tools::spread2(
      #   landscape = r,
      #   start = loci, 
      #   spreadProb = r,
      #   asRaster = FALSE
      # )
      
      #spreadState[ , fire_id := .GRP, by = "initialPixels"] # Add an fire_id column
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
       
      # # 10 replicates to better estimate the median
      # ad.test(
      #   list(
      #     apply(
      #       do.call(
      #         "rbind",
      #         lapply(
      #           1:10, 
      #           function(i) 
      #           {
      #             spreadState <- SpaDES.tools::spread2(
      #               landscape = r,
      #               start = loci, 
      #               spreadProb = r,
      #               asRaster = FALSE
      #             )
      #             
      #             spreadState[ , fire_id := .GRP, by = "initialPixels"] # Add an fire_id column
      #             
      #             tabulate(spreadState[["fire_id"]])
      #           }
      #         )
      #       ),
      #       2L,
      #       median
      #     ),
      #     sizes
      #   )
      # )[["ad"]][1,1]
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

    rasters <- mget(allxy, envir = mod_env, inherits = FALSE) %>%
      lapply(function(x) if( is(x, "RasterStack") || is(x, "RasterBrick") ) unstack(x) else list(x)) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)

    ## Get loci from the raster sim$landscape for the fire locations
    lociDF <- raster::extract(x = rasters[[1L]], y = mod_env[["fireAttributesFireSense_SpreadFit"]],
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
                                    sizes = unlist(eapply(environment(), FUN = function(x) x[["sizes"]])))),
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
  
  print("browser line 497: Check fireSense_SpreadFitRaster, loci, sizes... all params")
  browser() # Check fireSense_SpreadFitRaster, loci, sizes... all params
  DE <- DEoptim(
    .objfun, 
    lower = P(sim)$lower,
    upper = P(sim)$upper,
    control = do.call("DEoptim.control", control),
    rasters = rasters, 
    formula = P(sim)$formula, 
    loci = loci,
    sizes = sizes,
    fireSense_SpreadFitRaster = fireSense_SpreadFitRaster
  )
  
  val <- DE %>% `[[` ("optim") %>% `[[` ("bestmem")
  AD <- DE$optim$bestval
  
  sim$fireSense_SpreadFitted <- list(
    formula = P(sim)$formula,
    coef = setNames(
      val,
      nm = c(
        "d", "a", "b", "g", 
        if (attr(terms, "intercept")) "Intercept" else NULL,
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
