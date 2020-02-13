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
  reqdPkgs = list("DEoptim", "kSamples", "magrittr", "parallel", "PtProcess", "raster"),
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
                            'RasterLayers', 'RasterStacks' or 'RasterBricks'. 
                            'RasterStacks' and 'RasterBricks' can be used in cases
                            where fires have started at different times and should
                            not be spread at the same time interval, but are still
                            used to describe the same fire size distribution. In 
                            this case, the number of layers in the 'RasterStack' 
                            should equal the number of distinct times in column 'time'."),
    defineParameter(name = "lower", class = "numeric", default = NULL,
                    desc = "see `?DEoptim`. Lower bounds should be supplied for the lower
                            bound, upper bound, slope, asymmetry, Then in the order they
                            appear in the formula. Lower bounds for the parameters of the
                            logistic function should be supplied first, while the model
                            parameters need to be supplied after."),
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
    defineParameter(name = "clusterEvalExpr", class = "expression", default = expression(),
                    desc = "optional. An expression to evaluate on each cluster node. 
                            Ignored when parallel computing is disabled."),
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
      objectName = "fireAttributesFireSense_SpreadFit",
      objectClass = "SpatialPointsDataFrame",
      sourceURL = NA_character_,
      desc = "An object of class SpatialPointsDataFrame describing fires
              starting locations, final sizes ('size' column), and possibly the
              starting times ('time' column) if fires are to be spread at
              different time intervals. If the 'time' column is not present, all
              fires are assumed to have started at the same time interval."
    ),
    expectsInput(
      objectName = "historicalBurnMapFireSense_SpreadFit",
      objectClass = "RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "An object of class 'RasterLayer', 'RasterStack' or 'RasterBrick' 
              describing which pixels did burn (coded as 1) or not (coded as 0)
              historically. RasterStacks and RasterBricks can be used in cases
              where fires have started at different times and should not be 
              spread at the same time interval, but are still used to describe
              the same fire size distribution."
    ),
    expectsInput(
      objectName = "dataFireSense_SpreadFit",
      objectClass = "RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "One or more objects of class 'RasterLayer', 'RasterStack'
              or 'RasterBrick', in which to look for variables present 
              in the model formula. 'RasterStacks' and 'RasterBricks' can 
              be used in cases where fires have started at different 
              times and should not be spread at the same time interval,
              but are still used to describe the same fire size 
              distribution. In this case, the number of layers in the 
              'RasterStack' should equal the number of distinct times in column 'time'."
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_SpreadFitted",
    objectClass = "fireSense_SpreadFit",
    desc = "A fitted model object of class 'fireSense_SpreadFit'."
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
  stopifnot(P(sim)$nCores >= 0)
  if (!is(P(sim)$formula, "formula"))
    stop(moduleName, "> The supplied object for the 'formula' parameter is not of class formula.")
  
  invisible(sim)
} 

spreadFitRun <- function(sim)
{
  moduleName <- current(sim)$moduleName
  
  ## Toolbox: set of functions used internally by spreadFitRun
  chk_duplicatedStartPixels <- function(cells, size, beta, theta)
  {
    if (anyDuplicated(cells))
    {
      warning(moduleName, "> No more than one fire can start in a given pixel during",
              " the same time interval, keeping the largest fire.", immediate. = TRUE)
      
      to_rm <- unlist(
        lapply(
          unique(cells[duplicated(cells)]), 
          function(locus)
          {
            wh <- which(cells == locus)
            sizes <- size[wh]
            wh[-base::which.max(sizes)]
          }
        )
      )
      
      list(loci = cells[-to_rm], sizes = size[-to_rm])
    }
    else list(loci = cells, sizes = sizes) 
  }
  
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  if (is.null(sim[["fireAttributesFireSense_SpreadFit"]]))
    stop(moduleName, "> 'fireAttributesFireSense_SpreadFit' not found in data objects or NULL.")
  
  if (!is(sim[["fireAttributesFireSense_SpreadFit"]], "SpatialPointsDataFrame"))
    stop(moduleName, "> 'fireAttributesFireSense_SpreadFit' is not a SpatialPointsDataFrame.")
  
  if (is.null(sim[["fireAttributesFireSense_SpreadFit"]][["size"]]))
    stop(moduleName, "> 'fireAttributesFireSense_SpreadFit' must have a 'size' column.")
  
  sizes <- sim[["fireAttributesFireSense_SpreadFit"]][["size"]]
  
  
  if (is.null(sim[["historicalBurnMapFireSense_SpreadFit"]]))
    stop(moduleName, "> 'fireAttributesFireSense_SpreadFit' not found in data objects or NULL.")
  
  if (!is(sim[["historicalBurnMapFireSense_SpreadFit"]], "RasterLayer") &&
      !is(sim[["historicalBurnMapFireSense_SpreadFit"]], "RasterStack") && 
      !is(sim[["historicalBurnMapFireSense_SpreadFit"]], "RasterBrick"))
    stop(moduleName, "> 'historicalBurnMapFireSense_SpreadFit' is not a 'RasterLayer', 'RasterStack' or 'RasterBrick'")
  
  
  if (is.empty.model(P(sim)$formula))
    stop(moduleName, "> The formula describes an empty model.")
  
  terms <- P(sim)$formula %>% terms.formula %>% delete.response ## If the formula has a LHS remove it
  allxy <- all.vars(terms)
  
  if (is.null(sim[["fireAttributesFireSense_SpreadFit"]][["time"]])) ## All fires started during the same time interval
  {
    for(x in P(sim)$data)
    {
      if (!is.null(sim[[x]]))
      {
        if (is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick"))
        {
          list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = mod)
        } 
        else if (is(sim[[x]], "RasterLayer")) 
        {
          mod[[x]] <- sim[[x]]
        } 
        else stop(moduleName, "> '", x, "' is not a RasterLayer, a RasterStack or a RasterBrick.")
      }
    }
    
    missing <- !allxy %in% ls(mod, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    rasters <- mget(allxy, envir = mod, inherits = FALSE) %>% stack
    
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
      r <- predict(rasters, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[3:length(par)]) %>%
        calc(function(x) .1 + .4 / (1 + x^(-par[1L])) ^ par[2L]) ## 5-parameters logistic
      
      spreadState <- SpaDES.tools::spread2(
        landscape = r,
        start = loci, 
        spreadProb = r,
        asRaster = FALSE,
        maxSize = 1e5
      )
      
      spreadState[ , fire_id := .GRP, by = "initialPixels"] # Add an fire_id column
      
      
      
      ad.test(
        list(
          tabulate( # Here tabulate() is equivalent to table() but faster
            spreadState[["fire_id"]]
          ),
          sizes
        )
      )[["ad"]][1,1]
      
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
          mod[[x]] <- sim[[x]]
        } 
        else 
          stop(moduleName, "> '", x, "' is not a RasterLayer, a RasterStack or a RasterBrick.")
      }
    }
    
    missing <- !allxy %in% ls(mod, all.names = TRUE)
    
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
    
    # Each stack or brick describes for each variable the spatio-temporal variations for all unique times,
    # for example years. We want to simulate the spread of fires under the conditions in which they burned. 
    # Since fires burned in different years, the variables must be described year after year. We fetch one
    # year at a time from the stack(s) describing the variable(s) and create annual stacks (stored as 
    # data.tables) that contained all the data.
    times_stacks <- mget(allxy, envir = mod, inherits = FALSE) %>%
      lapply(function(x) if( is(x, "RasterStack") || is(x, "RasterBrick") ) unstack(x) else list(x)) %>%
      c(list(FUN = function(...) stack(list(...)), SIMPLIFY = FALSE)) %>%
      do.call("mapply", args = .)
    
    # For each year, calculate model matrices in advance
    mms <- lapply(
      times_stacks,
      function(x)
      {
        unname(
          model.matrix(
            P(sim)$formula,
            model.frame(
              P(sim)$formula,
              data = as.data.frame(x[]), 
              na.action = NULL
            )
          )
        )
      }
    )
    
    hist_burn_maps <- lapply(
      unstack(sim[["historicalBurnMapFireSense_SpreadFit"]]), 
      function(x) x[] == 0 # Did the pixel burn? 0 = no, 1 = yes
    )
    
    weight_burn_historically <- sum(!is.na(unlist(hist_burn_maps))) / sum(!unlist(hist_burn_maps), na.rm = TRUE)
    
    unique_times <- sort(unique(sim[["fireAttributesFireSense_SpreadFit"]][["time"]]))
    
    list2env(
      with(
        setNames(
          lapply(
            mapply(
              unique_times,
              times_stacks,
              FUN = function(unique_time_i, r)
              {
                slot(
                  raster::extract(
                    r,
                    sim[["fireAttributesFireSense_SpreadFit"]][sim[["fireAttributesFireSense_SpreadFit"]][["time"]] == unique_time_i, ],
                    cellnumbers = TRUE,
                    df = TRUE,
                    sp = TRUE
                  ),
                  "data"
                )
              },
              SIMPLIFY = FALSE
            ),
            function(x) with(x, chk_duplicatedStartPixels(cells, size))
          ),
          nm = unique_times
        ),
        list(
          loci = eapply(environment(), FUN = function(x) sort(x[["loci"]])) #,
          # sizes = eapply(environment(), FUN = function(x) x[["sizes"]][order(x[["loci"]])])
        )
      ),
      envir = environment()
    )
    
    # browser()
    
    objfun <- function(par, mms, landscape, loci, hist_burn_maps)
    {
      # browser()
      
      # library(profvis)
      
      # p <- profvis({
      sum(
        unlist(
          mapply(
            FUN = function(x, loci, hist_burn_map)
            {
              # browser()
              
              spreadProb <- .1 + .2 / ( 1 + drop(x %*% par[3:length(par)]) ^ (-par[1L]) ) ^ par[2L]
              
              burnmap <- numeric(ncell(landscape))
              
              if (median(spreadProb, na.rm = TRUE) > .245) return(1e100)
              
              for (i in 1:100)
              {
                spreadState <- SpaDES.tools::spread(
                  landscape = landscape,
                  loci = loci, 
                  returnIndices = TRUE,
                  spreadProb = spreadProb,
                  maxSize = 1.2e5
                )
                
                burnmap[spreadState[["indices"]]] <- burnmap[spreadState[["indices"]]] + 1
              }
              
              # Calc cross-entropy
              burnmap <- burnmap / 100
              
              # browser()
              
              sum(
                -log(
                  c(
                    burnmap[hist_burn_map],
                    (1 - burnmap[!hist_burn_map]) ^ weight_burn_historically[2]
                  ) + .Machine$double.eps
                ),
                na.rm = TRUE
              )
            },
            mms,
            loci = loci,
            hist_burn_map = hist_burn_maps,
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
          )
        )
      )
      
      # })
      
      # browser()
      
      # htmlwidgets::saveWidget(p, "D:/Jean/profile_spread_newcode.html")
    }
  }
  
  control <- list(itermax = P(sim)$itermax, trace = P(sim)$trace)
  
  if (P(sim)$nCores > 1) 
  {
    cl <- parallel::makePSOCKcluster(names = P(sim)$nCores)
    on.exit(stopCluster(cl))
    parallel::clusterEvalQ(cl, for (i in c("kSamples", "magrittr", "PtProcess", "raster")) library(i, character.only = TRUE))
    parallel::clusterEvalQ(cl, options(raster.maxmemoy = 4e10))
    parallel::clusterCall(cl, eval, P(sim)$clusterEvalExpr, env = .GlobalEnv)
    control$cluster <- cl
  }
  
  # browser()
  
  DE <- DEoptim(
    objfun, 
    lower = P(sim)$lower,
    upper = P(sim)$upper,
    control = do.call("DEoptim.control", control),
    landscape = raster(sim[["historicalBurnMapFireSense_SpreadFit"]]), # Template raster
    mm = mms,
    loci = loci,
    hist_burn_maps = hist_burn_maps
  )
  
  val <- DE %>% `[[` ("optim") %>% `[[` ("bestmem")
  AD <- DE %>% `[[` ("optim") %>% `[[` ("bestval")
  
  sim$fireSense_SpreadFitted <- list(
    formula = P(sim)$formula,
    coef = setNames(
      val,
      nm = c(
        "b", "g", 
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
