.objfun <- function(par, rasters, formula, loci, sizes, verbose){ #fireSense_SpreadFitRaster
  # Optimization's objective function
  ad.test(list(unlist(mapply(FUN = function(x, loci){
    #removed this fun. Was not working: fireSense_SpreadFitRaster
    # fireSense_SpreadFitRaster <- function(model, data, par){
    #   browser()
    #   drop(model.matrix(model, data) %*% par)
    # }
    # 
    print("browser for Eliot")
    browser()
            # How many of the parameters belong to the model?
            parsModel <- length(attributes(terms(formula))[["term.labels"]]) 
            # Making a data table of all raster values
            modelDT <- data.table(pixelID = 1:ncell(x), getValues(x))
            # removing NA's from outside studyArea
            modelDT <- modelDT[!is.na(weather), ]
            # filling up classes with 0.
            dtReplaceNAwith0(modelDT)
            pixelID <- modelDT$pixelID
            # Not sure what is the correct way of doing this...
            
            # Way 1: fit a model with the formula and data
            # mod <- lm(formula = formula, data = modelDT[, -1]) # doesn't work
            
            # Way 2: Evaluate the formula, but this only works if its a simple additive model
            modelDT <- modelDT[, Map("*", .SD, tail(x = par, n = parsModel)), .SDcols = names(modelDT)[-1]]
            dataEnv <- new.env()
            list2env(x = modelDT, envir = dataEnv)
            assign("predicted", value = eval(parse(text = paste0(formula)[2]),
                                             envir = dataEnv), envir = dataEnv)
            predicted <- get("predicted", dataEnv)
            
            # Test if this is worth testing
            if (isTRUE(median(predicted, na.rm = TRUE) > .245)) 
              return(1e100)
            
            # Put the prediction back in the raster
            predDT <- data.table(pixelID = pixelID, pred = predicted)
            mergedDT <- merge(data.table(pixelID = 1:ncell(x)), predDT, all.x = TRUE, by = "pixelID")
            predRas <- raster::setValues(x = x[[1]][[1]], values = mergedDT$pred)
            names(predRas) <- "spreadProb"
            
            r <- calc(predRas, fun = function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
            if (verbose)
              print(paste0("spreadProb raster: median = ", median(getValues(r), na.rm = TRUE)))
          
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
            
            # spreadState[ , fire_id := .GRP, by = "initialPixels"] # Add an fire_id column
            spreadState[ , fire_id := .GRP, by = "initialLocus"] # Add an fire_id column
            
            tabulate(spreadState[["fire_id"]]) # Here tabulate() is equivalent to table() but faster
            # tabulate(spreadState[["initialLocus"]]) # Here tabulate() is equivalent to table() but faster
            
            # # 10 replicates to better estimate the median
            # apply(
            #   do.call(
            #     "rbind",
            #     lapply(
            #       1:10,
            #       function(i)
            #       {
            #         spreadState <- SpaDES.tools::spread2(
            #           landscape = r,
            #           start = loci,
            #           spreadProb = r,
            #           asRaster = FALSE
            #         )
            # 
            #         spreadState[ , fire_id := .GRP, by = "initialPixels"] # Add an fire_id column
            # 
            #         tabulate(spreadState[["fire_id"]])
            #       }
            #     )
            #   ),
            #   2L,
            #   median
            # )
          },
          rasters,
          loci = loci, 
          SIMPLIFY = FALSE
        )
      ),
      sizes
    )
  )[["ad"]][1L, 1L]
}
