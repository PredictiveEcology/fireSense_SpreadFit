.objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster){
  # Optimization's objective function
  ad.test(list(unlist(mapply(FUN = function(x, loci){
            # How many of the parameters belong to the model?
            parsModel <- length(attributes(terms(formula))[["term.labels"]]) 
            # Making a data table of all raster values
            modelDT <- data.table(pixelID = 1:ncell(x), getValues(x))
            # removing NA's from outside studyArea
            modelDT <- modelDT[!is.na(weather), ]
            # filling up classes with 0.
            dtReplaceNAwith0(modelDT)
            modelDT <- modelDT[, Map("*", .SD, tail(x = par, n = parsModel)), .SDcols = names(modelDT)[-1]]
            # Not sure what is the correct way of doing this...
            dataEnv <- new.env()
            list2env(x = modelDT, envir = dataEnv)
            assign("predicted", value = eval(parse(text = paste0(formula)[2]), 
                                             envir = dataEnv), envir = dataEnv)
            predicted <- get("predicted", dataEnv)
            # Put the prediction back in the raster
            browser()
            
            if (isTRUE(median(predicted, na.rm = TRUE) > .245)) 
              return(1e100)
            
            r <- calc(predX, fun = function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
            r[] <- r[]
                    
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
