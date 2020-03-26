.objfun <- function(par, rasters, formula, loci, sizes, bufferedRealHistoricalFiresList, verbose){ #fireSense_SpreadFitRaster
  # Optimization's objective function
 results <- mapply(FUN = function(x, bufferedRealHistoricalFires, loci, nonNA, wADtest = 1){
    
            
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
              loci = rep(loci, times = 10), 
              spreadProb = r,
              returnIndices = TRUE,
              allowOverlap = TRUE
            )
            
            spreadState[ , fire_id := .GRP, by = "initialLocus"] # Add an fire_id column
            
            fireSizes <- tabulate(spreadState[["fire_id"]]) # Here tabulate() is equivalent to table() but faster
            SNLL <- -sum(dbinom(prob = ras[nonNA], size = N, x = bufferedRealHistoricalFires[nonNA], log = TRUE), na.rm = TRUE) # Sum of the negative log likelihood
            internalResults <- list(fireSizes = fireSizes, SNLL = SNLL)
            
            return(internalResults)
          },
          rasters,
          loci = loci, 
          SIMPLIFY = FALSE
        )
  results <- purrr::transpose(results)
 
  adTest <- ad.test(unlist(results$fireSizes), sizes)[["ad"]][1L, 1L]
  SNLLTest <- sum(unlist(results$SNLL))
  adTest <- rescale(adTest)
  SNLLTest <- rescale(SNLLTest)
  objFunRes <- wADtest * adTest + SNLLTest # wADtest is the weight for the AD test
  # Figure out what we want from these. This is potentially correct (i.e. we want the smallest ad.test and the smallest SNLL)
  return(objFunRes)
}
