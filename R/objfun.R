.objfun <- function(par, landscape, rastersDT, formula, #loci, sizes, 
                    historicalFires,
                    fireBufferedListDT,
                    wADtest = 1,
                    #bufferedRealHistoricalFiresList, 
                    verbose){ #fireSense_SpreadFitRaster
  # Optimization's objective function
  lapply(historicalFires, setDT)
  lapply(rastersDT, setDT)
  lapply(fireBufferedListDT, setDT)
  dtThreadsOrig <- data.table::setDTthreads(1)
  colsToUse <- attributes(terms(formula))[["term.labels"]]
  # How many of the parameters belong to the model?
  parsModel <- length(colsToUse) 
  ncells <- ncell(landscape)
  r <- raster(landscape)
  years <- as.character(names(rastersDT))
  names(years) <- years
  cells <- integer(ncells)
  Nreps <- 10
  
  results <- Map(
    yr = years, annualDT = rastersDT, 
    annualFires = historicalFires, 
    annualFireBufferedDT = fireBufferedListDT,
    MoreArgs = list(par = par, parsModel = parsModel),
    function(yr, annualDT, par, parsModel, annualFires, annualFireBufferedDT) {
      # needed because data.table objects were recovered from disk
      try(setDT(annualDT))
      # matrix multiplication
      set(annualDT, NULL, "pred", as.matrix(annualDT[,..colsToUse]) %*% tail(x = par, n = parsModel))
      # logistic multiplication
      set(annualDT, NULL, "spreadProb", logistic4p(annualDT$pred, par[1:4])) ## 5-parameters logistic
      actualBurnSP <- annualDT[annualFireBufferedDT, on = "pixelID"]
      medSP <- median(actualBurnSP[, mean(spreadProb, na.rm = TRUE)], na.rm = TRUE)
      if (medSP <= 0.27) {
      if (verbose) {
        message(paste0("-- year: ",yr, ", spreadProb raster: median in buffered pixels = ", 
                             round(medSP, 3)))
      }
      cells[as.integer(annualDT$pixelID)] <- annualDT$spreadProb
      maxSizes <- rep(annualFires$size, times = Nreps) * 2
      spreadState <- SpaDES.tools::spread(
        landscape = r,
        maxSize = maxSizes,
        loci = rep(annualFires$cells, times = Nreps), 
        spreadProb = cells,
        returnIndices = TRUE,
        allowOverlap = TRUE
      )
      fireSizes <- tabulate(spreadState[["id"]]) # Here tabulate() is equivalent to table() but faster
      burnedProb <- spreadState[, .N, by = "indices"]
      setnames(burnedProb, "indices", "pixelID")
      out <- burnedProb[annualFireBufferedDT, on = "pixelID"]
      out[is.na(N), N := 0]
      
      # THis is a work around for cases where "initial pixels" are not actually burned in 
      #   the polygon database
      out[pixelID %in% annualFires$cells, buffer := 1]
      # Add a very small number so that no pixel has exactly zero probability -- creating Inf
      SNLL <- -sum(dbinom(prob = pmin(out$N/Nreps + 0.001, 0.99), size = 1, 
                          x = out$buffer, 
                          log = TRUE
      ), na.rm = TRUE) # Sum of the negative log likelihood
      } else {
        SNLL <- 1e7
        fireSizes <- numeric()
      }
      
      list(fireSizes = fireSizes, SNLL = SNLL)
    })
  results <- purrr::transpose(results)
  historicalFiresTr <- purrr::transpose(historicalFires)
  
  adTest <- ad.test(unlist(results$fireSizes), unlist(historicalFiresTr$size))[["ad"]][1L, 1L]
  SNLLTest <- sum(unlist(results$SNLL))
  objFunRes <- adTest + SNLLTest/1e3 # wADtest is the weight for the AD test
  # Figure out what we want from these. This is potentially correct (i.e. we want the smallest ad.test and the smallest SNLL)
  return(objFunRes)
  
  # results <- mapply(FUN = function(x, bufferedRealHistoricalFires, loci, nonNA, wADtest = 1){
  #   
  #   
  #   print("browser for Eliot")
  #   browser()
  #   # How many of the parameters belong to the model?
  #   parsModel <- length(attributes(terms(formula))[["term.labels"]]) 
  #   # Making a data table of all raster values
  #   modelDT <- data.table(pixelID = 1:ncell(x), getValues(x))
  #   # removing NA's from outside studyArea
  #   modelDT <- modelDT[!is.na(weather), ]
  #   # filling up classes with 0.
  #   dtReplaceNAwith0(modelDT)
  #   pixelID <- modelDT$pixelID
  #   
  #   # Way 2: Evaluate the formula, but this only works if its a simple additive model
  #   modelDT <- modelDT[, Map("*", .SD, tail(x = par, n = parsModel)), .SDcols = names(modelDT)[-1]]
  #   dataEnv <- new.env()
  #   list2env(x = modelDT, envir = dataEnv)
  #   assign("predicted", value = eval(parse(text = paste0(formula)[2]),
  #                                    envir = dataEnv), envir = dataEnv)
  #   predicted <- get("predicted", dataEnv)
  #   
  #   # Test if this is worth testing
  #   if (isTRUE(median(predicted, na.rm = TRUE) > .245)) 
  #     return(1e100)
  #   
  #   # Put the prediction back in the raster
  #   predDT <- data.table(pixelID = pixelID, pred = predicted)
  #   mergedDT <- merge(data.table(pixelID = 1:ncell(x)), predDT, all.x = TRUE, by = "pixelID")
  #   predRas <- raster::setValues(x = x[[1]][[1]], values = mergedDT$pred)
  #   names(predRas) <- "spreadProb"
  #   
  #   r <- calc(predRas, fun = function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]) ## 5-parameters logistic
  #   if (verbose)
  #     print(paste0("spreadProb raster: median = ", median(getValues(r), na.rm = TRUE)))
  #   
  #   spreadState <- SpaDES.tools::spread(
  #     landscape = r,
  #     loci = rep(loci, times = 10), 
  #     spreadProb = r,
  #     returnIndices = TRUE,
  #     allowOverlap = TRUE
  #   )
  #   
  #   spreadState[ , fire_id := .GRP, by = "initialLocus"] # Add an fire_id column
  #   
  #   fireSizes <- tabulate(spreadState[["fire_id"]]) # Here tabulate() is equivalent to table() but faster
  #   SNLL <- -sum(dbinom(prob = ras[nonNA], size = N, x = bufferedRealHistoricalFires[nonNA], log = TRUE), na.rm = TRUE) # Sum of the negative log likelihood
  #   internalResults <- list(fireSizes = fireSizes, SNLL = SNLL)
  #   
  #   return(internalResults)
  # },
  # x = rasters,
  # bufferedRealHistoricalFires = bufferedRealHistoricalFiresList,
  # loci = loci, 
  # SIMPLIFY = FALSE
  # )
  # results <- purrr::transpose(results)
  # 
  # adTest <- ad.test(unlist(results$fireSizes), sizes)[["ad"]][1L, 1L]
  # SNLLTest <- sum(unlist(results$SNLL))
  # adTest <- rescale(adTest)
  # SNLLTest <- rescale(SNLLTest)
  # objFunRes <- wADtest * adTest + SNLLTest # wADtest is the weight for the AD test
  # # Figure out what we want from these. This is potentially correct (i.e. we want the smallest ad.test and the smallest SNLL)
  # return(objFunRes)
}

logistic4p <- function(x, par) {
  par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]
}
