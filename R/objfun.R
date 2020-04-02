
logistic4p <- function(x, par) {
  par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L]
}

.objfun <- function(par, 
                    landscape, 
                    annualDTx1000, 
                    nonAnnualDTx1000,
                    pixelIndices,
                    formula, 
                    historicalFires,
                    fireBufferedListDT,
                    wADtest = 1, # not used yet
                    verbose = TRUE){ 
  
  # Optimization's objective function
  data.table::setDTthreads(1)
  if (missing(landscape))
    landscape <- get("landscape", envir = .GlobalEnv)
  if (missing(annualDTx1000))
    annualDTx1000 <- get("annualDTx1000", envir = .GlobalEnv)
  if (missing(nonAnnualDTx1000))
    nonAnnualDTx1000 <- get("nonAnnualDTx1000", envir = .GlobalEnv)
  if (missing(historicalFires))
    historicalFires <- get("historicalFires", envir = .GlobalEnv)
  if (missing(fireBufferedListDT))
    fireBufferedListDT <- get("fireBufferedListDT", envir = .GlobalEnv)
  lapply(nonAnnualDTx1000, setDT)
  colsToUse <- attributes(terms(formula))[["term.labels"]]
  
  # How many of the parameters belong to the model?
  parsModel <- length(colsToUse) 
  ncells <- ncell(landscape)
  r <- raster(landscape)
  years <- as.character(names(annualDTx1000))
  names(years) <- years
  cells <- integer(ncells)
  Nreps <- 10
  yearSplit <- strsplit(names(nonAnnualDTx1000), "_")
  names(yearSplit) <- as.character(seq_along(nonAnnualDTx1000))
  indexNonAnnual <- rbindlist(
    Map(ind = seq_along(nonAnnualDTx1000), date = yearSplit, 
      function(ind, date) data.table(ind = ind, date = date))
  )
  results <- Map(annDTx1000 = annualDTx1000, 
    yr = years, 
    annualFires = historicalFires, 
    annualFireBufferedDT = fireBufferedListDT,
    MoreArgs = list(par = par, parsModel = parsModel, 
                    #pixelIndices = pixelIndices,
                    verbose = verbose, 
                    nonAnnualDTx1000 = nonAnnualDTx1000,
                    indexNonAnnual = indexNonAnnual,
                    colsToUse = colsToUse),
    function(yr, annDTx1000, par, parsModel, 
             annualFires, nonAnnualDTx1000, annualFireBufferedDT, #pixelIndices,
             indexNonAnnual, colsToUse,
             verbose = TRUE) {
      # Rescale to numerics and /1000
      setDT(annDTx1000)
      # needed because data.table objects were recovered from disk
      shortAnnDTx1000 <- nonAnnualDTx1000[[indexNonAnnual[date == yr]$ind]][annDTx1000, on = "pixelID"]
      mat <- as.matrix(shortAnnDTx1000[, ..colsToUse])/1000
      # matrix multiplication
      covPars <- tail(x = par, n = parsModel)
      logisticPars <- par[1:4]
      set(shortAnnDTx1000, NULL, "spreadProb", logistic4p(mat %*% covPars, logisticPars))

      # logistic multiplication
      set(annDTx1000, NULL, "spreadProb", logistic4p(annDTx1000$pred, par[1:4])) ## 5-parameters logistic
      
      medSP <- median(shortAnnDTx1000[, mean(spreadProb, na.rm = TRUE)], na.rm = TRUE)
      if (medSP <= 0.25 & medSP >= 0.16) {
        if (verbose) {
          print(paste0(Sys.getpid(), "-- year: ",yr, ", spreadProb raster: median in buffered pixels = ", 
                       round(medSP, 3)))
        }
        cells[as.integer(shortAnnDTx1000$pixelID)] <- shortAnnDTx1000$spreadProb
        maxSizes <- rep(annualFires$size, times = Nreps) * 2
        lociAll <- rep(annualFires$cells, times = Nreps)
        
        spreadState <- SpaDES.tools::spread(
          landscape = r,
          maxSize = maxSizes,
          loci = rep(annualFires$cells, times = Nreps),
          spreadProb = cells,
          returnIndices = TRUE,
          allowOverlap = TRUE,
          quick = TRUE)
        fireSizes <- tabulate(spreadState[["id"]]) # Here tabulate() is equivalent to table() but faster
        if (length(fireSizes) == 0) browser()
        burnedProb <- spreadState[, .N, by = "indices"]
        setnames(burnedProb, "indices", "pixelID")
        setDT(annualFireBufferedDT)
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
        fireSizes <- sample(1:3, 1)
      }
      
      list(fireSizes = fireSizes, SNLL = SNLL)
    })
  results <- purrr::transpose(results)
  historicalFiresTr <- purrr::transpose(historicalFires)
  
  adTest <- try(ad.test(unlist(results$fireSizes), unlist(historicalFiresTr$size))[["ad"]][1L, 1L])
  SNLLTest <- sum(unlist(results$SNLL))
  objFunRes <- adTest + SNLLTest/1e3 # wADtest is the weight for the AD test
  # gc()
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
