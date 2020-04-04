makeBufferedFires <- function(fireLocationsPolys, rasterToMatch, 
                              lowerTolerance = 3.8, 
                              upperTolerance = 4.2,
                              verbose = getOption("verbose", TRUE),
                              useParallel = FALSE){
  t1 <- Sys.time()
  # fireLocationsPolys: list of each year of SpatialPolygonsDataFrame with fire polygons
  # lowerTolerance: lower tolerance for buffer to be different from fire points (i.e. 0.8, 20% lower) 
  # upperTolerance: higher tolerance for buffer to be different from fire points (i.e. 1.2, 20% higher)
  library("sf") # st_as_sf
  library("fasterize") # fasterize
  library("raster") # raster
  library("data.table")
  library("crayon") # yellow, green
  library("future")
  library("future.apply")
  fun <- ifelse (useParallel, future_lapply, lapply)
  historicalFire <- do.call(what = fun, args = list(X = names(fireLocationsPolys), FUN = function(yr){
    # Projection is not the same, so I need to convert the polygon
    fireLocationsPoly <- reproducible::projectInputs(x = fireLocationsPolys[[yr]],
                                                     targetCRS = crs(rasterToMatch))
    sf_fPY <- sf::st_as_sf(fireLocationsPoly)
    print("MakeBufferedFires")
    firePolyRas <- fasterize::fasterize(sf = sf_fPY, raster = raster(rasterToMatch), field = "NFIREID")
    names(firePolyRas) <- yr
    # Do the calculation for each fire
    fireIDS <- unique(firePolyRas[!is.na(firePolyRas)])
    allFires <- suppressWarnings(do.call(what = fun, args = list(X = fireIDS, FUN = function(fireID){
      valsFireRas  <- which(firePolyRas[] == fireID)
      adj <- adjacent(firePolyRas, valsFireRas, directions = 8, pairs = FALSE)
      tb <- data.table(V1 = c(0, 1), N = c(1, 2))
      stateToCheck <- paste0("isFALSE(isTRUE(tb[1, N] < tb[2, N]*upperTolerance) & ",
                             "isTRUE(tb[1, N] > tb[2, N]*lowerTolerance))")
      while (eval(parse(text = stateToCheck))){
        adj <- adjacent(firePolyRas, adj, directions = 8, pairs = FALSE)
        rasBuffer <- raster(firePolyRas)
        rasBuffer[adj] <- 0
        rasBuffer[valsFireRas] <- 1
        tb <- data.table(table(rasBuffer[]))
        perc <- round((tb[1, N]/tb[2, N])*100, 0)
        direcion <- ifelse(perc > 100, "larger", "smaller")
        perc <- ifelse(direcion == "larger", perc - 100, 100 - perc)
        if (verbose){
          if (all(direcion == "larger", perc/100 > upperTolerance-1)){
            message(crayon::red(paste0("Buffered area is ", perc,
                                       "% ", direcion, " than fires. Outside of bounds, ",
                                       "returning for ", yr)))
          } else {
            if(eval(parse(text = stateToCheck))){
              message(crayon::yellow(paste0("Buffered area is ", perc,
                                            "% ", direcion, " than fires. Trying again for ", yr)))
            } else {
              message(crayon::green(paste0("Buffered area is ", perc,
                                           "% ", direcion, " than fires. Within bounds for ", 
                                           yr)))
            }
          }
        }
        if (tb[1, N] > tb[2, N]*upperTolerance) break
      }
      if (is.null(rasBuffer)){
        print("NULL raster? Debug")
        browser()
      }
      attr(rasBuffer, "buffer") <- adj
      return(rasBuffer)
    })))
    # Convert to data table to speed up putting the rasters back together
    adjAll <- unlist(lapply(allFires, attr, which = "buffer"))
    allFires <- lapply(allFires, function(ras){
      attr(ras, "buffer") <- NULL
      return(ras)
      })
    stk <- stack(allFires)
    stkDT <- as.data.table(stk[])
    stkDT[, fires := rowSums(.SD, na.rm = TRUE)]
    rasBuffer <- setValues(raster(firePolyRas), stkDT$fires)
    # Put NA's back
    rasBuffer[rasBuffer > 1] <- 1 # Just making sure all fires are 1 (the same pixel might have burned                                     more then one time)
    fires  <- which(rasBuffer[] == 1)
    rasBuffer[] <- NA
    rasBuffer[adjAll] <- 0 # Buffer
    rasBuffer[fires] <- 1 # Fires
    return(rasBuffer)
  }))
  names(historicalFire) <- names(fireLocationsPolys)
  return(historicalFire)
  if (verbose)
    print(Sys.time() - t1)
} 
