.objfun <- function(par, rasters, formula, loci, sizes, fireSense_SpreadFitRaster){
  # Optimization's objective function
  ad.test(
    list(
      unlist(
        mapply(
          FUN = function(x, loci)
          {
            r <- calc(
              predict(x, model = formula, fun = fireSense_SpreadFitRaster, na.rm = TRUE, par = par[5:length(par)]),
              fun = function(x) par[1L] + (par[2L] - par[1L]) / (1 + x^(-par[3L])) ^ par[4L] ## 5-parameters logistic
            )
            print("browser: make sure r is in memory, and that spreadProb")
            browser()
            r[] <- r[]
            rasVals <- data.table(valsR = getValues(r))
            
            if (median(spreadProb, na.rm = TRUE) > .245) return(1e100)
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
