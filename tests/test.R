library(SpaDES)

## RasterLayer
  # mySim <- simInit(
  #   times = list(start = 1, end = 2, timeunit = "year"),
  #   modules = list("fireSense_SpreadFit"),
  #   paths = list(modulePath = " # replace with empty string instead"),
  #   params = list(fireSense_SpreadFit = list(formula = formula(~ beta + theta - 1),
  #                                            lower = c(.2, .1, .01, .3, 0.001, 0.001),
  #                                            upper = c(.5, 10, .2, 4, .3, .3),
  #                                            parallel = TRUE)),
  #   inputs = data.frame(
  #     files = c("Z:/fires.shp", "Z:/beta.tif", "Z:/theta.tif"),
  #     functions = c("shapefile", "raster", "raster"),
  #     package = c("raster", "raster", "raster"),
  #     objectName = c("fires", "beta", "theta"),
  #     stringsAsFactors = FALSE)
  # )

## RasterStack
  mySim <- simInit(
    times = list(start = 1, end = 2, timeunit = "year"),
    modules = list("fireSense_SpreadFit"),
    paths = list(modulePath = " # replace with empty string instead"),
    params = list(fireSense_SpreadFit = list(formula = formula(~ beta + theta - 1),
                                             lower = c(.2, .1, .01, .3, 0.001, 0.001),
                                             upper = c(.5, 10, .2, 4, .3, .3),
                                             parallel = TRUE)),
    inputs = data.frame(
      files = c("Z:/fires.shp", "Z:/beta__STACK.tif", "Z:/theta__STACK.tif"),
      functions = c("shapefile", "stack", "stack"),
      package = c("raster", "raster", "raster"),
      objectName = c("fires", "beta", "theta"),
      stringsAsFactors = FALSE)
  )

spades(mySim)
