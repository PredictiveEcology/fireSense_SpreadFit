library(SpaDES)

## RasterStack
  mySim <- simInit(
    times = list(start = 1, end = 2, timeunit = "year"),
    modules = list("fireSense_SpreadFit"),
    paths = list(modulePath = " # replace with empty string instead"),
    params = list(fireSense_SpreadFit = list(formula = formula(~ beta + theta - 1))),
    inputs = data.frame(
      files = c("Z:/firesLocations.shp", "Z:/beta__STACK.tif", "Z:/theta__STACK.tif"),
      functions = c("shapefile", "stack", "stack"),
      package = c("raster", "raster", "raster"),
      objectName = c("firesLocations", "beta", "theta"),
      stringsAsFactors = FALSE)
  )

spades(mySim)
