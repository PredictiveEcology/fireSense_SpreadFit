library(SpaDES)

setPaths(
  inputPath = "../inputs",
  modulePath = ".." 
)

# Define simulation parameters
times <- list(start = 1, end = 1, timeunit = "year")
modules <- list("fireSense_SpreadFit")
paths <- list(
  modulePath = modulePath
)

# Define module parameters
parameters <- list(
  fireSense_SpreadFit = list(
    formula = ~ TP_Beta + TP_Theta,
    lower = c(.01, .2, .1,  .3, .001, .001, .001),
    upper = c(.20, .5, 10,  4., .300, .300, .300),
    trace = 5,
    nCores = 1,
    itermax = 5,
    data = c("TP_Beta", "TP_Theta")
  )
)

# Define from where and how data will be loaded in the simList environment
inputs <- data.frame(
  objectName = c("fireLoc_FireSense_SpreadFit", "TP_Beta", "TP_Theta"),
  file = normalizePath(
    file.path(
      getPaths()$inputPath,
      c(
        "fireLoc_FireSense_SpreadFit.shp",
        "dataFireSense_SpreadFit_Beta.tif",
        "dataFireSense_SpreadFit_Theta.tif"
      )
    )
  ),
  functions = c("raster::shapefile", "raster::raster", "raster::raster"),
  loadTime = 1
)

# Create the simList
sim <- simInit(
  times = times,
  modules = modules,
  paths = paths,
  params = parameters,
  inputs = inputs
)


# Time series: start fires at different years
sim <- spades(sim)
sim$fireSense_SpreadFitted

# All fires at the same time
sim[["fireLoc_FireSense_SpreadFit"]][["date"]] <- NULL
sim <- spades(sim)
sim$fireSense_SpreadFitted

