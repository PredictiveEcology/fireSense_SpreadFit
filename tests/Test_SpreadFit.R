library(SpaDES)

modulePath <- normalizePath("..")

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
    lower = c(.2, .1, .01, .3, 0.001, 0.001),
    upper = c(.5, 10, .2, 4, .3, .3),
    trace = 5,
    nCores = 1,
    itermax = 5,
    data = c("TP_Beta", "TP_Theta")
  )
)

# Define from where and how data will be loaded in the simList environment
inputs <- data.frame(
  objectName = c("fireLoc_FireSense_SpreadFit", "TP_Beta", "TP_Theta"),
  file = normalizePath(c("../inputs/fireLoc_FireSense_SpreadFit.shp",
                         "../inputs/dataFireSense_SpreadFit_Beta.tif",
                         "../inputs/dataFireSense_SpreadFit_Theta.tif")),
  fun = c("shapefile", "stack", "stack"),
  package = c("raster", "raster", "raster"),
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

# All fires at the same time
loadFiles(sim)
# sim[["fires"]][["date"]] <- NULL
# sim[["beta"]] <- setNames(raster::unstack(sim[["beta"]])[[1]], "beta")
# sim[["theta"]] <- setNames(raster::unstack(sim[["theta"]])[[1]], "theta")
# inputs(sim) <- list()

sim <- spades(sim)
sim$fireSense_SpreadFitted
