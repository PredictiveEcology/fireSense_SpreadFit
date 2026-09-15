## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(.ELFind                                = "character",
      .runName                               = "character",
      fireBufferedListDT                     = "list",
      fireSense_annualSpreadFitCovariates    = "data.table",
      fireSense_nonAnnualSpreadFitCovariates = "data.table",
      fireSense_spreadFormula                = "character",
      parsKnown                              = "numeric",
      rasterToMatch                          = "SpatRaster",
      spreadFirePoints                       = "sf",
      spreadFitAdditionalColNames            = "character",
      studyArea                              = "sf")
  )
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(
    outputs[order(names(outputs))],
    c(covMinMax_spread          = "data.table",
      DE                        = "data.table",
      fireSense_SpreadFitted    = "fireSense_SpreadFit",
      fsSpreadFit_hists         = "ggplot",
      lociList                  = "list",
      studyAreaWithSpreadParams = "sf")
  )
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    sort(c(".c", ".plots", ".plotSize", ".runInitialTime", ".runInterval",
           ".saveInitialTime", ".saveInterval", ".useCache", "cacheId_DE",
           "cloudFolderID_DE", "cores", "DEoptimControl", "DEoptimTests", "doObjFunAssertions",
           "initialpop", "iterDEoptim", "iterStep", "iterThresh", "libPathDEoptim",
           "lower", "maxFireSpread", "mode", "mutuallyExclusiveCols", "NP",
           "objFunCoresInternal", "objfunFireReps", "onlyLoadDEOptim", "rep",
           "rescaleAll", "SNLL_FS_thresh", "spreadFitFilename",
           "spreadFitGoogleDriveFolder", "stopIfNoPreRunFit", "strategy", "trace",
           "upper", "upperAndLowerVal", "urlDEOptimObject", "useCache_DE",
           "useCloud_DE", "verbose", "visualizeDEoptim"))
  )
})

test_that("the required clusters has the fixes a fit on the fleet needs", {
  ## clusters 0.0.41 (2026-09-15): clusterSetup() no longer sends the cluster object to every worker (a fit
  ## stalled for hours), works without reproducible attached or ~/.ssh/config, picks tunnel ports below the
  ## ephemeral range (a 110-worker build hung), and runs iterStep generations per DEoptim call. With a lower
  ## floor, Require keeps an installed clusters that has none of these (the fleet had 0.0.31).
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  clustersReq <- grep("/clusters@", unlist(md$reqdPkgs), value = TRUE)
  expect_length(clustersReq, 1L)
  expect_true(package_version(sub(".*>=\\s*([0-9.]+).*", "\\1", clustersReq)) >= "0.0.41")
})
