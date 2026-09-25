defineModule(sim, list(
  name = "fireSense_SpreadFit",
  description = paste("Fit statistical models that can be used to parameterize the",
                      "fire spread component of simulation models (e.g. fireSense).",
                      "This module implement a Pattern Oriented Modelling (POM)",
                      "approach to derive spread probabilities from final fire sizes.",
                      "Spread probabilities can vary between pixels, and thus reflect",
                      "local heterogeneity in environmental conditions."),
  keywords = c("fire", "spread", "POM", "percolation"),
  authors = c(
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "aut"),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "aut"),
    person("Alex M.", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(),
  version = list(fireSense_SpreadFit = "1.0.6.9013"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_SpreadFit.Rmd"),
  loadOrder = list(after = c("fireSense_dataPrepFit", "fireSense_ignitionFit")),
  reqdPkgs = list("data.table", "DEoptim", "fpCompare", "future",
                  "ggplot2", "scales", "kSamples", "munsell",
                  "logging", "magrittr", "parallel", "raster", "terra", "tidyr", ## TODO: remove magrittr
                  "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/clusters@main (>= 0.0.46)",
                  "PredictiveEcology/Require@development (>= 0.3.1)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9045)",
                  "PredictiveEcology/SpaDES.tools@development (>= 2.1.3.9008)"),
  parameters = rbind(
    defineParameter(".plots", "character|logical", default = NULL, ## TODO: use .plotInitialTime etc.
                    desc = "Plot types passed to `Plots()`, e.g. 'png' or 'screen'; NULL or NA for none."),
    defineParameter(".plotSize", "list", default = list(height = 1600, width = 2000),
                    desc = paste("List specifying height and width of plotting device (in pixels)",
                                 "used to plot DEoptim histograms when `visualizeDEoptim` is TRUE.")),
    defineParameter(".runInitialTime", "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start time of the simulation."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used."),
    defineParameter(".useCache", c("logical", "character"), "init", NA, NA,
                    desc = paste("Should this entire module be run",
                                 "with caching activated? This is generally intended for data-type",
                                 "modules, where stochasticity and time are not relevant.")),
    defineParameter("cores", "integer", default = 1L,
                    desc = paste("Passed to `cores` in `fireSenseUtils::runDEoptim()`: a number of local cores, or a",
                                 "character vector of machine names, one element per core wanted on that machine.")),
    defineParameter("DEoptimTests", "character", default = c("adTest", "SNLL_FS"),
                    desc = paste("Currently either `'SNLL_FS'` or `'adTest'` or a length 2 character vector of both.",
                                 "Passed to `tests` in `fireSenseUtils::.objfunSpreadFit()`.")),
    defineParameter("doObjFunAssertions", "logical", default = TRUE,
                    desc = "Passed to `fireSenseUtils::.objfunSpreadFit()`; TRUE runs diagnostics but is slower; FALSE for operational runs"),
    defineParameter("initialpop", "numeric", default = NULL,
                    desc = paste("A numeric matrix of dimensions `NCOL = length(lower)`",
                                 "and `NROW = NP`. This will be passed into DEoptim",
                                 "through `control$initialpop = P(sim)$initialpop` if it is",
                                 "not NULL")),
    defineParameter("iterDEoptim", "integer", default = 5000L,
                    desc = paste("integer defining the maximum number of iterations allowed (DEoptim optimizer).",
                                 "A ceiling: clusters (>= 0.0.46) stops the fit earlier, once the population's",
                                 "median value has stopped improving.")),
    defineParameter("iterStep", "integer", default = 25L,
                    desc = paste("DEoptim runs its `iterDEoptim` iterations in blocks of this many; each block is",
                                 "cached and, if `visualizeDEoptim` is a path, plotted.")),
    defineParameter("iterThresh", "integer", default = 96L,
                    desc = "Number of random parameter sets tried when calibrating `SNLL_FS_thresh`."),
    defineParameter("libPathDEoptim", "character", default = .libPaths()[1],
                    desc = paste("Absolute path specifying R package directory location to use when running DEotpim.",
                                 "NOTE: this path must be read/write accessible on ALL machines",
                                 "used for fitting (identified in cores).",
                                 "Therefore, it's best use a directory in your user's `~` directory.",
                                 "If the directory does not exist at this path, will attempt to create it.")),
    defineParameter("lower", "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Lower limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (named in the order they",
                                 "appear in the formula).")),
    defineParameter("maxFireSpread", "numeric", default = 0.28,
                    desc = paste0("optional. Maximum fire spread average to be passed to the `.objFun`. ",
                                  "This puts an upper limit on `spreadProb` during optimization.")),
    defineParameter("link", "character", default = "logistic3p",
                    desc = paste("The spread link. 'logistic3p', or 'logistic3pUpper': the same curve with",
                                 "Stukel's upper tail, one more parameter `upperTail1` that changes only how the",
                                 "curve approaches its ceiling (`fireSenseUtils::logistic3pUpper()`). Its default",
                                 "bounds are `upperTailBounds`.")),
    defineParameter("mode", "character", default = "fit",
                    desc = paste("Options: debug, fit, visualize, validate. Can use multiples. 'debug' runs the",
                                 "objective function with visuals instead of DEoptim; 'fit' runs DEoptim; 'visualize'",
                                 "adds the `debug` and `plot` events after the fit; 'validate' adds `crossValidate`,",
                                 "two more fits, each on half the years, predicting the other half",
                                 "(`sim$spreadFitHeldOut`). Validation never writes the ledger.")),
    defineParameter("profileReps", "integer", default = 10L,
                    desc = paste("After the fit, each covariate coefficient in turn is set to 0 and to 5 values",
                                 "across the final population, the others held at the best member, and each point",
                                 "is evaluated this many times (`fireSenseUtils::profileCoefficients()`). About",
                                 "`6 * nCoefficients * profileReps` evaluations, on the fit's workers. It decides",
                                 "which coefficients are identified in isolation (`sim$spreadFitIdentifiability`).",
                                 "0 skips it.")),
    defineParameter("mutuallyExclusiveCols", "list", list("youngAge" = c("class", "nonForest")), NA, NA,
                    desc = "a named list of mutually exclusive covariates - see `fireSenseUtils::makeMutuallyExclusive`"),
    defineParameter("nCoresNeeded", "integer", default = NULL,
                    desc = paste("How many workers to request for the DEoptim cluster. This IS the population",
                                 "size: `clusters::clusterSetup()` sets NP to the workers it builds. `NULL`",
                                 "leaves `fireSenseUtils::runDEoptim()`'s default of 10 per estimated",
                                 "parameter. A generation costs the slowest of NP evaluations and that barely",
                                 "falls as NP falls, so a smaller NP buys throughput by allowing more fits at",
                                 "once rather than by shortening generations (measured 2026-09-16).")),
    defineParameter("simulateMembers", "integer", default = 10L,
                    desc = paste("After the fit, this many best members simulate the observed fires without the",
                                 "size cap, for `sim$spreadFitSizes` and `sim$spreadFitLinkSaturation`; also the",
                                 "members each `crossValidate` fold predicts with. 0 skips it after the fit.")),
    defineParameter("sizeLik", "character", default = "t",
                    desc = paste("Likelihood of fire size in the objective, 'kde' or 't', passed to",
                                 "`fireSenseUtils::runDEoptim()`. 't' with `weighted = FALSE` predicted held-out",
                                 "years best in the 2026-09-21 cross-validation.")),
    defineParameter("escapeSizeHa", "numeric", default = 50,
                    desc = paste("Size (ha) a fire must reach to count as escaped. The spread model is fitted to",
                                 "escaped fires only, and each simulated fire first grows to this size with its",
                                 "own spread probabilities (its burning cells stay active until it gets there),",
                                 "then spreads normally. `NULL` or `NA` gives the old fit (any fire over 1",
                                 "pixel). Passed to `fireSenseUtils::runDEoptim()`.")),
    defineParameter("sizeLikDf", "numeric", default = 5,
                    desc = "Degrees of freedom of the 't' size likelihood."),
    defineParameter("weighted", "logical|character", default = FALSE,
                    desc = paste("Weight of each fire in the size likelihood: FALSE (none), TRUE (log size) or",
                                 "'sqrt'. Passed to `fireSenseUtils::runDEoptim()`.")),
    defineParameter("adWeight", "character|numeric", default = "auto",
                    desc = paste("Weight of the Anderson-Darling term against the size likelihood; 'auto' is",
                                 "`fireSenseUtils::adWeightAuto()`.")),
    defineParameter("yearAreaWeight", "numeric|character", default = 0,
                    desc = paste("Weight of the annual-area term in the objective: each fit year's observed area",
                                 "burned is scored against that year's simulated totals (one per replicate) with",
                                 "the size likelihood. 0 (default) leaves it out; 'auto' is (number of fitted",
                                 "fires) / (number of fit years), so the year view and the per-fire view weigh",
                                 "the same. Passed to `fireSenseUtils::runDEoptim()`.")),
    defineParameter("areaDistWeight", "numeric|character", default = 0,
                    desc = paste("Weight of the area-weighted size-distribution term: simulated and observed fires",
                                 "compared by the share of area burned that fires up to each size make up",
                                 "(`fireSenseUtils::areaWeightedCvM()`). 0 (default) leaves it out; 'auto' uses",
                                 "the Anderson-Darling term's weight (`fireSenseUtils::adWeightAuto()`).")),
    defineParameter("jumpTries", "numeric", default = 0,
                    desc = paste("With `escapeSizeHa`: how many attempts a simulated fire that is still below the",
                                 "escape size, with no burnable neighbour left, may make to jump to burnable land",
                                 "nearby (`SpaDES.tools::spreadCpp()`). 0 (default) is off.")),
    defineParameter("jumpMeanDist", "numeric", default = 3,
                    desc = paste("Mean jump distance (pixels) for `jumpTries`; distances are exponential,",
                                 "truncated to 1.5-20 pixels. No effect while `jumpTries` is 0.")),
    defineParameter("objFunCoresInternal", "integer", default = 1L,
                    desc = paste("Integer defining the number of cores to pass to `mcmapply(mc.cores = ...)`",
                                 "This will fork this many to do the years loop internally.",
                                 "This would be in addition to `cores` and is effecively a multiplier.",
                                 "The computer needs to have `cores * objFunCoresInternal` threads or it will stall.")),
    defineParameter("objfunFireReps", "integer", default = 50L,
                    desc = paste("integer defining the number of replicates the objective function",
                                 "will attempt each fire.")),
    defineParameter("rep", "integer", 1L, NA, NA,
                    desc = paste("An optional integer indicating which replicate run this represents. ",
                                 "This is used to identify unique runs of `runDEoptim`, from a Cache perspective. ",
                                 "For example, if this module is run twice with all the same data, ",
                                 "Cache will think that the second run ",
                                 "should recover the cache result, unless this `rep` is modified")),
    defineParameter(".c", "numeric", 0, NA, NA,
                    desc = paste("the `c` argument passed to DEoptim.control. With `iterStep` > 1, clusters",
                                 "(>= 0.0.42) runs DEoptim with `c = 0` instead: DEoptim's adaptation can make",
                                 "every trial NaN when one call runs several generations. With `iterStep = 1`",
                                 "the adaptation restarts every generation, so `c` has no effect either way.")),
    defineParameter("DEoptimControl", "list", list(p = 0.1), NA, NA,
                    desc = paste("Further `DEoptim.control()` settings, e.g. `list(CR = 0.7, F = 0.6)`,",
                                 "passed through `fireSenseUtils::runDEoptim()` to DEoptim. Names must be",
                                 "`DEoptim.control()` arguments. `strategy`, `trace`, `initialpop` and `.c`",
                                 "have their own parameters; `NP` is the number of workers the cluster gets.",
                                 "The default `p = 0.1` is for `strategy = 6`.")),
    defineParameter("rescaleAll", "logical", TRUE, NA, NA,
                    desc = "rescale covariates for `DEOptim`"),
    defineParameter("spreadFitGoogleDriveFolder", "character", "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf",
                    NA, NA, "Google Drive folder url holding the shared fit ledger (`spreadFitFilename`)."),
    defineParameter("spreadFitFilename", "character", "latest",
                    NA, NA, paste("File name of the shared fit ledger: an `sf` object with one row of fitted parameters",
                                  "per polygon. `\"latest\"` (the default) writes to the file named for this fit's fire",
                                  "years and model, `fireSenseUtils::spreadFitFilenameFor()`, e.g.",
                                  "`fireSenseParams_1985-2024_linearFuel.rds`; readers then find it with",
                                  "`fireSenseUtils::latestSpreadFits()`.")),
    defineParameter("strategy", "integer", default = 6L,
                    desc = paste("Passed to `DEoptim.control`. 6 (DE/current-to-p-best/1) with `p = 0.1` did best",
                                 "in a settings study (see NEWS).")),
    defineParameter("SNLL_FS_thresh", "integer", default = NULL,
                    desc = "Threshold multiplier used in objective function SNLL fire size test."),
    defineParameter("refitExisting", "logical", FALSE, NA, NA,
                    paste("FOR DEVELOPERS ONLY: a re-fit is a full DEoptim run and is only practical with",
                          "access to at least 40 cores. Fit this polygon even when the ledger already holds",
                          "parameters for it. A ledger row normally means the fit is done, and the run event",
                          "skips it. Set this when the fit's INPUTS have changed -- new land cover, new",
                          "vegetation parameters, a new objective -- so the stored row is stale and the",
                          "polygon must be fitted again. When TRUE it OVERRIDES `stopIfNoPreRunFit`: `init`",
                          "schedules the fit instead of stopping.")),
    defineParameter("stopIfNoPreRunFit", "logical", default = TRUE,
                    desc = paste("If TRUE, `init` stops with an error when this polygon would have to be",
                                 "fitted, instead of fitting it. Ignored when `refitExisting` is TRUE.")),
    
    defineParameter("trace", "numeric", default = 1L,
                    desc = paste("non-negative integer. If > 0, tracing information on",
                                 "the progress of the optimization are printed every",
                                 "`trace` iteration. Default is 1, i.e. every iteration. ",
                                 "Setting to 0 turns off tracing.")),
    defineParameter("upper", "numeric", default = NA,
                    desc = paste("see `?DEoptim`. Upper limits for the logistic function",
                                 "parameters (lower bound, upper bound, slope, asymmetry)",
                                 "and the statistical model parameters (named in the order they",
                                 "appear in the formula).")),
    defineParameter("useCache_DE", "logical", default = TRUE,
                    desc = "should `DEoptim` use `Cache`? to do multiple independent runs, use FALSE"),
    defineParameter("verbose", "numeric", default = 1,
                    desc = paste0("optional. With increasing number, more verbosity. Level 1 is ",
                                  "normal reproducible (e.g., Cache), level 2 includes objective function ",
                                  "e.g., print median of spreadProb during calculations")),
    defineParameter("visualizeDEoptim", "Path", default = asPath(figurePath(sim)),
                    desc = paste("Directory where `runDEoptim` saves parameter plots after each `iterStep` block.",
                                 "Reset to `figurePath(sim)` unless its last folder is the module name.")),
    defineParameter("covFixedRange", "list", default = list(CMDsm = c(0, 100), CMD = c(0, 100), CMDsp = c(0, 100),
                                                            cumMDC = c(0, 100)),
                    desc = paste("Named list of `c(min, max)`: covariates rescaled with this FIXED range and not with the",
                                 "range of this polygon's data. `CMDsm = c(0, 100)` makes the covariate CMDsm / 100 in every",
                                 "polygon. With the data's range, 1 meant a CMDsm of 104 in one polygon and 297 in another,",
                                 "so the coefficient could not be compared across polygons, and a polygon that never gets",
                                 "dry stretched its small range over [0, 1]. Names not among the covariates are ignored.",
                                 "`fireSense_SpreadPredict` rescales with the stored `covMinMax_spread`, so it follows. CMD, CMDsp and",
                                 "cumMDC (also mm) are the other candidates of fireSense_dataPrepFit's `spread = 'auto'`,",
                                 "so an ELF that picks one of them gets the same fixed scale.")),
    defineParameter("yearSpreadSDBounds", "numeric", default = c(0, 1),
                    desc = paste("Bounds of `yearSpreadSD`, the sd of a per-year random effect on logit spread",
                                 "probability (`fireSenseUtils::.objfunSpreadFit()`), when `lower`/`upper` are not",
                                 "supplied. A seasonal departure: each year draws one eps, so all of a year's fires",
                                 "burn hotter or cooler together, which widens the simulated fire-size",
                                 "distribution. `NA` turns it off.")),
    defineParameter("upperTailBounds", "numeric", default = c(-1, 1),
                    desc = "Bounds of `upperTail1` when `link` is 'logistic3pUpper' and `lower`/`upper` are not supplied."),
    defineParameter("upperAndLowerVal", "numeric", default = 9,
                    desc = "Bound given to each covariate coefficient (`upper` = this, `lower` = minus this) when `upper` or `lower` is not supplied."),
    defineParameter("upperAndLowerValFuel", "numeric", default = 60,
                    desc = paste("As `upperAndLowerVal`, for the fuel biomass covariates. They are biomass / 1e4, so their",
                                 "coefficients are larger than those of covariates rescaled to [0, 1]: with a bound of 9",
                                 "the fuel coefficient sat on the bound (fitted 4.57 in a +-9 box on the log scale, 11.07",
                                 "once widened; 31.7 on the linear scale). 60 did not bind in any of 36 fits."))
  ),
  inputObjects = rbind(
    expectsInput(".runName", "character", "Some descriptive, short name for this fitting, e.g., ELF14.1"),
    expectsInput(".ELFind", "character",
                 desc = paste("Identifier of the polygon being fit, e.g. '6.1.1'. This becomes the",
                              "`polygonID` of the row this module writes to the shared cloud fit",
                              "ledger (`spreadFitFilename` in `spreadFitGoogleDriveFolder`), which",
                              "`fireSense_dataPrepFit` matches against the polygon ids carried by",
                              "`rasterToMatchELF`. It must therefore be the polygon's identity, not",
                              "a run label: `.runName` encodes the whole scenario (climate period,",
                              "GCM, SSP, rep) in some projects, and keying the ledger on it writes",
                              "rows no other run can find and trips dataPrepFit's id match.",
                              "Defaults to `.runName` for backwards compatibility.")),
    expectsInput("fireBufferedListDT", "list",
                 desc = "list of data.tables with fire id, pixelID, and buffer status"),
    expectsInput("fireSense_annualSpreadFitCovariates", "data.table",
                 desc = "table of climate and/or veg covariates, burn status, polyID, and pixelID"),
    expectsInput("fireSense_nonAnnualSpreadFitCovariates", "data.table",
                 desc = "table of veg covariates, burn status, polyID, and pixelID"),
    expectsInput("spreadFitAdditionalColNames", "character",
                 desc = paste0("Names of the list-columns of the ledger row. Reset to ",
                               "`fireSenseUtils::spreadFitAdditionalColNamesTxt` if different.")),
    expectsInput("fireSense_spreadFormula", "character",
                 desc = paste0("a formula that contains the annual and non-annual covariates ",
                               "e.g. `~ 0 + MDC + class2 + class3 + youngAge`.")),
    expectsInput("parsKnown", "numeric",
                 desc = paste0("Optional vector of known parameters, e.g., from a previous `DEoptim` run. ",
                               "If this is supplied, then 'mode' will be automatically converted to 'debug'")),
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "template raster for study area"),
    expectsInput("spreadFirePoints", "sf",
                 desc = "list of `sf` points, one element per year, of fire ignition locations"),
    expectsInput("studyArea", "sf",
                 desc = "Polygon being fit; its geometry and crs go in the ledger row. Defaults to NWT.",
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU")
  ),
  outputObjects = rbind(
    createsOutput("covMinMax_spread", "data.table",
                  desc = "`data.table` of covariates min and max"),
    createsOutput("DE", "data.table",
                  desc = "list of `DEoptim` objects, one per `iterStep` block, ordered by best objective value"),
    createsOutput("studyAreaWithSpreadParams", "sf",
                  desc = paste("Rows of the shared fit ledger that intersect `studyArea`, including the row this",
                               "fit writes: `studyArea` geometry, `polygonID`, and list-columns named by",
                               "`spreadFitAdditionalColNames` (the 5 best parameter sets are in `params`).")),
    createsOutput("fsSpreadFit_hists", "ggplot",
                  desc = "histograms of each parameter used in `DEoptim` fitting."),
    createsOutput("lociList", "list",
                  desc = "per-year `data.table`s of fire start cells and sizes, from `fireSenseUtils::makeLociList()`"),
    createsOutput("spreadFitConvergence", "data.table",
                  desc = "The objective across the fit's generations (`fireSenseUtils::fitConvergence()`)."),
    createsOutput("spreadFitRescore", "data.table",
                  desc = paste("The final population, one row per member, with the mean and sd of its replicated",
                               "re-scores (`reMean`, `reSD`). The ledger's parameter sets are the best of these.")),
    createsOutput("spreadFitIdentifiability", "data.table",
                  desc = paste("One row per covariate coefficient: how tightly the population pins it",
                               "(`fireSenseUtils::coefIdentifiability()`) and, with `profileReps > 0`, whether",
                               "dropping it worsens the fit; `identified` = identified in isolation",
                               "(`fireSenseUtils::identifiedInIsolation()`).")),
    createsOutput("spreadFitProfile", "data.table",
                  desc = "The one-at-a-time profile around the best member (`fireSenseUtils::profileCoefficients()`)."),
    createsOutput("spreadFitSizes", "data.table",
                  desc = paste("Observed against simulated fire sizes of the fitted years, without the size cap",
                               "(`fireSenseUtils::scoreFireSizes()`): bias, error, quantiles.")),
    createsOutput("spreadFitLinkSaturation", "data.table",
                  desc = paste("Per member, the share of pixel-years at the spread-probability ceiling and the",
                               "quantiles of spread probability (`fireSenseUtils::linkSaturation()`).")),
    createsOutput("spreadFitHeldOut", "list",
                  desc = paste("mode 'validate' only: `sims`, the held-out years simulated from the fit to the",
                               "other years (column `fold`), and `score`, from `fireSenseUtils::scoreFireSizes()`."))
  )
))

#' Event dispatcher for fireSense_SpreadFit
#'
#' `init` schedules `spreadFitPrepare`, and, unless the ledger already holds a fit for this polygon
#' (or `refitExisting` is TRUE), `estimateThreshold` then `run` (or `debug` when `mode` has "debug").
#'
#' @param sim a `simList`.
#' @param eventTime numeric; current simulation time.
#' @param eventType character; one of `init`, `spreadFitPrepare`, `estimateThreshold`, `run`,
#'   `debug`, `plot`, `postFitDiagnostics` (after every fit), `crossValidate` (mode "validate").
#' @param debug not used.
#' @return the `simList`, invisibly.
doEvent.fireSense_SpreadFit = function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- current(sim)$moduleName
  switch(
    eventType,
    init = {
      if (!is.null(Par$debugMode)) if (Par$debugMode)
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))

      # If user supplies known DEOptim outputs as simple coefficients using parsKnown...
      if (!is.null(sim$parsKnown)) {
        params(sim)[[moduleName]][["mode"]] <- unique(c(P(sim)$mode, "debug"))
      }
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "spreadFitPrepare")
      
      # Fit unless the ledger already holds parameters for THIS polygon. The object
      # may exist and be a data.frame while holding only neighbours' rows, or none.
      # `refitExisting` overrides that: the stored row is stale when the inputs have changed.
      if (isTRUE(Par$refitExisting) || !hasPreRunFitForThisPolygon(sim)) {
        # `refitExisting` is an explicit request to fit, so it overrides `stopIfNoPreRunFit`.
        if (isTRUE(Par$stopIfNoPreRunFit) && !isTRUE(Par$refitExisting))
          stop("There is no pre-run SpreadFit (sim$studyAreaWithSpreadParams), ",
               "but parameter `stopIfNoPreRunFit` is `TRUE`")
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "spreadFitPrepare")
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "estimateThreshold")
        if ("debug" %in% P(sim)$mode) {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
        } else {
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "run")
          sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "postFitDiagnostics")
          if ("visualize" %in% P(sim)$mode) {
            sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "debug")
            sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "plot")
          }
        }
      } else if ("validate" %in% P(sim)$mode) {
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "estimateThreshold")
      }
      if ("validate" %in% P(sim)$mode)
        sim <- scheduleEvent(sim, P(sim)$.runInitialTime, moduleName, "crossValidate")
    },
    spreadFitPrepare = {
      sim <- spreadFitPrep(sim) # makes the covariates into the x1000 integers
    },
    debug = {
      ## This below is to test the code without running DEOptim
      thresh <- runSpreadWithoutDEoptim(
        iterThres = P(sim)$iterThresh,
        lower = P(sim)$lower, upper = P(sim)$upper,
        fireSense_spreadFormula = sim$fireSense_spreadFormula,
        flammableRTM = sim$rasterToMatch,
        mutuallyExclusive =  P(sim)$mutuallyExclusiveCols,
        doObjFunAssertions = P(sim)$doObjFunAssertions,
        annualDTx1000 = mod$covsX1000$annualDTx1000,
        nonAnnualDTx1000 = mod$covsX1000$nonAnnualDTx1000,
        fireBufferedListDT = mod$covsX1000$fireBufferedListDT,
        historicalFires = mod$covsX1000$historicalFires,
        covMinMax = sim$covMinMax_spread,
        formulaToFit = sim$fireSense_spreadFormula,
        objfunFireReps = P(sim)$objfunFireReps,
        tests = P(sim)$DEoptimTests,
        mode = Par$mode,
        escapeSizeHa = escapeSizeHaOrNULL(P(sim)$escapeSizeHa),
        weighted = P(sim)$weighted, sizeLik = P(sim)$sizeLik, sizeLikDf = P(sim)$sizeLikDf,
        adWeight = P(sim)$adWeight, link = spreadLink(P(sim)$link),
        jumpTries = P(sim)$jumpTries, jumpMeanDist = P(sim)$jumpMeanDist,
        yearAreaWeight = P(sim)$yearAreaWeight, areaDistWeight = P(sim)$areaDistWeight,
        maxFireSpread = P(sim)$maxFireSpread) 
    },
    estimateThreshold = {
      # Estimate threshold for .objFunSpreadFit
      sim <- estimateSNLLThresholdPostLargeFires(sim)
    },
    run = {
      if (isTRUE(Par$refitExisting) || !hasPreRunFitForThisPolygon(sim)) {

        if (!identical(basename(Par$visualizeDEoptim), currentModule(sim))) {
          params(sim)[[currentModule(sim)]][["visualizeDEoptim"]] <- figurePath(sim)
        }
        DE <- fitSpread(sim, mod$covsX1000, mod$thresh, runName = sim$.runName)
        mod$fitDE <- DE # sim$DE is reordered below, which drops the attributes postFitDiagnostics reads
        sim$DE <- DE
        objFunVal <- vapply(sim$DE, function(D) D$member$bestvalit, FUN.VALUE = numeric(1))
        ord <- order(objFunVal, decreasing = FALSE)
        sim$DE <- sim$DE[ord]
        ## The 5 distinct members of the final population with the lowest replicated mean -- not the 5
        ## generations with the lowest best value, which are copies of one (lucky) member. Read from `DE`:
        ## reordering `sim$DE` above drops its "finalRescore" attribute.
        best <- bestParamSets(DE, names(P(sim)$lower), n = 5L)
        paramsBest <- best$params
        objFunValBest <- best$objFunVal
        numIterations <- length(sim$DE)
        
        # This is normally OK, but there are cached calls that are recovering the wrong ones.
        if (!setequal(sim$spreadFitAdditionalColNames, fireSenseUtils::spreadFitAdditionalColNamesTxt)) {
          sim$spreadFitAdditionalColNames <- fireSenseUtils::spreadFitAdditionalColNamesTxt
        }
        
        ## covMinMax_spread: prediction rescales covariates with it, exactly as this fit did
        df <- data.frame(I(list(numIterations)),
                         I(list(objFunValBest)),
                         I(list(paramsBest)),
                         I(list(sim$sppEquiv)),
                         I(list(sim$nonForestedLCCGroups)),
                         I(list(sim$missingLCCgroup)),
                         I(list(sim$covMinMax_spread))) |>
          setNames(fireSenseUtils::spreadFitAdditionalColNamesTxt)
        # The ledger is keyed by polygon identity, NOT by run label -- see the
        # `.ELFind` input declaration. This row is shared cloud state that every
        # other project reads, so validate before writing.
        polygonID <- sim$.ELFind
        if (!is.character(polygonID) || length(polygonID) != 1L ||
            is.na(polygonID) || !nzchar(polygonID))
          stop("fireSense_SpreadFit: `sim$.ELFind` must be a single non-empty character ",
               "identifying the polygon being fit; got: ",
               paste(format(polygonID), collapse = ", "))
        df <- data.frame(df, "polygonID" = polygonID)
        
        crses <- terra::crs(sim$studyArea)
        b <- dplyr::mutate(df, crs = I(crses)) 
        
        # need to add crs as an entry in a column

        saHere <- if (is(sim$studyArea, "SpatVector")) sf::st_as_sf(sim$studyArea) else sim$studyArea
        saHere <- sf::st_as_sf(sf::st_geometry(saHere))
        sf::st_geometry(saHere) <- "geometry"
        sim$studyAreaWithSpreadParams <- saHere |>
          dplyr::mutate(df)
        le <- function(x) {x}
        sim$studyAreaWithSpreadParams <- CacheGeo(cloudFolderID = Par$spreadFitGoogleDriveFolder,
                                                  targetFile = ledgerWriteFile(
                                                    Par$spreadFitFilename,
                                                    P(sim, module = "fireSense_dataPrepFit")$fireYears,
                                                    names(sim$fireSense_annualSpreadFitCovariates)),
                                                  domain = saHere,
                                                  destinationPath = inputPath(sim),
                                                  FUN = le(studyAreaFireSense),
                                                  le = le, purge = 7,
                                                  studyAreaFireSense = sim$studyAreaWithSpreadParams,
                                                  action = "update")
      }
    },
    postFitDiagnostics = {
      sim <- makeFitDiagnostics(sim, mod$fitDE)
    },
    crossValidate = {
      sim$spreadFitHeldOut <- crossValidateSpread(sim, mod$covsX1000, mod$thresh)
    },
    plot = {
      DEpop_df <- as.data.frame(sim$DE[[1]]$member$pop)
      colnames(DEpop_df) <- names(P(sim)$lower) ## the names the `run` event gives these coefficients
      sim$fsSpreadFit_hists <- ggplot(tidyr::gather(DEpop_df), aes(value)) +
        geom_histogram(bins = 20) +
        facet_wrap(~key, scales = "free_x") +
        ggtitle(paste("distributions of SpreadFit coefficients for", basename(outputPath(sim))))

      checkPath(file.path(outputPath(sim), currentModule(sim), "figures"), create = TRUE)
      ggsave(file.path(outputPath(sim), currentModule(sim), "figures", "spreadFit_coeffs.png"), sim$fsSpreadFit_hists)

      sim$fsSpreadFit_hists ## show plot in session
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )

  invisible(sim)
}

#' Prepare the inputs of the fit
#'
#' Fills `upper`/`lower` and `mutuallyExclusiveCols` when left at their defaults, checks inputs, and
#' makes `sim$covMinMax_spread`, `sim$lociList` and `mod$covsX1000` (covariates as integers x 1000,
#' restricted to years present in every annual list).
#'
#' @param sim a `simList`.
#' @return the `simList`.
spreadFitPrep <- function(sim) {
  # Mutually Exclusive Columns -- basically no class with biomass or land cover can also be in the
  #   youngAge class. Inside the optimization function, the covariates are set to zero if
  #   youngAge is 1
  mec <- "mutuallyExclusiveCols"
  defaults <- depends(sim)@dependencies$fireSense_SpreadFit@parameters
  defaultMutuallyExclusive <- defaults[defaults$paramName %in% mec, "default"][[1]]
  if (identical(Par[[mec]], defaultMutuallyExclusive)) {
    sp_lcc <- colnames(sim$fireSense_nonAnnualSpreadFitCovariates[[1]])
    sp_lcc <- grep("pixel", sp_lcc, invert = TRUE, value = TRUE)
    P(sim)[[mec]] <- Map(l = Par[[mec]], nam = names(Par[[mec]]), function(l, nam) {
      if (identical(nam, youngAge))
        c(l, sp_lcc)
      else
        l
    })
    message("Mutually exclusive is now:")
    message(Par[[mec]]) # message doesn't show name of list

  }

  ## Fuel biomass arrives from fireSense_dataPrepFit on the log scale (fireSenseUtils::logMinB). The
  ## spread model takes it on the LINEAR scale, divided by a fixed 1e4 -- see
  ## fireSenseUtils::fuelLogToLinear() for why, and why the log is undone here and not at its source.
  ## The input is left as it is; everything below that feeds the fit uses this copy.
  ## fireSense_SpreadPredict applies the same function, recognising a linear fit by covMinMax_spread.
  fuelCols <- fuelColumns(sim$fireSense_nonAnnualSpreadFitCovariates)
  nonAnnualLinear <- lapply(sim$fireSense_nonAnnualSpreadFitCovariates, function(dt) {
    dt <- data.table::copy(dt)
    for (cn in intersect(fuelCols, names(dt))) set(dt, NULL, cn, fireSenseUtils::fuelLogToLinear(dt[[cn]]))
    dt
  })

  # veg coefficients should probably have bounds of 4
  # however youngAge should have an upper limit of zero to prevent self-propagating fires
  # MDC should have a lower limit of zero - drought shouldn't increase spread probability
  ## yearSpreadSD (the per-year random effect) is in the default bounds unless turned off; when one bound
  ## is supplied, the one filled in includes it only if the supplied one does, so the two stay aligned
  fsdBounds <- if (!anyNA(Par$yearSpreadSDBounds)) Par$yearSpreadSDBounds
  upperMissing <- is.null(P(sim)$upper) || any(is.na(P(sim)$upper))
  lowerMissing <- is.null(P(sim)$lower) || any(is.na(P(sim)$lower))
  if (upperMissing && !lowerMissing && !"yearSpreadSD" %in% names(P(sim)$lower)) fsdBounds <- NULL
  if (lowerMissing && !upperMissing && !"yearSpreadSD" %in% names(P(sim)$upper)) fsdBounds <- NULL
  if (is.null(P(sim)$upper) || any(is.na(P(sim)$upper))) {
    P(sim)$upper <- estimateSpreadParams(sim$fireSense_spreadFormula,
                                         sim$fireSense_annualSpreadFitCovariates,
                                         whichBound = "upper", upperAndLower = Par$upperAndLowerVal,
                                         fuelTerms = fuelCols, upperAndLowerFuel = Par$upperAndLowerValFuel,
                                         upperTailBounds = if (identical(Par$link, "logistic3pUpper")) Par$upperTailBounds,
                                         yearSpreadSDBounds = fsdBounds)
  }

  if (is.null(P(sim)$lower) || any(is.na(P(sim)$lower))) {
    ## TODO - figure out the 2-4 piece logistic defaults :S
    P(sim)$lower <-  estimateSpreadParams(sim$fireSense_spreadFormula,
                                          sim$fireSense_annualSpreadFitCovariates,
                                          whichBound = "lower", upperAndLower = Par$upperAndLowerVal,
                                          fuelTerms = fuelCols, upperAndLowerFuel = Par$upperAndLowerValFuel,
                                         upperTailBounds = if (identical(Par$link, "logistic3pUpper")) Par$upperTailBounds,
                                         yearSpreadSDBounds = fsdBounds)
  }
  ## sanity check parameters + inputs
  #cores can be NA for interactive debugging
  stopifnot(
    "parameter 'trace' must be postive" = P(sim)$trace >= 0,
    "parameter 'cores' must be a postive integer" = length(P(sim)$cores) >= 0 || isTRUE(is.na(P(sim)$cores)),
    "each non-annual spreadFit covariate cannot be all zeros" =
      all(sapply(rbindlist(sim$fireSense_nonAnnualSpreadFitCovariates), max) > 0)
  )

  if (!all(names(P(sim)$upper) == names(P(sim)$lower))) {
    stop("please ensure 'upper' and 'lower' params are named with an identical order")
  }

  if (P(sim)$rescaleAll) {
    sim$covMinMax_spread <- deriveCovMinMax(
      annualList = sim$fireSense_annualSpreadFitCovariates,
      nonAnnualList = nonAnnualLinear, fuelCols = fuelCols, fixedRange = Par$covFixedRange
    )
    if (any(is.na(sim$covMinMax_spread))) {
      stop("covMinMax_spread contains NA values. Check upstream for introduction of NAs.")
    }
  }

  if (anyPlotting(Par$.plots)) {
    digASFC <- .robustDigest(sim$fireSense_annualSpreadFitCovariates)
    digNASFC <- .robustDigest(sim$fireSense_nonAnnualSpreadFitCovariates)
    histOuts <- histOfCovariates(annualList = sim$fireSense_annualSpreadFitCovariates,
                         nonAnnualList = nonAnnualLinear)
    Plots(histOuts[["annual"]], filename = "Histograms of AnnualClimateLayers", useCache = "png")
    Plots(histOuts[["nonAnnual"]], filename = "Histograms of FuelLayers", useCache = "png") 
  }

  IDvar <- grep("ID", names(sim$spreadFirePoints[[1]]), value = TRUE) |> setdiff("GID")
  sim$lociList <- makeLociList(ras = sim$rasterToMatch, pts = sim$spreadFirePoints, idsCol = IDvar,
                               yearPrefix = fireSenseUtils::yearTxt)

  keepNames <- intersect(names(sim$fireSense_annualSpreadFitCovariates), names(sim$fireBufferedListDT))
  mod$covsX1000 <- covsX1000AndSetDF(
    annualList = sim$fireSense_annualSpreadFitCovariates[keepNames],
    nonAnnualList = nonAnnualLinear,
    fireBufferedList = sim$fireBufferedListDT[keepNames],
    fireLociList = sim$lociList,
    paramOrder = P(sim)$upper)
  
  namesWithGTZeroRows <- lapply(mod$covsX1000, function(x) names(x[sapply(x, function(y) NROW(y)) > 0]))
  annualDataNames <- grep("nonAnnual", names(namesWithGTZeroRows), invert = TRUE, value = TRUE)
  keepYearsNamed <- table(unname(unlist(namesWithGTZeroRows[annualDataNames]))) == length(annualDataNames)
  keepYears <- names(keepYearsNamed)[keepYearsNamed]
  mod$covsX1000[annualDataNames] <- lapply(mod$covsX1000[annualDataNames], function(x) x[keepYears])

  return(sim)
}

#' Names of the fuel biomass columns among the non-annual covariates
#'
#' As they arrive from `fireSense_dataPrepFit`, fuel biomass columns are on the
#' `fireSenseUtils::logMinB()` scale, whose floor is 3.6; every other non-annual covariate is an
#' indicator or a proportion, at most 1. So a maximum above 1 marks a fuel column, which is the rule
#' `deriveCovMinMax()` always used to find them.
#'
#' @param nonAnnualList list of `data.table`s of non-annual covariates, as supplied to the module.
#' @return character vector of column names.
fuelColumns <- function(nonAnnualList) {
  dt <- rbindlist(nonAnnualList)
  cols <- setdiff(colnames(dt), "pixelID")
  cols[vapply(cols, function(cn) max(dt[[cn]], na.rm = TRUE) > 1, logical(1))]
}

#' Minimum and maximum of each covariate, for rescaling
#'
#' Fuel biomass columns all get `fireSenseUtils::fuelLinearRange`, `c(0, 1e4)`: a fixed range, not
#' the data's, so that rescaling is `biomass / 1e4` in every polygon and in every predicted year.
#'
#' @param annualList list of `data.table`s of annual covariates, one per year.
#' @param nonAnnualList list of `data.table`s of non-annual covariates, fuel biomass on the LINEAR scale.
#' @param fuelCols names of the fuel biomass columns, from [fuelColumns()].
#' @param fixedRange named list of `c(min, max)` that replace the data's range for those covariates.
#' @return `data.table` with 2 rows (min, max) and one column per covariate.
deriveCovMinMax <- function(annualList, nonAnnualList, fuelCols, fixedRange = list()) {

  nonAnnRescales <- rbindlist(nonAnnualList)
  vals1 <- setdiff(colnames(nonAnnRescales), "pixelID")

  #Biomass columns should be normalized together
  #else 140 Mg/ha pine is treated the same as e.g. 80 Mg/ha White spruce
  # assuming those were the fuel classes and respective maximum observed biomass
  minMax <- nonAnnRescales[, lapply(.SD, range), .SDcols = vals1]
  names(minMax) <- vals1
  biomassCols <- intersect(vals1, fuelCols)
  sharedRange <- fireSenseUtils::fuelLinearRange

  biomassMax <- minMax[, lapply(.SD,FUN = function(x){return(sharedRange)}), .SDcols = biomassCols]

  #override the min and max
  coverCols <- setdiff(vals1, biomassCols)
  covMinMax1 <- cbind(biomassMax, minMax[, .SD, .SDcols = coverCols])
  #just in case covMinMax must respect original order
  setcolorder(covMinMax1, vals1)

  #annual covariates (climate/youngAge)
  annRescales <- rbindlist(annualList, fill = TRUE)
  vals2 <- setdiff(colnames(annRescales), c("buffer", "pixelID", "ids"))
  covMinMax2 <- annRescales[, lapply(.SD, range), .SDcols = vals2]
  covMinMax <- cbind(covMinMax1, covMinMax2)
  ## a fixed range is the same in every polygon and every predicted year; the data's range is neither
  for (cn in intersect(names(fixedRange), names(covMinMax))) {
    stopifnot(length(fixedRange[[cn]]) == 2L, is.numeric(fixedRange[[cn]]), fixedRange[[cn]][2] > fixedRange[[cn]][1])
    ## an NA in the data must still reach spreadFitPrep()'s check, so a range that is NA is left NA
    if (!anyNA(covMinMax[[cn]])) set(covMinMax, NULL, cn, as.numeric(fixedRange[[cn]]))
  }
  covMinMax
}

#' Histograms of the spread-fit covariates
#'
#' @param annualList list of `data.table`s of annual covariates, one per year; must have `CMDsm`.
#' @param nonAnnualList list of `data.table`s of non-annual (log biomass) covariates.
#' @return list of two `ggplot`s: `annual` (`CMDsm` by year) and `nonAnnual` (biomass by fuel and year).
histOfCovariates <- function(annualList, nonAnnualList) {
  annualCols <- colnames(annualList[[1]])
  annualColsToPlot <- setdiff(annualCols, "pixelID")
  nonAnnualCols <- colnames(nonAnnualList[[1]])
  nonAnnualColsToPlot <- setdiff(nonAnnualCols, "pixelID")

  yr <- "year"

  nplots <- length(annualColsToPlot) * length(annualList) +
    length(nonAnnualColsToPlot) * length(nonAnnualList)
  ncols <- ceiling(sqrt(nplots))
  nrows <- ceiling(nplots/ncols)
  par(mfrow = c(ncols, nrows))
  ann <- rbindlist(annualList, idcol = yr, use.names = TRUE, fill = TRUE)
  set(ann, NULL, "pixelID", NULL)

  # 1. Create a clean environment
  clean_env <- new.env(parent = .GlobalEnv)
  # 2. "Inject" only the necessary objects
  clean_env$ann <- ann
  clean_env$yr  <- yr
  # 3. Evaluate the plot inside that environment
  annHists <- local({
    ggplot(ann) + 
      geom_histogram(aes_string("CMDsm")) +
      facet_wrap(yr) + 
      ggplot2::theme_bw()
  }, envir = clean_env)

  nonAnn <- rbindlist(nonAnnualList, idcol = yr, use.names = TRUE, fill = TRUE)
  set(nonAnn, NULL, "pixelID", NULL)

  v <- "LogBiomass"
  Fue <- "Fuel"
  nonAnnDT <- melt(
    nonAnn,
    id.vars = yr,              # keep year as an identifier
    variable.name = Fue,        # new column holding the old column names
    value.name = v           # numeric values
  )
  whMin <- which(nonAnnDT[[v]] == min(nonAnnDT[[v]]))
  set(nonAnnDT, NULL, v, exp(nonAnnDT[[v]]))
  set(nonAnnDT, whMin, v, 1)

  # 1. Create a clean environment
  clean_env <- new.env(parent = .GlobalEnv)
  # 2. "Inject" only the necessary objects
  clean_env$nonAnnDT <- nonAnnDT
  clean_env$yr  <- yr
  clean_env$Fue  <- Fue
  clean_env$v <- v
  
  nonAnnHists <- local({
    ggplot(nonAnnDT, aes(x = .data[[v]])) +
    geom_histogram(bins = 20, color = "white") +
    facet_grid(
      rows = vars(.data[[yr]]),      # one strip per row, showing year
      cols = vars(.data[[Fue]]),     # one strip per column, showing fuel
      labeller = labeller(.multi_line = FALSE)  # cleaner strip labels
    ) +
    scale_x_log10(                     # compress the axis but keep original values
      breaks = scales::breaks_log(n = 6),                   # nice log breaks
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    labs(
      x = "Biomass (log-compressed axis)",
      y = "Num Pixels",
      title = "Histogram of Biomass by Fuel × Year"
    ) +
    theme_bw() +
    theme(
      strip.placement = "outside",   # move strips outside the panels
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text.y.left = element_text(angle = 0)  # readable vertical strips
    )}, envir = clean_env)

  list(annual = annHists, nonAnnual = nonAnnHists)
}


#' A seed that depends only on the ELF
#'
#' The SNLL threshold becomes `thresh` in `runDEoptim()`, so it is part of every cached DEoptim
#' generation's key. Drawing the seed by chance meant a single cache miss on `estimateThreshold`
#' re-drew the threshold and invalidated every cached generation for that ELF. Keyed on the ELF, a
#' miss costs only the threshold estimate.
#'
#' @param elf character; `sim$.ELFind` (which falls back to `sim$.runName`).
#' @return a single integer in `1:1e6`.
.elfSeed <- function(elf) {
  stopifnot(is.character(elf), length(elf) == 1L, nzchar(elf))
  ## a stable digest of the identifier, folded into DEoptim's seed range; no RNG involved, so it is
  ## identical across sessions, machines and R versions
  bytes <- utils::head(as.integer(charToRaw(elf)), 64L)
  1L + as.integer(sum(bytes * seq_along(bytes) * 7919) %% 1e6)
}

#' Set `mod$thresh`, the SNLL fire-size threshold of the objective function
#'
#' Uses `SNLL_FS_thresh` if supplied; otherwise calibrates it with a cached
#' `runSpreadWithoutDEoptim()` call.
#'
#' @param sim a `simList`, after `spreadFitPrep()`.
#' @return the `simList`.
estimateSNLLThresholdPostLargeFires <- function(sim) {
  thresh <- if (is.null(Par$SNLL_FS_thresh) || is.na(Par$SNLL_FS_thresh)) {
    message("Estimating threshold for inside .objFunSpreadFit -- This can be supplied via SNLL_FS_thresh parameter")

    # Took 50 minutes using 10 cores for Taiga studyArea
    runSpreadWithoutDEoptim(
      iterThres = P(sim)$iterThresh,
      lower = P(sim)$lower, upper = P(sim)$upper,
      fireSense_spreadFormula = sim$fireSense_spreadFormula,
      flammableRTM = sim$rasterToMatch,
      mutuallyExclusive =  P(sim)$mutuallyExclusiveCols,
      doObjFunAssertions = P(sim)$doObjFunAssertions,
      annualDTx1000 = mod$covsX1000$annualDTx1000,
      nonAnnualDTx1000 = mod$covsX1000$nonAnnualDTx1000,
      fireBufferedListDT = mod$covsX1000$fireBufferedListDT,
      historicalFires = mod$covsX1000$historicalFires,
      covMinMax = sim$covMinMax_spread,
      formulaToFit = sim$fireSense_spreadFormula,
      objfunFireReps = P(sim)$objfunFireReps,
      tests = P(sim)$DEoptimTests,
      mode = Par$mode,
      ## Deterministic per ELF: the threshold feeds every DEoptim generation's cache key, so a
      ## re-drawn threshold discards the whole fit's cached generations (2026-09-16: 1236 -> 1416
      ## cost ~17 h). `seed` is an argument, so it is in this Cache key too -- which is what
      ## test-thresholdCacheKey.R asks for: nothing that changes the result is omitted.
      seed = .elfSeed(sim$.ELFind),
      escapeSizeHa = escapeSizeHaOrNULL(P(sim)$escapeSizeHa),
      ## the same objective as the fit (R/fitSpread.R), or the threshold is calibrated on another one
      weighted = P(sim)$weighted, sizeLik = P(sim)$sizeLik, sizeLikDf = P(sim)$sizeLikDf,
      adWeight = P(sim)$adWeight, link = spreadLink(P(sim)$link),
      jumpTries = P(sim)$jumpTries, jumpMeanDist = P(sim)$jumpMeanDist,
      yearAreaWeight = P(sim)$yearAreaWeight, areaDistWeight = P(sim)$areaDistWeight,
      maxFireSpread = P(sim)$maxFireSpread) |>
      ## Nothing is omitted from the key, because both of the arguments that used to
      ## be omitted change the result.
      ##
      ## `mode` selects which branch of runSpreadWithoutDEoptim runs, and the branches
      ## return different types -- the fitting branch returns the numeric threshold
      ## (R/runSpreadWithoutDEoptim.R), the debug branch ends in a `for` loop with
      ## no return and yields NULL. Omitting it let a debug-mode result be served to a
      ## fit-mode caller and the reverse, which is how `mod$thresh` came back as a
      ## character and the objective function died at `round(thresh, 0)`.
      ##
      ## `objfunFireReps` becomes `Nreps` in the objective function, so it changes the
      ## threshold's *value* rather than its type: a threshold calibrated at 5
      ## replicates would be served to a caller asking for 25, with nothing to show
      ## that it had been.
      Cache()
  } else {
    P(sim)$SNLL_FS_thresh
  }
  mod$thresh <- thresh
  return(sim)
}

#' Default `upper` or `lower` bounds for DEoptim
#'
#' Covariate coefficients get +/- `upperAndLower`, except annual covariates (lower bound 0) and
#' `youngAge` (upper bound 0). The three logistic parameters get fixed bounds.
#'
#' @param fireSense_spreadFormula character; the spread formula.
#' @param anyAnnualCovariates list of annual covariate `data.table`s; only column names are used.
#' @param whichBound "upper" or "lower".
#' @param upperAndLower numeric; absolute bound for covariate coefficients.
#' @param upperTailBounds numeric; if not `NULL`, the bounds of `upperTail1`, which is added after
#'   `inflectionPoint1` (link "logistic3pUpper").
#' @return named numeric vector: `maxAsymptote`, `hillSlope1`, `inflectionPoint1`, `upperTail1` (with
#'   `upperTailBounds`), then formula terms.
estimateSpreadParams <- function(fireSense_spreadFormula, anyAnnualCovariates, whichBound,
                                 upperAndLower, fuelTerms = character(), upperAndLowerFuel = upperAndLower,
                                 upperTailBounds = NULL, yearSpreadSDBounds = NULL) {

  stopifnot(whichBound %in% c("upper", "lower"))

  formulaTerms <- attr(terms(as.formula(fireSense_spreadFormula, env = .GlobalEnv)), "term.labels")
  termLength <- length(formulaTerms)
  if (whichBound == "upper") {
    newParams <- rep(upperAndLower, times = termLength)
  } else {
    newParams <- rep(-(upperAndLower), termLength)
  }
  newParams <- as.vector(newParams)
  ## fuel biomass is biomass / 1e4, not [0, 1], so its coefficients need a wider box
  newParams[formulaTerms %in% fuelTerms] <- if (whichBound == "upper") upperAndLowerFuel else -upperAndLowerFuel
  whAnnual <- formulaTerms %in% colnames(anyAnnualCovariates[[1]])
  whYA <- formulaTerms[whAnnual] %in% youngAge
  newParams[whAnnual] <- ifelse(whichBound == "upper", upperAndLower, 0)
  newParams[whAnnual][whYA] <- ifelse(whichBound == "upper", 0, -(upperAndLower))

  names(newParams) <- formulaTerms

  if (whichBound == "upper") {
    newParams <- c("maxAsymptote" = 0.276, "hillSlope1" = 2, "inflectionPoint1" = 4, newParams)
  } else {
    newParams <- c("maxAsymptote" = 0.25, "hillSlope1" = 0.2, "inflectionPoint1" = 0.1, newParams)
  }
  ## the objective takes the logistic parameters by position, so upperTail1 must be the 4th
  if (!is.null(upperTailBounds))
    newParams <- append(newParams, c(upperTail1 = if (whichBound == "upper") max(upperTailBounds)
                                                  else min(upperTailBounds)), after = 3L)
  ## the per-year random effect's sd; the objective finds it by name and requires it LAST
  if (!is.null(yearSpreadSDBounds))
    newParams <- c(newParams, yearSpreadSD = if (whichBound == "upper") max(yearSpreadSDBounds)
                                             else min(yearSpreadSDBounds))

  return(newParams)
}

#' Default inputs
#'
#' Supplies `studyArea`, `rasterToMatch`, `.ELFind` (from `.runName`) and
#' `spreadFitAdditionalColNames` when absent; stops if `fireSense_spreadFormula` is absent.
#'
#' @param sim a `simList`.
#' @return the `simList`, invisibly.
.inputObjects <- function(sim) {
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dPath,
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }

  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"),
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif",
                               destinationPath = dPath,
                               overwrite = TRUE, filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID",
                                            "useCloud", "overwrite", "filename2"))
  }

  if (!suppliedElsewhere(object = ".ELFind", sim = sim)) {
    # Backwards compatibility: before `.ELFind` was declared, the ledger was keyed
    # on `.runName`. Keep that behaviour for pipelines that do not supply a polygon
    # id, but say so, because a `.runName` that encodes a whole scenario produces a
    # ledger key nothing else can match.
    sim$.ELFind <- sim$.runName
    message(currentModule(sim), ": `.ELFind` not supplied; keying the shared fit ",
            "ledger on `.runName` ('", sim$.runName, "'). Supply `.ELFind` if this ",
            "is not the polygon's identifier.")
  }

  if (!suppliedElsewhere("fireSense_spreadFormula", sim)) {
    stop("fireSense_spreadFormula must be supplied.")
  }

  if (!suppliedElsewhere("spreadFitAdditionalColNames")) {
    sim$spreadFitAdditionalColNames <- fireSenseUtils::spreadFitAdditionalColNamesTxt
  }

  return(invisible(sim))
}

## name of the young-age covariate
youngAge <- fireSenseUtils::youngAgeTxt
