---
title: "fireSense_SpreadFit Manual"
subtitle: "v.1.0.6.9008"
date: "Last updated: 2026-09-24"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_fireSense_SpreadFit.bib
link-citations: true
always_allow_html: true
---

# fireSense_SpreadFit Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-SpreadFit) *fireSense_SpreadFit*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Tati Micheletti <tati.micheletti@gmail.com> [aut], Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut], Jean Marchal <jean.d.marchal@gmail.com> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Fit statistical models that can be used to parameterize the fire spread component of simulation models (e.g., fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019]).
This module implement a Pattern Oriented Modelling (POM) approach to derive spread probabilities from final fire sizes. <!-- TODO add citation for POM -->
Spread probabilities can vary between pixels, and thus reflect local heterogeneity in environmental conditions.

The fit is a differential evolution search (`DEoptim`, run by `fireSenseUtils::runDEoptim()` on a cluster built by the `clusters` package).
Each candidate parameter set is scored by simulating the historical fires and comparing simulated with observed fire sizes (`fireSenseUtils::.objfunSpreadFit()`).
The 5 best parameter sets, and the covariate ranges used to rescale the covariates, are written as one row per polygon to a shared "fit ledger" on Google Drive (`spreadFitFilename` in `spreadFitGoogleDriveFolder`), keyed by `.ELFind`.
If the ledger already holds a row for the polygon, the module does nothing unless `refitExisting = TRUE`.
By default (`stopIfNoPreRunFit = TRUE`) the module stops rather than start a fit; set it to `FALSE` to fit.

#### `refitExisting`

`refitExisting = TRUE` forces a fit for this polygon even when the ledger already holds a row for it.
Use it when the fit's INPUTS have changed -- new land cover, new vegetation parameters, a new objective
function -- so the stored row is stale and the polygon must be fitted again.

`refitExisting` **overrides** `stopIfNoPreRunFit`: with `refitExisting = TRUE`, `init` schedules the fit
rather than stopping, whatever `stopIfNoPreRunFit` is set to. Setting `stopIfNoPreRunFit = FALSE` is only
needed when the polygon has no ledger row.

`refitExisting` is intended **for developers** who have access to **at least 40 cores**: it triggers a
full DEoptim run (see `cores` and `nCoresNeeded`), which is not practical on a small machine.

### Module inputs and parameters

The covariate tables, fire buffers, fire points and formula are made by `fireSense_dataPrepFit`.
`fireSense_spreadFormula` must be supplied; `.ELFind` defaults to `.runName`.

Table \@ref(tab:moduleInputs-fireSense-SpreadFit) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-SpreadFit)(\#tab:moduleInputs-fireSense-SpreadFit)List of (ref:fireSense-SpreadFit) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .runName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Some descriptive, short name for this fitting, e.g., ELF14.1 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .ELFind </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Identifier of the polygon being fit, e.g. '6.1.1'. This becomes the `polygonID` of the row this module writes to the shared cloud fit ledger (`spreadFitFilename` in `spreadFitGoogleDriveFolder`), which `fireSense_dataPrepFit` matches against the polygon ids carried by `rasterToMatchELF`. It must therefore be the polygon's identity, not a run label: `.runName` encodes the whole scenario (climate period, GCM, SSP, rep) in some projects, and keying the ledger on it writes rows no other run can find and trips dataPrepFit's id match. Defaults to `.runName` for backwards compatibility. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireBufferedListDT </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of data.tables with fire id, pixelID, and buffer status </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_annualSpreadFitCovariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of climate and/or veg covariates, burn status, polyID, and pixelID </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_nonAnnualSpreadFitCovariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of veg covariates, burn status, polyID, and pixelID </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitAdditionalColNames </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Names of the list-columns of the ledger row. Reset to `fireSenseUtils::spreadFitAdditionalColNamesTxt` if different. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_spreadFormula </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> a formula that contains the annual and non-annual covariates e.g. `~ 0 + MDC + class2 + class3 + youngAge`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parsKnown </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Optional vector of known parameters, e.g., from a previous `DEoptim` run. If this is supplied, then 'mode' will be automatically converted to 'debug' </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> template raster for study area </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFirePoints </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> list of `sf` points, one element per year, of fire ignition locations </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Polygon being fit; its geometry and crs go in the ledger row. Defaults to NWT. </td>
   <td style="text-align:left;"> https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-SpreadFit))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-SpreadFit)(\#tab:moduleParams-fireSense-SpreadFit)List of (ref:fireSense-SpreadFit) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> characte.... </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Plot types passed to `Plots()`, e.g. 'png' or 'screen'; NULL or NA for none. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotSize </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> 1600, 2000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> List specifying height and width of plotting device (in pixels) used to plot DEoptim histograms when `visualizeDEoptim` is TRUE. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> when to start this module? By default, the start time of the simulation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical,.... </td>
   <td style="text-align:left;"> init </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cores </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `cores` in `fireSenseUtils::runDEoptim()`: a number of local cores, or a character vector of machine names, one element per core wanted on that machine. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DEoptimTests </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SNLL_FS </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Currently either `'SNLL_FS'` or `'adTest'` or a length 2 character vector of both. Passed to `tests` in `fireSenseUtils::.objfunSpreadFit()`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> doObjFunAssertions </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `fireSenseUtils::.objfunSpreadFit()`; TRUE runs diagnostics but is slower; FALSE for operational runs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> initialpop </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A numeric matrix of dimensions `NCOL = length(lower)` and `NROW = NP`. This will be passed into DEoptim through `control$initialpop = P(sim)$initialpop` if it is not NULL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> iterDEoptim </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> integer defining the maximum number of iterations allowed (DEoptim optimizer). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> iterStep </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> DEoptim runs its `iterDEoptim` iterations in blocks of this many; each block is cached and, if `visualizeDEoptim` is a path, plotted. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> iterThresh </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 96 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Number of random parameter sets tried when calibrating `SNLL_FS_thresh`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> libPathDEoptim </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> /home/ru.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Absolute path specifying R package directory location to use when running DEotpim. NOTE: this path must be read/write accessible on ALL machines used for fitting (identified in cores). Therefore, it's best use a directory in your user's `~` directory. If the directory does not exist at this path, will attempt to create it. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lower </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> see `?DEoptim`. Lower limits for the logistic function parameters (lower bound, upper bound, slope, asymmetry) and the statistical model parameters (named in the order they appear in the formula). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> maxFireSpread </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.28 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Maximum fire spread average to be passed to the `.objFun`. This puts an upper limit on `spreadProb` during optimization. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> link </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> logistic3p </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The spread link. 'logistic3p', or 'logistic3pUpper': the same curve with Stukel's upper tail, one more parameter `upperTail1` that changes only how the curve approaches its ceiling (`fireSenseUtils::logistic3pUpper()`). Its default bounds are `upperTailBounds`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mode </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fit </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Options: debug, fit, visualize, validate. Can use multiples. 'debug' runs the objective function with visuals instead of DEoptim; 'fit' runs DEoptim; 'visualize' adds the `debug` and `plot` events after the fit; 'validate' adds `crossValidate`, two more fits, each on half the years, predicting the other half (`sim$spreadFitHeldOut`). Validation never writes the ledger. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> profileReps </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> After the fit, each covariate coefficient in turn is set to 0 and to 5 values across the final population, the others held at the best member, and each point is evaluated this many times (`fireSenseUtils::profileCoefficients()`). About `6 nCoefficients profileReps` evaluations, on the fit's workers. It decides which coefficients are identified in isolation (`sim$spreadFitIdentifiability`). 0 skips it. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mutuallyExclusiveCols </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> c("class.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> a named list of mutually exclusive covariates - see `fireSenseUtils::makeMutuallyExclusive` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nCoresNeeded </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> How many workers to request for the DEoptim cluster. This IS the population size: `clusters::clusterSetup()` sets NP to the workers it builds. `NULL` leaves `fireSenseUtils::runDEoptim()`'s default of 10 per estimated parameter. A generation costs the slowest of NP evaluations and that barely falls as NP falls, so a smaller NP buys throughput by allowing more fits at once rather than by shortening generations (measured 2026-09-16). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simulateMembers </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> After the fit, this many best members simulate the observed fires without the size cap, for `sim$spreadFitSizes` and `sim$spreadFitLinkSaturation`; also the members each `crossValidate` fold predicts with. 0 skips it after the fit. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sizeLik </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Likelihood of fire size in the objective, 'kde' or 't', passed to `fireSenseUtils::runDEoptim()`. 't' with `weighted = FALSE` predicted held-out years best in the 2026-09-21 cross-validation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sizeLikDf </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Degrees of freedom of the 't' size likelihood. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> weighted </td>
   <td style="text-align:left;"> logical&amp;#124;.... </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Weight of each fire in the size likelihood: FALSE (none), TRUE (log size) or 'sqrt'. Passed to `fireSenseUtils::runDEoptim()`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> adWeight </td>
   <td style="text-align:left;"> characte.... </td>
   <td style="text-align:left;"> auto </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Weight of the Anderson-Darling term against the size likelihood; 'auto' is `fireSenseUtils::adWeightAuto()`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> objFunCoresInternal </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Integer defining the number of cores to pass to `mcmapply(mc.cores = ...)` This will fork this many to do the years loop internally. This would be in addition to `cores` and is effecively a multiplier. The computer needs to have `cores objFunCoresInternal` threads or it will stall. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> objfunFireReps </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> integer defining the number of replicates the objective function will attempt each fire. Since the default approach is using `EnvStats::demp`, it should be at least 100 to get a smooth distribution for a likelihood. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rep </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> An optional integer indicating which replicate run this represents. This is used to identify unique runs of `runDEoptim`, from a Cache perspective. For example, if this module is run twice with all the same data, Cache will think that the second run should recover the cache result, unless this `rep` is modified </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .c </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.5 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> the `c` argument passed to DEoptim.control </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DEoptimControl </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Further `DEoptim.control()` settings, e.g. `list(CR = 0.7, F = 0.6)`, passed through `fireSenseUtils::runDEoptim()` to DEoptim. Names must be `DEoptim.control()` arguments. `strategy`, `trace`, `initialpop` and `.c` have their own parameters; `NP` is the number of workers the cluster gets. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rescaleAll </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> rescale covariates for `DEOptim` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitGoogleDriveFolder </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> https://.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Google Drive folder url holding the shared fit ledger (`spreadFitFilename`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitFilename </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fireSens.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> File name of the shared fit ledger: an `sf` object with one row of fitted parameters per polygon. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> strategy </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `DEoptim.control` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SNLL_FS_thresh </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Threshold multiplier used in objective function SNLL fire size test. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> refitExisting </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> FOR DEVELOPERS ONLY: a re-fit is a full DEoptim run and is only practical with access to at least 40 cores. Fit this polygon even when the ledger already holds parameters for it. A ledger row normally means the fit is done, and the run event skips it. Set this when the fit's INPUTS have changed -- new land cover, new vegetation parameters, a new objective -- so the stored row is stale and the polygon must be fitted again. When TRUE it OVERRIDES `stopIfNoPreRunFit`: `init` schedules the fit instead of stopping. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stopIfNoPreRunFit </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If TRUE, `init` stops with an error when this polygon would have to be fitted, instead of fitting it. Ignored when `refitExisting` is TRUE. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trace </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> non-negative integer. If &gt; 0, tracing information on the progress of the optimization are printed every `trace` iteration. Default is 1, i.e. every iteration. Setting to 0 turns off tracing. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upper </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> see `?DEoptim`. Upper limits for the logistic function parameters (lower bound, upper bound, slope, asymmetry) and the statistical model parameters (named in the order they appear in the formula). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useCache_DE </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> should `DEoptim` use `Cache`? to do multiple independent runs, use FALSE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> verbose </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. With increasing number, more verbosity. Level 1 is normal reproducible (e.g., Cache), level 2 includes objective function e.g., print median of spreadProb during calculations </td>
  </tr>
  <tr>
   <td style="text-align:left;"> visualizeDEoptim </td>
   <td style="text-align:left;"> Path </td>
   <td style="text-align:left;"> /tmp/Rtm.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Directory where `runDEoptim` saves parameter plots after each `iterStep` block. Reset to `figurePath(sim)` unless its last folder is the module name. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> covFixedRange </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> c(0, 100.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of `c(min, max)`: covariates rescaled with this FIXED range and not with the range of this polygon's data. `CMDsm = c(0, 100)` makes the covariate CMDsm / 100 in every polygon. With the data's range, 1 meant a CMDsm of 104 in one polygon and 297 in another, so the coefficient could not be compared across polygons, and a polygon that never gets dry stretched its small range over [0, 1]. Names not among the covariates are ignored. `fireSense_SpreadPredict` rescales with the stored `covMinMax_spread`, so it follows. CMD, CMDsp and cumMDC (also mm) are the other candidates of fireSense_dataPrepFit's `spread = 'auto'`, so an ELF that picks one of them gets the same fixed scale. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yearSpreadSDBounds </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0, 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Bounds of `yearSpreadSD`, the sd of a per-year random effect on logit spread probability (`fireSenseUtils::.objfunSpreadFit()`), when `lower`/`upper` are not supplied. A seasonal departure: each year draws one eps, so all of a year's fires burn hotter or cooler together, which widens the simulated fire-size distribution. `NA` turns it off. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upperTailBounds </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> -1, 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Bounds of `upperTail1` when `link` is 'logistic3pUpper' and `lower`/`upper` are not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upperAndLowerVal </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Bound given to each covariate coefficient (`upper` = this, `lower` = minus this) when `upper` or `lower` is not supplied. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upperAndLowerValFuel </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> As `upperAndLowerVal`, for the fuel biomass covariates. They are biomass / 1e4, so their coefficients are larger than those of covariates rescaled to [0, 1]: with a bound of 9 the fuel coefficient sat on the bound (fitted 4.57 in a +-9 box on the log scale, 11.07 once widened; 31.7 on the linear scale). 60 did not bind in any of 36 fits. </td>
  </tr>
</tbody>
</table>

### Events

- `init`: schedules `spreadFitPrepare` and, if the polygon needs a fit, `estimateThreshold` then `run` (or `debug` when `mode` includes `"debug"`; `debug` and `plot` after `run` when it includes `"visualize"`);
- `spreadFitPrepare`: sets default `lower`/`upper`, computes `covMinMax_spread` and `lociList`, converts covariates to integers (x 1000);
- `estimateThreshold`: uses `SNLL_FS_thresh`, or calibrates it from `iterThresh` random parameter sets (cached, with a seed derived from `.ELFind`);
- `run`: runs DEoptim (cached per `iterStep` block) and writes the polygon's row to the ledger;
- `debug`: evaluates the objective function without DEoptim;
- `plot`: histograms of the final population;

### Plotting

With `.plots` set, histograms of the annual and non-annual covariates.
During the fit, `runDEoptim()` saves parameter histograms and trace plots to `visualizeDEoptim` after each `iterStep` block.

### Saving

The fit's row in the cloud ledger, and the DEoptim cache entries. Nothing else is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-SpreadFit)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-SpreadFit)(\#tab:moduleOutputs-fireSense-SpreadFit)List of (ref:fireSense-SpreadFit) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> covMinMax_spread </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> `data.table` of covariates min and max </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DE </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> list of `DEoptim` objects, one per `iterStep` block, ordered by best objective value </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaWithSpreadParams </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Rows of the shared fit ledger that intersect `studyArea`, including the row this fit writes: `studyArea` geometry, `polygonID`, and list-columns named by `spreadFitAdditionalColNames` (the 5 best parameter sets are in `params`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fsSpreadFit_hists </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> histograms of each parameter used in `DEoptim` fitting. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lociList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> per-year `data.table`s of fire start cells and sizes, from `fireSenseUtils::makeLociList()` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitConvergence </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> The objective across the fit's generations (`fireSenseUtils::fitConvergence()`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitRescore </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> The final population, one row per member, with the mean and sd of its replicated re-scores (`reMean`, `reSD`). The ledger's parameter sets are the best of these. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitIdentifiability </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> One row per covariate coefficient: how tightly the population pins it (`fireSenseUtils::coefIdentifiability()`) and, with `profileReps &gt; 0`, whether dropping it worsens the fit; `identified` = identified in isolation (`fireSenseUtils::identifiedInIsolation()`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitProfile </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> The one-at-a-time profile around the best member (`fireSenseUtils::profileCoefficients()`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitSizes </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Observed against simulated fire sizes of the fitted years, without the size cap (`fireSenseUtils::scoreFireSizes()`): bias, error, quantiles. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitLinkSaturation </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Per member, the share of pixel-years at the spread-probability ceiling and the quantiles of spread probability (`fireSenseUtils::linkSaturation()`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> spreadFitHeldOut </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> mode 'validate' only: `sims`, the held-out years simulated from the fit to the other years (column `fold`), and `score`, from `fireSenseUtils::scoreFireSizes()`. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Inputs come from `fireSense_dataPrepFit`. `fireSense_SpreadPredict` uses `studyAreaWithSpreadParams` (the ledger rows) to predict spread probabilities.

### Usage


``` r
## in a project that also runs fireSense_dataPrepFit
params <- list(fireSense_SpreadFit = list(
  stopIfNoPreRunFit = FALSE,          # allow a fit to start
  cores = rep(c("hostA", "hostB"), each = 20), # host name repeated once per worker; or a number for localhost
  nCoresNeeded = 40,                  # = DEoptim population size (NP)
  iterDEoptim = 500, iterStep = 25,
  mode = "fit"
))
objects <- list(.ELFind = "6.1.1")    # polygon id used as the ledger key
```

### Getting help

- <https://github.com/PredictiveEcology/fireSense_SpreadFit/issues>

## References

<!-- autogenerated from bibligraphy -->
