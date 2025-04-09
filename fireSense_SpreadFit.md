---
title: "fireSense_SpreadFit Manual"
subtitle: "v.1.0.1"
date: "Last updated: 2025-04-08"
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

Jean Marchal <jean.d.marchal@gmail.com> [aut], Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Tati Micheletti <tati.micheletti@gmail.com> [aut], Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Fit statistical models that can be used to parameterize the fire spread component of simulation models (e.g., fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019]).
This module implement a Pattern Oriented Modelling (POM) approach to derive spread probabilities from final fire sizes. <!-- TODO add citation for POM -->
Spread probabilities can vary between pixels, and thus reflect local heterogeneity in environmental conditions.

### Module inputs and parameters

Describe input data required by the module and how to obtain it (e.g., directly from online sources or supplied by other modules)
If `sourceURL` is specified, `downloadData("fireSense_SpreadFit", "..")` may be sufficient.

Table \@ref(tab:moduleInputs-fireSense-SpreadFit) shows the full list of module inputs.

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> fireBufferedListDT </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of data.tables with fire id, pixelID, and buffer status </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> RTM without ice/rocks/urban/water. Flammable map with 0 and 1. </td>
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
   <td style="text-align:left;"> fireSense_spreadFormula </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> a formula that contains the annual and non-annual covariatese.g. `~ 0 + MDC + class2 + class3 + youngAge`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parsKnown </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Optional vector of known parameters, e.g., from a previous `DEoptim` run.If this is supplied, then 'mode' will be automatically converted to 'debug' </td>
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
   <td style="text-align:left;"> list of spatial points objects representing annual fire centroids </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Study area for the prediction. Defaults to NWT. </td>
   <td style="text-align:left;"> https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-SpreadFit))


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> Should outputs be plotted? </td>
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
   <td style="text-align:left;"> .runInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Interval between two runs of this module, expressed in units of simulation time. By default, NA, which means that this module only runs once per simulation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. When to start saving output to a file. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> init </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cacheId_DE </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> An optional character string representing a `cacheId` to recover from the Cache. After `reproducible &gt;= 2.0.10.9016`, this can be set to 'previous', meaning the Cache will get the previous item in the Cache that matches the `P(sim)$rep` (see that param) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cloudFolderID_DE </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `cloudFolderID` in the `Cache(DEoptim...)` call. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cores </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> non-negative integer. Defines the number of logical cores to be used for parallel computation. The default value is 1, which disables parallel computing. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DEoptimTests </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SNLL_FS </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Currently either `'SNLL_FS'` or `'adTest'` or a length 2 character vector of both. These are passed to `.objFunSpreadFit` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> doObjFunAssertions </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This is passed to `objFunSpreadProb`; TRUE will do some diagnostics but is slower; FALSE for operational runs </td>
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
   <td style="text-align:left;"> Passed to runDEoptim </td>
  </tr>
  <tr>
   <td style="text-align:left;"> iterThresh </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 96 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Number of iterations for automated threshold calibration. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> libPathDEoptim </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> /Users/a.... </td>
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
   <td style="text-align:left;"> optional. Maximum fire spread average to be passed to the `.objFun`.This puts an upper limit on `spreadProb` during optimization. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mode </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fit </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Options: debug, fit, visualize. Can use multiples. 'debug' will trigger running of the objective function with visuals; 'fit' will trigger DEoptim; 'visualize' will trigger visualization after DEoptim. For 'visualize', DE object must be findable, either in sim, on disk or a cloud URL. These last 2 can be specified with `urlDEOptimObject` param. </td>
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
   <td style="text-align:left;"> NP </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Number of Populations. See `?DEoptim.control`. </td>
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
   <td style="text-align:left;"> onlyLoadDEOptim </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. If TRUE, the module will skip the fitting altogether and will only load the latest uploaded version of the `DEOptim` object </td>
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
   <td style="text-align:left;"> rescaleAll </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> rescale covariates for `DEOptim` </td>
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
   <td style="text-align:left;"> urlDEOptimObject </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> https://.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. If `onlyLoadDEOptim == TRUE`, you can pass the url to the `DEOptim` object. The default is the object from the run on 11JUN20 from the `logistic2p` </td>
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
   <td style="text-align:left;"> useCloud_DE </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `useCloud` in the `Cache(DEoptim...)` call </td>
  </tr>
  <tr>
   <td style="text-align:left;"> verbose </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Should it calculate and print median of spread Probability during calculations? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> visualizeDEoptim </td>
   <td style="text-align:left;"> Path </td>
   <td style="text-align:left;"> /private.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to runDEoptim. This makes histographs at each iterStep and saves them to this path </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upperAndLowerVal </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This will be given to the upper and -lower values if not supplied by user </td>
  </tr>
</tbody>
</table>

### Events

<!-- TODO -->
- Module initialization;
- Prepare spread fit;
- debug;
- estimate threshold;
- run spreadFit;
- retrieve DEoptim;
- make `makefireSense_SpreadFitted`;
- plot;

### Plotting

Write what is plotted.

### Saving

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-SpreadFit)).

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
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
   <td style="text-align:left;"> `DEOptim` object </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadFitted </td>
   <td style="text-align:left;"> fireSense_SpreadFit </td>
   <td style="text-align:left;"> DEFUNCT -- A fitted model object of class fireSense_SpreadFit. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaWithSpreadParams </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> This is the studyArea, but with 10 duplicated features, each with its own set of parameters from the 10 best DEoptim runs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fsSpreadFit_hists </td>
   <td style="text-align:left;"> ggplot </td>
   <td style="text-align:left;"> histograms of each parameter used in `DEoptim` fitting. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lociList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of fire locs </td>
  </tr>
</tbody>
</table>

### Links to other modules

<!-- TODO: add links to other modules -->
This module can be used in association with `fireSense_SpreadPredict` to derive fire spread probabilities that are sensitive to environmental conditions.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_SpreadFit/issues>

## References

<!-- autogenerated from bibligraphy -->
