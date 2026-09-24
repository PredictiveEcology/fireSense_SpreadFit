# fireSense_SpreadFit (development version)

## DEoptim crossover adaptation

- Requires clusters >= 0.0.42 (was 0.0.41). With `iterStep` > 1, that version runs DEoptim with `c = 0`, because
  DEoptim's F adaptation turns every trial vector into NaN once a call's first generation has no successful trial.
  Without it, this module's defaults (`iterStep = 25`, `.c = 0.5`) could crash a fit on every worker, so projects
  had to set `iterStep = 1`.

## Per-year random effect

- New parameter `yearSpreadSDBounds` (default `c(0, 1)`): the default bounds get `yearSpreadSD` last, and
  `fireSenseUtils::runDEoptim()` (>= 0.2.3.9041) fits it as the sd of a per-year random effect on logit spread
  probability, a seasonal departure: each year draws one eps, so all of a year's fires burn hotter or cooler
  together, which widens the simulated fire-size distribution. `NA` turns it off. If only one bound is supplied,
  the other includes `yearSpreadSD` only if the supplied one does. (Briefly `fireSpreadSDBounds`, per fire.) This
  changes every fit's cache key.
- `covFixedRange` also fixes the scale of CMD, CMDsp and cumMDC (all / 100), the other climate candidates of
  fireSense_dataPrepFit's `spread = "auto"`, so their coefficients compare across ELFs as CMDsm's do.

## Objective and link

- New parameters `sizeLik` (default "t"), `sizeLikDf`, `weighted` (default FALSE) and `adWeight` reach the objective in the fit and in the re-score. Fits used "kde" with a log(size) weight before, only because the module could not ask for anything else; "t" without a weight predicted held-out years best in the 2026-09-21 cross-validation. This changes every fit's cache key.
- New parameter `link`: "logistic3pUpper" adds `upperTail1` (bounds `upperTailBounds`, default c(-1, 1)), which changes only how the spread probability approaches its ceiling (`fireSenseUtils::logistic3pUpper()`). The default stays "logistic3p".

## Diagnostics after every fit

- New event `postFitDiagnostics`, scheduled after `run`. It makes `spreadFitRescore`, `spreadFitIdentifiability` (which covariates are identified in isolation), `spreadFitProfile`, `spreadFitSizes` (observed against uncapped simulated fire sizes), `spreadFitLinkSaturation` and `spreadFitConvergence`. The costly parts run on the fit's workers inside `runDEoptim()`: `profileReps` (default 10) and `simulateMembers` (default 10).
- `mode = "validate"` adds `crossValidate`: two fits, each on every other year, predicting the years it did not see (`spreadFitHeldOut`). It never writes the ledger.

## Climate covariate

- New parameter `covFixedRange` (default `list(CMDsm = c(0, 100))`): covariates rescaled with a fixed range, not the range of the polygon's data. CMDsm is now CMDsm / 100 everywhere. With the data's range, 1 meant a CMDsm of 104 in ELF 5.3.2 and 297 in ELF 13.1, so the coefficient meant something different in each polygon, and one that never gets dry stretched its small range over [0, 1]. Pooled over six ELFs on the absolute scale, fire size is flat below a CMDsm of about 125 and about twice as large above 150; no single polygon's own scale shows that. `covFixedRange = list()` restores the old behaviour. An NA in the data still reaches the NA check.

## Fuel covariates

- Fuel biomass is fitted on the linear scale, divided by a fixed 1e4. It arrives from `fireSense_dataPrepFit` logged (`fireSenseUtils::logMinB()`); `spreadFitPrepare` undoes that with `fireSenseUtils::fuelLogToLinear()` on a copy, and the supplied covariates are not changed. On the log scale the treed pixels of ELF 5.3.2 fell in 16% of the covariate range and 45% of pixels sat on the floor, so the fuel coefficients estimated little more than treed against treeless. Needs fireSenseUtils >= 0.2.3.9029.
- `covMinMax_spread` gives every fuel column `c(0, 1e4)` (`fireSenseUtils::fuelLinearRange`) and no longer the data's shared range. `fireSense_SpreadPredict` recognises a linear fit by that range, so parameters fitted earlier, on the log scale, still predict as they did.
- New parameter `upperAndLowerValFuel` (default 60): the default bound of the fuel coefficients. With 9, as for every other covariate, the fuel coefficient sat on its bound.
- In 36 model-selection fits over six ELFs, one column per fuel class, as here, was best or within 0.5% of the best in every ELF; collapsing fuel to a total cost 9.6% in the most mixed ELF (three replicate runs each, p = 0.007).

# fireSense_SpreadFit 1.0.6

First release from `development` since `master` was last updated (2023-09-06). Full history: https://github.com/PredictiveEcology/fireSense_SpreadFit/compare/cfc4cd7...v1.0.6

## Breaking changes

- Removed input `dataFireSense_SpreadFit` (RasterLayer, RasterStack).
- Removed input `firePoints` (SpatialPointsDataFrame).
- Removed input `firePolys` (list).
- Removed input `flammableRTM` (RasterLayer).
- Removed input `polyCentroids` (list).
- Input `rasterToMatch` is now `SpatRaster` (was `RasterLayer`).
- Input `studyArea` is now `sf` (was `SpatialPolygonDataFrame`).
- Removed output `covMinMax` (data.table).
- Removed parameters: `.plot`, `debugMode`, `fireYears`, `formula`, `minBufferSize`, `parallelMachinesIP`, `useCentroids`.

## New features

- New inputs: `.ELFind`, `.runName`, `fireBufferedListDT`, `fireSense_annualSpreadFitCovariates`, `fireSense_nonAnnualSpreadFitCovariates`, `fireSense_spreadFormula`, `parsKnown`, `spreadFirePoints`, `spreadFitAdditionalColNames`.
- New outputs: `covMinMax_spread`, `fsSpreadFit_hists`, `lociList`, `studyAreaWithSpreadParams`.
- New parameters: `.c`, `.plotSize`, `.plots`, `DEoptimTests`, `SNLL_FS_thresh`, `doObjFunAssertions`, `iterThresh`, `libPathDEoptim`, `mode`, `mutuallyExclusiveCols`, `rep`, `spreadFitFilename`, `spreadFitGoogleDriveFolder`, `stopIfNoPreRunFit`, `upperAndLowerVal`, `useCache_DE`.

## Dependencies

- No longer depends on `fastdigest`, `rgeos`.
- Now depends on `clusters`, `fpCompare`, `ggplot2`, `munsell`, `scales`, `terra`, `tidyr`.

## Testing

- testthat suite and CI (`testthat-module`), including a snapshot of the module's inputs, outputs and parameters in `tests/testthat/test-metadata.R`.
