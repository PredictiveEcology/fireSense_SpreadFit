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
