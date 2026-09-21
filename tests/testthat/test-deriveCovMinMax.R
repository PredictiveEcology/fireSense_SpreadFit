## deriveCovMinMax(): the ranges that rescale covariates, in the fit and again in prediction.
## Fuel biomass columns get the FIXED range fireSenseUtils::fuelLinearRange, c(0, 1e4), so rescaling is
## biomass / 1e4 in every polygon and every predicted year; a data-derived maximum would differ between
## polygons and would have to be the fitted polygon's, forever. Every other covariate keeps its own range.

dt <- data.table::data.table

test_that("fuel columns get the fixed range; every other covariate keeps its own", {
  nonAnnual <- list(a = dt(pixelID = 1:3, class1 = c(0, 300, 22601), class2 = c(40, 0, 9000), nf = c(0, 0.4, 0.8)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(10, 40), youngAge = c(0, 1)))
  out <- deriveCovMinMax(annual, nonAnnual, fuelCols = c("class1", "class2"))
  expect_identical(as.list(out),
                   list(class1 = c(0, 1e4), class2 = c(0, 1e4), nf = c(0, 0.8),
                        CMDsm = c(10, 40), youngAge = c(0, 1)))
})

test_that("the fuel range is fixed, whatever the biomass in the data", {
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(1, 2)))
  low  <- deriveCovMinMax(annual, list(a = dt(pixelID = 1:2, class1 = c(5, 50))), fuelCols = "class1")
  high <- deriveCovMinMax(annual, list(a = dt(pixelID = 1:2, class1 = c(3000, 76308))), fuelCols = "class1")
  expect_identical(low$class1, c(0, 1e4))
  expect_identical(high$class1, c(0, 1e4))
  expect_identical(low$class1, fireSenseUtils::fuelLinearRange)
})

test_that("only the columns named as fuel get the fixed range", {
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(2, 500), other = c(3, 4)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(1, 2)))
  out <- deriveCovMinMax(annual, nonAnnual, fuelCols = "class1")
  expect_identical(out$class1, c(0, 1e4))
  expect_identical(out$other, c(3, 4))
})

test_that("ranges span every list element, and pixelID, buffer and ids are never covariates", {
  nonAnnual <- list(a = dt(pixelID = 1:2, nf = c(0.2, 0.3)), b = dt(pixelID = 3:4, nf = c(0.1, 0.7)))
  annual <- list(year2001 = dt(pixelID = 1:2, buffer = 1L, ids = 5L, CMDsm = c(10, 40)),
                 year2002 = dt(pixelID = 3:4, buffer = 0L, ids = 6L, CMDsm = c(5, 12)))
  out <- deriveCovMinMax(annual, nonAnnual, fuelCols = character())
  expect_identical(names(out), c("nf", "CMDsm"))
  expect_identical(out$nf, c(0.1, 0.7))
  expect_identical(out$CMDsm, c(5, 40))
})

test_that("column order is the non-annual table's, then the annual one's", {
  nonAnnual <- list(a = dt(pixelID = 1:2, nf = c(0, 0.5), class2 = c(2, 3), class1 = c(1, 9)))
  annual <- list(year2001 = dt(pixelID = 1:2, youngAge = c(0, 1), CMDsm = c(1, 2)))
  expect_identical(names(deriveCovMinMax(annual, nonAnnual, fuelCols = c("class1", "class2"))),
                   c("nf", "class2", "class1", "youngAge", "CMDsm"))
})

test_that("an annual column missing from some years is filled, and an NA reaches the result", {
  ## spreadFitPrep() stops on an NA here; this is where it comes from
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(2, 3)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(10, 40)),
                 year2002 = dt(pixelID = 1:2, CMDsm = c(5, 12), extra = c(1, 2)))
  out <- deriveCovMinMax(annual, nonAnnual, fuelCols = "class1")
  expect_identical(out$CMDsm, c(5, 40))
  expect_true(all(is.na(out$extra)))
})

## fuelColumns(): which non-annual columns are fuel biomass, judged on the log scale they arrive on.
test_that("fuelColumns(): a maximum above 1 marks fuel; indicators and proportions are not fuel", {
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(3.605, 9.2), nf = c(0, 0.8), youngAge = c(0, 1)),
                    b = dt(pixelID = 3:4, class1 = c(3.605, 3.605), nf = c(0.1, 0.2), youngAge = c(1, 1)))
  expect_identical(fuelColumns(nonAnnual), "class1")
})

test_that("fuelColumns(): a column exactly at 1 is not fuel, and pixelID never is", {
  expect_identical(fuelColumns(list(a = dt(pixelID = 100:101, cover = c(0.2, 1), fuel = c(3.605, 4)))), "fuel")
})
