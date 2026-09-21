## deriveCovMinMax(): the ranges that rescale covariates, in the fit and again in prediction.

dt <- data.table::data.table

test_that("biomass-like columns (max > 1) share one range; cover-like columns keep their own", {
  nonAnnual <- list(a = dt(pixelID = 1:3, class1 = c(0.5, 2, 5.2), class2 = c(1, 1.5, 3), nf = c(0, 0.4, 0.8)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(10, 40), youngAge = c(0, 1)))
  out <- deriveCovMinMax(annual, nonAnnual)
  ## class1 0.5-5.2 and class2 1-3 are both "biomass": shared range is 0.5-5.2
  expect_identical(as.list(out),
                   list(class1 = c(0.5, 5.2), class2 = c(0.5, 5.2), nf = c(0, 0.8),
                        CMDsm = c(10, 40), youngAge = c(0, 1)))
})

test_that("the shared biomass range is the envelope of all biomass columns, not the first one's", {
  ## class1 2-5, class2 1-3, class3 4-9 -> every biomass column gets 1-9
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(2, 5), class2 = c(1, 3), class3 = c(4, 9)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(1, 2)))
  out <- deriveCovMinMax(annual, nonAnnual)
  expect_identical(out$class1, c(1, 9))
  expect_identical(out$class2, c(1, 9))
  expect_identical(out$class3, c(1, 9))
})

test_that("ranges span every list element, and pixelID, buffer and ids are never covariates", {
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(2, 3)), b = dt(pixelID = 3:4, class1 = c(1, 7)))
  annual <- list(year2001 = dt(pixelID = 1:2, buffer = 1L, ids = 5L, CMDsm = c(10, 40)),
                 year2002 = dt(pixelID = 3:4, buffer = 0L, ids = 6L, CMDsm = c(5, 12)))
  out <- deriveCovMinMax(annual, nonAnnual)
  expect_identical(names(out), c("class1", "CMDsm"))
  expect_identical(out$class1, c(1, 7))
  expect_identical(out$CMDsm, c(5, 40))
})

test_that("a column exactly at 1 is cover, not biomass", {
  nonAnnual <- list(a = dt(pixelID = 1:2, cover = c(0.2, 1), biomass = c(3, 4)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(1, 2)))
  out <- deriveCovMinMax(annual, nonAnnual)
  expect_identical(out$cover, c(0.2, 1))   # own range: max is not > 1
  expect_identical(out$biomass, c(3, 4))
})

test_that("column order is the non-annual table's, then the annual one's", {
  nonAnnual <- list(a = dt(pixelID = 1:2, nf = c(0, 0.5), class2 = c(2, 3), class1 = c(1, 9)))
  annual <- list(year2001 = dt(pixelID = 1:2, youngAge = c(0, 1), CMDsm = c(1, 2)))
  expect_identical(names(deriveCovMinMax(annual, nonAnnual)),
                   c("nf", "class2", "class1", "youngAge", "CMDsm"))
})

test_that("an annual column missing from some years is filled, and an NA reaches the result", {
  ## spreadFitPrep() stops on an NA here; this is where it comes from
  nonAnnual <- list(a = dt(pixelID = 1:2, class1 = c(2, 3)))
  annual <- list(year2001 = dt(pixelID = 1:2, CMDsm = c(10, 40)),
                 year2002 = dt(pixelID = 1:2, CMDsm = c(5, 12), extra = c(1, 2)))
  out <- deriveCovMinMax(annual, nonAnnual)
  expect_identical(out$CMDsm, c(5, 40))
  expect_true(all(is.na(out$extra)))
})
