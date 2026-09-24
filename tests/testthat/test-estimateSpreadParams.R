## estimateSpreadParams(): the default DEoptim bounds.

form <- "~ 0 + CMDsm + youngAge + class1 + nf"
annual <- list(year2001 = data.table::data.table(pixelID = 1L, CMDsm = 1, youngAge = 0))

test_that("upper bounds: +B for covariates, 0 for youngAge, fixed logistic bounds first", {
  expect_identical(
    estimateSpreadParams(form, annual, whichBound = "upper", upperAndLower = 9),
    c(maxAsymptote = 0.276, hillSlope1 = 2, inflectionPoint1 = 4,
      CMDsm = 9, youngAge = 0, class1 = 9, nf = 9))
})

test_that("lower bounds: -B for non-annual covariates and youngAge, 0 for other annual covariates", {
  expect_identical(
    estimateSpreadParams(form, annual, whichBound = "lower", upperAndLower = 9),
    c(maxAsymptote = 0.25, hillSlope1 = 0.2, inflectionPoint1 = 0.1,
      CMDsm = 0, youngAge = -9, class1 = -9, nf = -9))
})

test_that("the bound scales with upperAndLower and every lower bound is <= its upper bound", {
  up <- estimateSpreadParams(form, annual, "upper", upperAndLower = 2.5)
  lo <- estimateSpreadParams(form, annual, "lower", upperAndLower = 2.5)
  expect_identical(unname(up[c("CMDsm", "class1", "nf")]), c(2.5, 2.5, 2.5))
  expect_identical(unname(lo[c("youngAge", "class1", "nf")]), c(-2.5, -2.5, -2.5))
  expect_identical(names(up), names(lo))
  expect_true(all(lo <= up))
})

test_that("a term is 'annual' only if it is a column of the annual covariates", {
  ## youngAge absent from the annual table: it is bounded like any other covariate
  noYA <- list(year2001 = data.table::data.table(pixelID = 1L, CMDsm = 1))
  expect_identical(unname(estimateSpreadParams(form, noYA, "upper", 9)["youngAge"]), 9)
  expect_identical(unname(estimateSpreadParams(form, noYA, "lower", 9)["youngAge"]), -9)
  ## no annual term in the formula at all
  expect_identical(estimateSpreadParams("~ 0 + class1", annual, "lower", 3),
                   c(maxAsymptote = 0.25, hillSlope1 = 0.2, inflectionPoint1 = 0.1, class1 = -3))
})

test_that("term order follows the formula", {
  expect_identical(names(estimateSpreadParams("~ 0 + nf + CMDsm", annual, "upper", 9)),
                   c("maxAsymptote", "hillSlope1", "inflectionPoint1", "nf", "CMDsm"))
})

test_that("whichBound must be 'upper' or 'lower'", {
  expect_error(estimateSpreadParams(form, annual, whichBound = "both", upperAndLower = 9),
               "whichBound %in% c(\"upper\", \"lower\") is not TRUE", fixed = TRUE)
})

test_that("the per-fire random effect's sd is bounded last, after an upper-tail term", {
  ## fireSenseUtils::runDEoptim() finds fireSpreadSD by name and requires it last; the objective takes
  ## the logistic parameters by position, so upperTail1 stays 4th
  up <- estimateSpreadParams(form, annual, "upper", 9, upperTailBounds = c(-1, 1), fireSpreadSDBounds = c(0, 1))
  lo <- estimateSpreadParams(form, annual, "lower", 9, upperTailBounds = c(-1, 1), fireSpreadSDBounds = c(0, 1))
  expect_identical(names(up)[4], "upperTail1")
  expect_identical(names(up)[length(up)], "fireSpreadSD")
  expect_identical(unname(c(lo["fireSpreadSD"], up["fireSpreadSD"])), c(0, 1))
  expect_identical(names(up), names(lo))
  ## off: no fireSpreadSD at all
  expect_false("fireSpreadSD" %in% names(estimateSpreadParams(form, annual, "upper", 9)))
})

