## Choosing the calibrated SNLL threshold.
##
## FireSense, 2026-09-17: ELF 6.1.1 fitted with thresh = Inf, so no trial ever bailed early -- 4% failed
## evaluations and a 48 s mean, against 15-61% and 14-26 s for the other fits in that wave. The cause was a
## hard-coded cutoff: the calibration keeps the thresholds whose trial did not fail, as `objFun < 1e5`, and
## 6.1.1's *normal* objective values are 131k-207k. Every row was dropped and min() of nothing is Inf.
## The failure sentinel is fireSenseUtils' failVal = 1e6, so that is the cutoff.

test_that("the smallest threshold whose trial did not fail is chosen", {
  expect_identical(pickThreshold(thresholds = c(300, 600, 900), objFun = c(1e6, 42000, 41000)), 600)
})

test_that("an ELF whose objective exceeds 1e5 still gets a threshold (the 6.1.1 bug)", {
  ## values like 6.1.1's: six figures, and none of them a failure
  expect_identical(pickThreshold(thresholds = c(1000, 2000, 3000), objFun = c(207282, 150218, 131067)), 1000)
})

test_that("failures are the sentinel, whatever else the objective returns", {
  ## a bailed trial returns failVal (+ the adTest term when that test is on), so use >= failVal
  expect_identical(pickThreshold(c(100, 200, 300), c(1e6, 1.5e6, 9000)), 300)
  expect_identical(pickThreshold(c(100, 200, 300), c(1e6, 1e6, 1e6)), NA_real_)
  expect_warning(pickThreshold(c(100, 200), c(1e6, 1e6)), "no threshold")
})

test_that("non-finite and missing values never become the threshold", {
  expect_identical(pickThreshold(c(100, 200, 300), c(NA, Inf, 9000)), 300)
})

test_that("the calibration uses pickThreshold()", {
  src <- readLines("../../R/runSpreadWithoutDEoptim.R", warn = FALSE)
  expect_true(any(grepl("pickThreshold(", src, fixed = TRUE)))
  expect_false(any(grepl("objFun < 1e5", src, fixed = TRUE)))
})
