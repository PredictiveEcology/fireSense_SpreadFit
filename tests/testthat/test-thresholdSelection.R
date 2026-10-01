## Choosing the calibrated SNLL threshold from the trials' own first-block SNLLs.
##
## FireSense, 2026-09-30: each trial used to be paired with an independent random threshold and the
## smallest passing one was kept, so the result was luck (ELF 6.2.1 fold 1: non-saturated trials scored
## 201, 212, 226 but the paired thresholds were 17, 209, 144 -> NA; 14.3 fold 1: 533/541 against a
## largest possible threshold of 381 -> NA). Now: threshold = margin x the best usable trial's
## first-block average annual SNLL, the unit fireSenseUtils' objective compares `thresh` with:
## it bails when  SNLL_FSTest > round(thresh) * numYrsDone  (R/objFunSpread.R), SNLL_FSTest being the
## first block's summed SNLL, so "Avg annual SNLL" = SNLL_FSTest / numYrsDone.

test_that("the threshold is 2 x the best usable value; saturated and failed trials are ignored", {
  annual <- c(533, 541, 1e6, 80, NA, 900)
  saturated <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)    # 80 saturated (a floor score); 1e6 failed
  expect_identical(pickThreshold(annual, saturated), 1066)
  expect_identical(pickThreshold(annual, saturated, margin = 3), 1599)
})

test_that("a non-integer product is rounded up, because the objective rounds thresh", {
  expect_identical(pickThreshold(c(10.2, 50), margin = 2), 21)
})

test_that("no usable trial -> NA with a warning, which noEarlyStopThreshold() turns into Inf", {
  expect_warning(out <- pickThreshold(c(1e6, 2e6, NA), c(FALSE, FALSE, FALSE)), "no threshold calibrated")
  expect_identical(out, NA_real_)
  expect_warning(pickThreshold(c(300, 400), c(TRUE, TRUE)), "no threshold calibrated")
  expect_message(res <- noEarlyStopThreshold(out, runName = "x", nTrials = 3L), "every one of 3 trials failed")
  expect_identical(res, Inf)
})

test_that("failVal is the cutoff for a usable value, and is exclusive", {
  expect_identical(pickThreshold(c(99, 100), failVal = 100), 198)
})

test_that("the best usable trial does not bail under the derived threshold (the objective's own comparison)", {
  ## the comparison in fireSenseUtils::.objfunSpreadFit(), first block: numYrsDone years, summed SNLL
  bails <- function(SNLL_FSTest, thresh, numYrsDone) SNLL_FSTest > round(thresh, 0) * numYrsDone
  for (numYrsDone in c(1L, 2L, 3L)) for (a in c(1, 201, 533.4, 1234)) {
    thresh <- pickThreshold(c(a, 3 * a, 1e6), margin = 2)
    SNLL_FSTest <- round(a * numYrsDone, 0)      # the best trial's first block, as the objective rounds it
    expect_false(bails(SNLL_FSTest, thresh, numYrsDone))
    expect_true(bails(round(2 * thresh * numYrsDone) + 1, thresh, numYrsDone))   # a worse set still bails
  }
})

test_that("the cache key of the calibration includes the rule's code", {
  src <- paste(readLines("../../fireSense_spreadFit.R", warn = FALSE), collapse = "\n")
  expect_match(src, "\\.cacheExtra = list\\(pickThreshold = deparse\\(pickThreshold\\)")
  expect_match(src, "trialFirstBlock = deparse\\(trialFirstBlock\\)")
})

test_that("a trial's first block is read from the objective's print-out", {
  fake <- function(par, thresh, verbose, ...) {
    print("  1 FAIL! 2001; Too burny a landscape; bailing: Min. 0.1")
    print("  1 Decent in 1st 2 years -- continuing.  SNLL threshold: Inf, Avg annual: 533; capHit share: 0; ")
    print("  1 FAIL! 1999; Not spread out enough; bailing: Min. 0.1")   # block 2: not this trial's saturation
    1
  }
  testthat::local_mocked_bindings(.objfunSpreadFit = fake)
  expect_identical(trialFirstBlock(c(a = 1)), list(annual = 533, saturated = TRUE))
})
