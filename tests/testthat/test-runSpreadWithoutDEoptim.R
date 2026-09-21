## runSpreadWithoutDEoptim(): everything except the objective function itself, which is replaced by a
## recorder (the real one simulates fires and belongs to fireSenseUtils).
##
## Fires: 5 and 12 pixels in 2001, 3 and 1 pixels in 2002. Fires of 1 pixel are ignored, so 3 fires
## remain in the two largest years and the largest fire of the largest year is 12 pixels:
## rough threshold = 3 * log(12) = 7.45, and candidates are drawn from 1:floor(4 * max(n, 7.45)).

fires <- list(year2001 = data.frame(size = c(5L, 12L), date = "year2001", ids = 11:12, cells = c(3L, 56L)),
              year2002 = data.frame(size = c(3L, 1L), date = "year2002", ids = 21:22, cells = c(89L, 12L)))
lower <- c(a = 0, b = 10)
upper <- c(a = 1, b = 20)

callIt <- function(mode, ..., iterThresh = 4L, seed = 42L) {
  suppressMessages(utils::capture.output(
    out <- runSpreadWithoutDEoptim(
      iterThresh = iterThresh, lower = lower, upper = upper, fireSense_spreadFormula = "~ 0 + a + b",
      flammableRTM = "theLandscape", annualDTx1000 = list(), nonAnnualDTx1000 = list(),
      fireBufferedListDT = list(), historicalFires = fires, covMinMax = NULL, objfunFireReps = 7L,
      maxFireSpread = 0.28, tests = "SNLL_FS", formulaToFit = "~ 0 + a + b", mode = mode, seed = seed, ...)))
  out
}

## records each call; a threshold below `failBelow` "bails" (returns fireSenseUtils' failVal)
recorder <- function(rec, failBelow = 0) {
  function(par, thresh, ...) {
    rec$calls[[length(rec$calls) + 1L]] <- c(list(par = par, thresh = thresh), list(...))
    if (thresh < failBelow) 1e6 else 1000 + thresh
  }
}

test_that("debug mode evaluates every random parameter set in turn and returns NULL", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  expect_null(callIt("debug"))
  expect_length(rec$calls, 4L)
  pars <- do.call(rbind, lapply(rec$calls, `[[`, "par"))
  expect_true(all(pars[, 1] >= 0 & pars[, 1] <= 1))      # each parameter inside its own bounds
  expect_true(all(pars[, 2] >= 10 & pars[, 2] <= 20))
  thr <- vapply(rec$calls, `[[`, numeric(1), "thresh")
  expect_true(all(thr >= 1 & thr <= 29))                 # floor(4 * 3 * log(12)) = 29
  expect_identical(anyDuplicated(thr), 0L)               # sampled without replacement
  ## what the objective function is told
  one <- rec$calls[[1]]
  expect_identical(one$landscape, "theLandscape")
  expect_identical(one$Nreps, 7L)
  expect_identical(one$tests, "SNLL_FS")
  expect_identical(one$maxFireSpread, 0.28)
  expect_identical(one$historicalFires, fires)
  expect_true(one$weighted)
  expect_true(one$plot.it)
})

test_that("the seed fixes the draws: values from origin/development at 6461e8d, R >= 3.6 sampling", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", seed = 42L)
  first <- rec$calls
  rec$calls <- NULL
  callIt("debug", seed = 42L)
  expect_identical(rec$calls, first)
  expect_equal(first[[1]]$par, c(0.914806043496355, 19.3707541329786))
  expect_identical(vapply(first, `[[`, integer(1), "thresh"), c(17L, 15L, 24L, 7L))
  rec$calls <- NULL
  callIt("debug", seed = 43L)
  expect_false(identical(rec$calls[[1]]$par, first[[1]]$par))
})

test_that("supplied `pars` are evaluated as given, with a threshold that never bails", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", pars = c(0.5, 15))
  expect_length(rec$calls, 1L)
  expect_identical(rec$calls[[1]]$par, c(0.5, 15))
  expect_identical(rec$calls[[1]]$thresh, 1e8)
})

test_that("fit mode returns the smallest candidate threshold whose trial did not bail", {
  ## the candidates for seed 42, recorded in debug mode with the same seed
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", seed = 42L)
  candidates <- vapply(rec$calls, `[[`, numeric(1), "thresh")

  withr::local_options(mc.cores = 2L)
  cutoff <- sort(candidates)[2]                           # the smallest candidate bails, the rest do not
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env(), failBelow = cutoff))
  expect_equal(callIt("fit", seed = 42L), cutoff)     # 15: candidates are 17, 15, 24, 7 and 7 bails
  expect_equal(cutoff, 15)

  ## every trial bails: NA and a warning, not Inf
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env(), failBelow = Inf))
  expect_warning(out <- callIt("fit", seed = 42L), "no threshold calibrated")
  expect_identical(out, NA_real_)
})

test_that("with more parameter sets than the rough threshold, candidates come from 1:(4 * n)", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", iterThresh = 20L)                       # 20 > 7.45, so 1:80
  thr <- vapply(rec$calls, `[[`, numeric(1), "thresh")
  expect_length(thr, 20L)
  expect_true(all(thr >= 1 & thr <= 80))
  expect_gt(max(thr), 29)                                 # seed 42: not confined to the small range
})
