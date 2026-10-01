## runSpreadWithoutDEoptim(): everything except the objective function itself, which is replaced by a
## recorder (the real one simulates fires and belongs to fireSenseUtils).
##
## The calibration runs each trial with no early stop and `returnTerms = TRUE`, and reads the
## objective's `firstBlockSNLL` and `bailed` fields; the recorder returns the same fields.

fires <- list(year2001 = data.frame(size = c(5L, 12L), date = "year2001", ids = 11:12, cells = c(3L, 56L)),
              year2002 = data.frame(size = c(3L, 1L), date = "year2002", ids = 21:22, cells = c(89L, 12L)))
lower <- c(a = 0, b = 10)
upper <- c(a = 1, b = 20)

callIt <- function(mode, ..., iterThresh = 4L, seed = 42L) {
  msgs <- character()
  withCallingHandlers(utils::capture.output(
    out <- runSpreadWithoutDEoptim(
      iterThresh = iterThresh, lower = lower, upper = upper, fireSense_spreadFormula = "~ 0 + a + b",
      flammableRTM = "theLandscape", annualDTx1000 = list(), nonAnnualDTx1000 = list(),
      fireBufferedListDT = list(), historicalFires = fires, covMinMax = NULL, objfunFireReps = 7L,
      maxFireSpread = 0.28, tests = "SNLL_FS", formulaToFit = "~ 0 + a + b", mode = mode, seed = seed, ...)),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  if (!is.null(out)) attr(out, "messages") <- msgs
  out
}

## records each call and returns what the real objective returns: a scalar, or with `returnTerms` the
## named terms. A trial's first-block score is a function of its parameters (forks do not share `rec`):
## `annual(par)`, and `saturated(par)` sets `bailed`. A trial run with penaliseRunaways = FALSE scores
## 1000 more, so a threshold built on another objective than the fit's is visible.
annualOf <- function(par) round(300 + 600 * par[["a"]])
saturatedOf <- function(par) par[["b"]] > 18
recorder <- function(rec, annual = annualOf, saturated = function(par) FALSE) {
  function(par, thresh, returnTerms = FALSE, penaliseRunaways = TRUE, ...) {
    rec$calls[[length(rec$calls) + 1L]] <- c(list(par = par, thresh = thresh, returnTerms = returnTerms,
                                                  penaliseRunaways = penaliseRunaways), list(...))
    a <- annual(par) + if (isTRUE(penaliseRunaways)) 0 else 1000
    if (isTRUE(returnTerms))
      return(c(objective = 1000 + a, firstBlockSNLL = a, bailed = as.numeric(saturated(par))))
    1000 + a
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
  expect_true(all(vapply(rec$calls, `[[`, numeric(1), "thresh") == Inf))   # no early stop
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

test_that("the seed fixes the draws: parameter draws, R >= 3.6 sampling", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", seed = 42L)
  first <- rec$calls
  rec$calls <- NULL
  callIt("debug", seed = 42L)
  expect_identical(rec$calls, first)
  expect_equal(first[[1]]$par, c(a = 0.914806043496355, b = 19.3707541329786))
  rec$calls <- NULL
  callIt("debug", seed = 43L)
  expect_false(identical(rec$calls[[1]]$par, first[[1]]$par))
})

test_that("supplied `pars` are evaluated as given, with no early stop", {
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", pars = c(0.5, 15))
  expect_length(rec$calls, 1L)
  ## named with `names(lower)`: unnamed and the same length, so the objective function can tell
  ## apart a trailing yearSpreadSD from a logistic parameter (see the naming test below)
  expect_identical(rec$calls[[1]]$par, c(a = 0.5, b = 15))
  expect_identical(rec$calls[[1]]$thresh, Inf)
})

test_that("drawn parameter sets are named, so a trailing yearSpreadSD bound is recognised", {
  ## R/runSpreadWithoutDEoptim.R used to draw `pars` with `runif()`, unnamed. Since yearSpreadSD
  ## became a default trailing bound, fireSenseUtils:::.objfunSpreadFit tells it apart from a
  ## logistic parameter only via names(par), so an unnamed draw made every trial error with
  ## "logistic with 4 parameters not tested yet" and the threshold came back NA.
  lowerYSD <- c(lower, yearSpreadSD = 0)
  upperYSD <- c(upper, yearSpreadSD = 1)
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  suppressMessages(utils::capture.output(
    runSpreadWithoutDEoptim(
      iterThresh = 4L, lower = lowerYSD, upper = upperYSD, fireSense_spreadFormula = "~ 0 + a + b",
      flammableRTM = "theLandscape", annualDTx1000 = list(), nonAnnualDTx1000 = list(),
      fireBufferedListDT = list(), historicalFires = fires, covMinMax = NULL, objfunFireReps = 7L,
      maxFireSpread = 0.28, tests = "SNLL_FS", formulaToFit = "~ 0 + a + b", mode = "debug", seed = 42L)))
  expect_identical(names(rec$calls[[1]]$par), c("a", "b", "yearSpreadSD"))
})

test_that("fit mode: threshold is 2 x the best usable first block; saturated trials are ignored", {
  withr::local_options(mc.cores = 2L)
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", iterThresh = 8L)                         # the 8 parameter sets seed 42 draws
  expect_true(all(vapply(rec$calls, `[[`, numeric(1), "thresh") == Inf))   # no early stop
  trials <- lapply(rec$calls, `[[`, "par")
  usable <- !vapply(trials, saturatedOf, logical(1))
  annual <- vapply(trials, annualOf, numeric(1))
  expect_true(any(!usable) && any(usable))                 # the fixture has both kinds
  expect_lt(min(annual[!usable]), min(annual[usable]))     # a saturated trial would otherwise win
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env(), saturated = saturatedOf))
  out <- callIt("fit", iterThresh = 8L)
  expect_equal(as.numeric(out), 2 * min(annual[usable]))
  expect_match(attr(out, "messages"), paste0(sum(usable), " of 8 trials usable"), all = FALSE)
})

test_that("fit mode: thresholdMargin scales the threshold; no usable trial gives NA and a warning", {
  withr::local_options(mc.cores = 2L)
  rec <- new.env()
  local_mocked_bindings(.objfunSpreadFit = recorder(rec))
  callIt("debug", iterThresh = 3L)
  best <- min(vapply(lapply(rec$calls, `[[`, "par"), annualOf, numeric(1)))
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env()))
  expect_equal(as.numeric(callIt("fit", iterThresh = 3L, thresholdMargin = 3)), 3 * best)
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env(), saturated = function(par) TRUE))
  out <- callIt("fit", iterThresh = 3L)                    # NA: noEarlyStopThreshold() makes it Inf
  expect_identical(as.numeric(out), NA_real_)
  expect_match(attr(out, "messages"), "0 of 3 trials usable", all = FALSE)
})

test_that("a crashed trial is not usable", {
  withr::local_options(mc.cores = 2L)
  local_mocked_bindings(.objfunSpreadFit = function(par, thresh, ...) stop("boom"))
  expect_identical(as.numeric(callIt("fit", iterThresh = 2L)), NA_real_)
})

test_that("fit mode: the trials run the fit's own objective (penaliseRunaways reaches them)", {
  withr::local_options(mc.cores = 2L)
  local_mocked_bindings(.objfunSpreadFit = recorder(new.env()))
  on <- callIt("fit", iterThresh = 3L, penaliseRunaways = TRUE)
  off <- callIt("fit", iterThresh = 3L, penaliseRunaways = FALSE)
  expect_equal(as.numeric(off) - as.numeric(on), 2000)
})
