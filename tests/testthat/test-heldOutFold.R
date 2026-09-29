## `heldOutFold` runs ONE cross-validation fold as its own job, instead of both folds in the same
## job (mode "validate"'s `crossValidate`). See fireSense_spreadFit.R init and R/fitSpread.R
## crossValidateSpreadOneFold().

heldOutFoldRun <- function(k, params = list()) {
  rec <- new.env()
  rec$fits <- list(); rec$sims <- list()
  sim <- toySim(c(list(heldOutFold = k, simulateMembers = 3L, stopIfNoPreRunFit = TRUE), params))
  mockFitAndLedger(sim, rec)
  mockInModule(sim,
    runDEoptim = function(...) {
      a <- list(...)
      rec$fits[[a$runName]] <- list(years = names(a$historicalFires), profileReps = a$profileReps)
      toyDE(length(a$lower))
    },
    ## Cache() wraps both the fit, already evaluated here, and the held-out simulation
    Cache = function(FUN, pop, fnArgs, ...) {
      if (!is.function(FUN)) return(FUN)
      rec$sims[[length(rec$sims) + 1L]] <- list(pop = pop, years = names(fnArgs$historicalFires))
      yr <- names(fnArgs$historicalFires)
      data.table::data.table(member = 1L, yr = yr, rep = 1L, ids = 1L, size = 4, sim = 8L)
    })
  list(sim = suppressMessages(SpaDES.core::spades(sim)), rec = rec)
}

test_that("heldOutFold = 1 fits fold 2's years, scores only fold 1, writes a fold-specific file", {
  out <- heldOutFoldRun(1L)
  sim <- out$sim; rec <- out$rec
  ## exactly one fit is run, on the OTHER fold's years (fold 1 is year2001; see cvFolds() test)
  expect_identical(names(rec$fits), "toyRun_cvFold1")
  expect_identical(rec$fits$toyRun_cvFold1$years, "year2002")
  expect_identical(rec$fits$toyRun_cvFold1$profileReps, 0L)
  ho <- sim$spreadFitHeldOut
  expect_identical(ho$sims$fold, 1L)
  expect_identical(vapply(rec$sims, `[[`, "", "years"), "year2001")   # predicts fold 1's held-out year
  expect_null(rec$geoArgs)                                           # the ledger is never touched
  heldOutPath <- file.path(SpaDES.core::outputPath(sim), moduleName, "spreadFitHeldOut_toyRun_fold1.rds")
  expect_true(file.exists(heldOutPath))
  expect_equal(readRDS(heldOutPath), ho)
  ## no full-fit file, and no mode-"validate" (both-fold) file
  expect_false(file.exists(file.path(SpaDES.core::outputPath(sim), moduleName, "spreadFitHeldOut_toyRun.rds")))
})

test_that("heldOutFold = 2 fits fold 1's years, scores only fold 2, writes a fold-specific file", {
  out <- heldOutFoldRun(2L)
  sim <- out$sim; rec <- out$rec
  expect_identical(names(rec$fits), "toyRun_cvFold2")
  expect_identical(rec$fits$toyRun_cvFold2$years, "year2001")
  ho <- sim$spreadFitHeldOut
  expect_identical(ho$sims$fold, 2L)
  expect_identical(vapply(rec$sims, `[[`, "", "years"), "year2002")
  heldOutPath <- file.path(SpaDES.core::outputPath(sim), moduleName, "spreadFitHeldOut_toyRun_fold2.rds")
  expect_true(file.exists(heldOutPath))
  expect_equal(readRDS(heldOutPath), ho)
})

test_that("heldOutFold never schedules or runs the full fit ('run') or writes the ledger", {
  out <- heldOutFoldRun(1L)
  expect_false("run" %in% SpaDES.core::events(out$sim)$eventType)
  expect_null(out$rec$geoArgs)
})
