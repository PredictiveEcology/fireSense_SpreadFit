## Edge cases of pickThreshold() and bestParamSets() not covered by the files that introduced them.

test_that("pickThreshold(): failVal is the cutoff, and is exclusive", {
  ## 99 < 100 passes, 100 does not
  expect_identical(pickThreshold(c(10, 20, 30), c(100, 99, 5), failVal = 100), 20)
  expect_identical(pickThreshold(c(30, 20, 10), c(100, 99, 5), failVal = 100), 10)
})

test_that("pickThreshold(): a try-error string from a crashed fork counts as a failed trial", {
  ## mcmapply() returns a list when one fork errors; as.numeric() of the message is NA
  expect_identical(pickThreshold(c(10, 20), list("Error in f() : boom", 5000)), 20)
  expect_warning(out <- pickThreshold(10, "Error"), "every trial failed \\(objective >= 1e\\+06\\)")
  expect_identical(out, NA_real_)
})

test_that("bestParamSets(): n sets how many members are recorded, best first", {
  skip_if_not("bestByReplicatedMean" %in% getNamespaceExports("fireSenseUtils"))
  DE <- toyDE(nPar = 2L)                       # member k has p1 = k and value 8 - k
  out <- bestParamSets(DE, c("p1", "p2"), n = 2L)
  expect_equal(out$params$p1, c(7, 6))
  expect_equal(out$objFunVal, c(1, 2))
  ## only the LAST block's population is read
  DE[[1]]$member$pop[, 1] <- 99
  expect_equal(bestParamSets(DE, c("p1", "p2"), n = 1L)$params$p1, 7)
})
