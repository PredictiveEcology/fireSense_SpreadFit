## estimateSNLLThresholdPostLargeFires() cached runSpreadWithoutDEoptim() with
## omitArgs = c("objfunFireReps", "mode"), but both change the result: `mode` picks a
## branch that returns a different type (a numeric threshold vs NULL), and
## `objfunFireReps` changes the threshold's value. Leaving them out of the key served a
## debug-mode result to a fit-mode run, and a 5-replicate threshold to a 25-replicate run.
##
## The call needs a full simList, so this checks the function's source instead of running
## it: parsed, not sourced, so no module machinery is needed.
test_that("the SNLL threshold cache key does not omit mode or objfunFireReps", {
  exprs <- parse(testthat::test_path("..", "..", "fireSense_SpreadFit.R"), keep.source = FALSE)
  def <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                  identical(x[[2]], as.name("estimateSNLLThresholdPostLargeFires")), exprs)
  expect_length(def, 1L)
  code <- paste(deparse(def[[1]][[3]]), collapse = "\n")
  expect_match(code, "runSpreadWithoutDEoptim", fixed = TRUE)
  expect_false(grepl("omitArgs\\s*=\\s*c\\([^)]*\"(mode|objfunFireReps)\"", code))
})
