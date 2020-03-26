dtReplaceNAwith0 <- function(DT, colsToUse){
  if (is.null(colsToUse))
    colsToUse <- names(DT)
  for (i in colsToUse)
    DT[is.na(get(i)), (i):=0]
}
