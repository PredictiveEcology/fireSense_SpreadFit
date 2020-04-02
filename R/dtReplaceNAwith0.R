dtReplaceNAwith0 <- function(DT, colsToUse = NULL){
  if (is.null(colsToUse))
    colsToUse <- names(DT)
  for (i in colsToUse) {
    nas <- which(is.na(DT[[i]]))
    if (length(nas))
      set(DT, nas, i, 0)
  }
  DT
}
