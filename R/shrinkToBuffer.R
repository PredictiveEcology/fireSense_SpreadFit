shrinkToBuffer <- function(annualDTx1000, whNotNA, fireBufferedListDT, ...) {
  annualDTx1000 <- lapply(annualDTx1000, function(x) {
    setDT(x)
    set(x, NULL, "pixelID", whNotNA)
    x
  })
  annualDTx1000 <- Map(merge, fireBufferedListDT, annualDTx1000, MoreArgs = list(by = "pixelID"))
}
