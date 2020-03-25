## Toolbox: set of functions used internally by spreadFitRun
chk_duplicatedStartPixels <- function(cells, size)
{
  if (anyDuplicated(cells))
  {
    warning(moduleName, "> No more than one fire can start in a given pixel during",
            " the same time interval, keeping the largest fire.", immediate. = TRUE)
    
    to_rm <- unlist(
      lapply(
        unique(cells[duplicated(cells)]), 
        function(locus)
        {
          wh <- which(cells == locus)
          sizes <- size[wh]
          wh[-base::which.max(sizes)]
        }
      )
    )
    
    list(loci = cells[-to_rm], sizes = size[-to_rm])
  }
  else list(loci = cells, sizes = size) 
}
