fireSense_SpreadFitRaster <- function(model, data, par)
{
  drop( model.matrix(model, data) %*% par )
}
