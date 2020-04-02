.doDataChecks <- function(moduleName, envir, attribs, fml){
  
  if (is.null(envir[["fireAttributesFireSense_SpreadFit"]]))
    stop(moduleName, "> '", attribs, "' not found in data objects or NULL.")
  
  if (!is(envir[["fireAttributesFireSense_SpreadFit"]], "SpatialPointsDataFrame"))
    stop(moduleName, "> '", attribs, "' is not a SpatialPointsDataFrame.")
  
  if (is.null(envir[["fireAttributesFireSense_SpreadFit"]][["size"]]))
    stop(moduleName, "> The SpatialPointsDataFrame '", attribs, "' must have a 'size' column.")

  if (is.empty.model(fml))
    stop(moduleName, "> The formula describes an empty model.")
  
}