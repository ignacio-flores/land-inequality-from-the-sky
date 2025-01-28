#raster preparer 
PrepRastList <- function(rl) {
  names(rl) <- NULL
  rl$fun <- max 
  rl$na.rm <- TRUE
  return(do.call(raster::mosaic, rl))
}