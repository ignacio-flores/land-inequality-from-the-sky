# Function to reproject and save raster files
rast_reproject_and_save <- function(year, pathcada, raster_types) {
  tic(paste0("reprojecting rasters, ", year))
  for (type in raster_types) {
    # Define source and destinations paths 
    fdr_raster <- file.path(pathcada, year, paste0(type, "_parcels_light.tif"))
    rep_raster <- file.path(pathcada, year, paste0(type, "_parcels_light_rep.tif"))
    # Open, reproject and save 
    raster <- rast(fdr_raster)
    raster_reprojected <- terra::project(raster, "EPSG:2154")
    terra::writeRaster(raster_reprojected, rep_raster, overwrite = TRUE)
    # Clean up
    rm(raster, raster_reprojected)
  }
  gc() 
  toc()
}