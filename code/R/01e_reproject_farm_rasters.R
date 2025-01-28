# Setup
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/rast_reproject_and_save.R")
libraries(stdlibs, geolibs)

# Define raster types
raster_types <- c("feeds","food", "farm") # "foods", "farms"
years <- 2015:2021

for (y in years) {
  rast_reproject_and_save(y, pathcada, raster_types)
}
