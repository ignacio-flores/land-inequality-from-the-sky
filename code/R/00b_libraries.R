require(easypackages)
stdlibs <- c("tidyverse", "tidyr", "devtools", "purrr", "furrr", "readr", "tibble", 
             "dplyr", "tictoc", "glue", "lubridate", "magrittr", "janitor", 
             "pushoverr", "progress", "stringr", "webshot", "shiny", "RSQLite",
             "htmlwidgets", "easypackages", "openxlsx", "corrplot")
geolibs <- c("terra", "raster", "gdalUtilities", "Recrop",
  "exactextractr", "sf", "sp", "gstat", "tmap", "tmaptools", "stars", 
  "rmapshaper", "leaflet", "exactextractr", "svMisc") #"rgdal", "gdalUtils"
all_libs <- c(stdlibs, geolibs)

# in addition, you need to install GDAL in your computer, 
# if you are using a Mac you can type, in the terminal: 
# brew install gdal 
# also install_github("lbusett/MODIStsp")
#you will also need easypackages
