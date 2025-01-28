#Summarise temperatures 
debug <- "nope"  #"yes"

#Setup 
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/raster_extractor.R")
source("code/R/functions/rdata_loader.R")
source("code/R/functions/reproject_gadm.R")
libraries(stdlibs, geolibs)

#set working station 
ncores = 2
plan(multisession, workers=ncores)

#1. SUMMARIZE TEMPERATURES ...

#load temperature stack
tempstack <- loadRData(pathtemp)
tmv <- length(tempstack@layers)

# Write dates in temperature stack
for (i in 1:tmv) {
  old_name <- tempstack@layers[[i]]@data@names
  year <- paste0(str_extract(old_name, "[0-9]{4}"),"-01-01")
  days <- str_sub(old_name, -3)
  tempstack@layers[[i]]@data@names <-
    as.character(as_date(as.integer(days), origin = as.Date(year)-1))
}

#extract temperatures
print(paste0("Started working with temp rasters at ", Sys.time()))
tic("Treating temperature rasters")

  #list raster layers
  if (debug == "yes") {
    templist <- c(181, 258, 179)
  } else templist <- 1:tmv
  lgt = tmv

  #call raster extractor
  parall_temp <- future_map_dfr(
    templist,
    ~raster_extractor(.x, stck=tempstack),
    .id="identifier"
  )
  
  #clean output
  parall_temp %<>% arrange(GID_4, date) %>%
    dplyr::select(GID_4, identifier, date, canton, farms, foods) %>%
    rename(temp_canton = `canton`, temp_farms = `farms`,
    temp_foods = `foods`, id = `identifier`)
  head(parall_temp)
  
  #save
  if (debug != "yes") {
    write.csv(parall_temp,
      "georeferenced_data/MODIS/Exported_data/gadm4_temp_pll.csv")
  }
toc()

#inform activity
print(paste0("Finished working with temp rasters at ", Sys.time()))

#free space
rm(tempstack)


