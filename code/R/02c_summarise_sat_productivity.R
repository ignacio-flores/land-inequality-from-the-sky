#Summarise GPP 

#define mode 
debug <- "nop"

#Setup 
setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs)

#set working station 
ncores = 4
plan(multicore, workers=ncores)

#1. LOAD FUNCTIONS AND REPROJECT GADM 

#load some functions 
source("code/R/functions/raster_extractor.R")
source("code/R/functions/rdata_loader.R")
source("code/R/functions/reproject_gadm.R")
source("code/R/functions/tabulate_satdat.R")
source("code/R/functions/area_breakdown.R")

#2 Summarize GPP data 

#load data directory 
gppstack <- loadRData(pathgpp)
gmv = length(gppstack@layers)

# Extract times of raster and name layers 
for (i in 1:length(gppstack@layers)) { 
  old_name <- gppstack@layers[[i]]@data@names
  year <- paste0(str_extract(old_name, "[0-9]{4}"),"-01-01")
  days   <- str_sub(old_name, -3)
  gppstack@layers[[i]]@data@names <-
    as.character(as_date(as.integer(days), origin = as.Date(year)-1))
  if (debug == "yes") {
    print(paste0(i, "-", old_name))
  }
  print(paste0(i, "-", old_name))
}

#break country in chunks
cantons  <- read_sf(paste0(pathgadm, "gadm36_FRA_4.shp")) %>%
  dplyr::select(GID_4) %>% st_transform(crs = 2154)
cantons_c  <- st_point_on_surface(cantons) 
country <- read_sf(paste0(pathgadm, "gadm36_FRA_0.shp")) %>%
  dplyr::select(NAME_0) %>% st_transform(crs = 2154)
ctry_chunks <- area_breakdown(shp = "country", times = 12)
ctry_chunks_list <- ctry_chunks$chunk_id

#define folder to export 
fold <- "georeferenced_data/MODIS/Exported_data/"

#loop over GPP layers 
print(paste0("Started working with GPP rasters at ", Sys.time()))
for (row in 1:length(gppstack@layers)) { 
  
  #check if file already exists
  dp <- paste0(fold, "gadm4_gpp_p_", row, ".csv")
  if (file.exists(dp)) {
    print(paste0("gadm4_gpp_p_", row, ".csv - already exists, skipping"))
  #treat only if it doesn't exist   
  } else {
    arealist <- ctry_chunks_list 
    tic(paste0("Treating GPP rasters ", row/length(gppstack@layers)*100), "%")
    
      #crunch layer by area chunks 
      gpp_part <- future_map_dfr(arealist, 
       ~raster_extractor(
         x = row, 
         y = .x,
         stck=gppstack
       ), .id = "identifier")
      
      #exporter 
      tabulate_satdat(
        df=gpp_part,
        dest=dp
      )
      
      #clean invisible memory 
      #(remove all tif files from tempdir) 
      file_list <- list.files(tempdir(), full.names = T)
      tiff_files <- file_list[grep("\\.tif$", file_list)]
      file.remove(tiff_files)
    toc()
  }
}

 

