#Build raster of ilots 

#setup 
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/rasterize_farms.R")
source("code/R/functions/PrepRastList.R")
source("code/R/functions/find_cadastre_path.R")
source("code/R/functions/conditions_by_type.R")
libraries(stdlibs, geolibs)

#debug?
debug <- "nop" # "yes"

#define list of years and plan work  
years <- 2015:2021
typelist <- c("feeds") #c("farms", "feeds", "foods")
future::plan("multicore", workers = 4)
scatter = 100 #break parcel data to parallelize 

if (debug == "yes") {
  years <- 2015:2016
}

#I. RASTERIZE PARCELS... 
print(paste0("Started rasterizing food ilots at ", Sys.time(), " for years:"))
print(years)
for (t in years) {
  
  #define path for temporary files 
  tfile <- file.path(pathcada, t, "temp")
  
  #inform activity
  if (t == min(years)) {
    if (debug == "yes") {
      print("debugging mode on")
    }
    if (debug == "nop") {
      print("all in mode")
    }
  }
  print(paste0("Started working with ", t, " at ", Sys.time()))
  tic(paste0("... Done processing ", t))  
  
    #get year specific file-naming 
    nampath <- find_cadastre_path(t)
    nam <- nampath[[1]]
    pcl <- nampath[[2]]
      
    #generate light version of dbase
    nogeo <- file.path(pathcada, t, "no_geom")
    tic("removing big file geometries to list parcels")
      gdalUtilities::ogr2ogr(
        src_datasource_name = pcl, dst_datasource_name = nogeo,
        select = 'ID_PARCEL, CODE_GROUP', nlt = 'NONE', overwrite = T
      )
      big_id <- st_read(file.path(nogeo, paste0(nam, ".dbf")), fid_column_name = "fid", quiet=T)
      big_id %<>% mutate(fid=as.numeric(fid)) %>% arrange(fid)
      #mini version for debugging 
      if (debug == "yes") {
        big_id %<>% filter(fid <= 1000)
      }
      
      #list parcels in groups 
      big_id %<>% 
        mutate(grouper = ntile(fid, scatter)) %>% group_by(grouper) %>% 
        summarise(ma = max(fid), mi=min(fid))
      sma_id <- pivot_longer(big_id, cols = c("ma", "mi"), names_to = "typ", values_to = "fid") %>% 
        arrange(fid) %>% group_by(grouper) 
      lister <- split(as.numeric(sma_id$fid), sma_id$grouper)
      rm(big_id, sma_id)
    toc()
     
    #get rid of remains from previous iterations  
    if (dir.exists(tfile)) {
      unlink(tfile, recursive = T)
    }
  
    for (type in typelist) {
 
      #run rasterization function 
      tic(paste0(t, ": Rasterizing ", type))
        future_map(
          1:scatter,
          ~rasterize_farms(.x,
              r_template = gpp_example,
              file = pcl,
              lyr = nam,
              fid_list = lister, #list of lists (takes min/max within sub-lists)
              incr.res = 7, #increase template resolution (460/x ~ 25ha/x), takes a lot of memory
              temp.fld = tfile,
              type = type 
          )
        )
      toc()
    
      #reunite rasters from list
      tic(paste0(t, ": Reunite scattered ", type, " rasters"))
      
        # define raster opener 
        openRasta <- function(r) {
          return(raster(r))
        }
        
        #list open and merge all rasters 
        flist <- list.files(tfile, pattern=paste0(type, "_ra*"), full.names = T)
        flist <- mapply(openRasta, flist)
        allf <- PrepRastList(flist) #mosaic rasters
   
      #save 
      if (debug != "yes") { 
        
        #write paths for rasters  
        fdr <- file.path(pathcada, t, paste0(type, "_parcels_light.tif"))
  
        #save raster 
        if (file.exists(fdr)) {
          file.remove(fdr)
        }
        raster::writeRaster(allf, fdr)
        
        #delete temporary files 
        unlink(tfile, recursive = T)
        
        #clean invisible memory 
        file_list <- list.files(tempdir(), full.names = T)
        tiff_files <- file_list[grep("\\.tif$", file_list)]
        file.remove(tiff_files)
      }
      toc()  
    toc()
  }
}  

