
#define standard function for information extraction 
raster_extractor <- function(x, y, stck) {
  
  #0. PRELIMINARY WORK ...
  
  #identify stack type 
  if (grepl("Surf_Temp_Monthly", stck@layers[[x]]@file@name, fixed = TRUE)) {
    short_name <- "temp"
    nr = 45
  }
  if (grepl("Gross_PP_GapFil", stck@layers[[x]]@file@name, fixed = TRUE)) {
    short_name <- "gpp"
    nr = 45
  }
  
  #get chunk boundary and add a buffer
  area   <- ctry_chunks %>% filter(chunk_id == y)
  area_b <- area %>% st_buffer(10000) #in meters

  #separate layer from stack 
  lyr <- stck@layers[[x]]
  #crop the lyr using the area's boundary
  lyr <- raster::crop(lyr, area_b)
  lyr <- raster::mask(lyr, area_b)
  
  #redefine paths
  orig_path <- lyr@file@name
  new_path <- file.path(glue(getwd(), substring(orig_path, nr)))
  lyr@file@name <- new_path

  #define date
  colnam <- lyr@data@names

  #fill null values as NA
  if (short_name == "temp") {
    lyr %<>% reclassify(cbind(0, NA))
  }
  if (short_name == "gpp") {
    lyr %<>% raster::reclassify(c(3.276, 4, NA))  
  }
  year <- str_extract(lyr@data@names, "[0-9]{4}")
  
  # I. SUMMARIZE INFO WITHIN CANTON ...

  #summarize raster in cantons
  d_cant <- exact_extract(
    lyr, gadm[["gadm4"]], 'mean', force_df=T,
    append_cols=c('GID_4'), stack_apply = T, progress = FALSE
  )
  names(d_cant)[2] <- "canton"
  
  #keep only values that are within the area
  cant_list <- cantons_c[sf::st_intersects(cantons_c, area, sparse = FALSE), ]
  cant_list <- cant_list$GID_4
  d_cant %<>% filter(GID_4 %in% cant_list)
  
  #convert to celsius if necessary 
  if (short_name == "temp") {
    d_cant %<>% mutate(canton = canton - 273.15)
  }
  
  # II. SUMMARIZE INFO FOR ALL FARMS ...

  #define path to farm rasters
  p_farm <- paste0(pathcada, "/", year, "/", fileilot_rast, "_rep.tif")
  #treat all farms equally
  if (file.exists(p_farm))  {
    
    #open farm raster 
    r_farm <- read_stars(p_farm)
    names(r_farm) <- "farm" 
    
    #crop it 
    r_farm %<>% st_crop(area_b)
    
    #recode missings 
    r_farm %<>% mutate(farm=if_else(!is.na(farm), 1, NA))
    lyr <- st_warp(st_as_stars(lyr), r_farm) 
    
    #extract farm info 
    d_farm <- rast(lyr)*rast(r_farm) 
    d_farm <- exact_extract(d_farm, gadm[["gadm4"]], 'mean', force_df=T, append_cols=c('GID_4'),
        stack_apply = T, progress = FALSE) #doesn't work well with pipe, keep extented form 
    names(d_farm)[2] <- "farms"
    
    #convert to celsius if necessary 
    if (short_name == "temp") {
      d_farm %<>% mutate(farms = farms - 273.15)
    }
    
    #again, keep values only for area 
    d_farm %<>% filter(GID_4 %in% cant_list)
   
    #free some space 
    rm(r_farm)
  } else d_farm <- NULL
  
  
  # III. EXTRACT INFO FOR FOOD-CROPS ONLY ...

  #define path 
  p_food <- paste0(pathcada, "/", year, "/", filefood_rast, "_rep.tif")
  
  if (file.exists(p_food)) {
    
    #open farm raster and crop satellite layer
    r_food <- read_stars(p_food) 
    names(r_food) <- "food"
    
    #crop this using the state boundary with buffer
    r_food %<>% st_crop(area_b) 
    
    #recode missings 
    r_food %<>% mutate(food=if_else(!is.na(food), 1, NA))
    
    #extract farm info 
    d_food <- rast(lyr)*rast(r_food) 
    d_food <- exact_extract(d_food, gadm[["gadm4"]], 'mean', force_df=T, append_cols=c('GID_4'),
      stack_apply = T, progress = FALSE) #doesn't work well with pipe, keep extented form 
    names(d_food)[2] <- "foods"
    
    #convert to celsius if necessary 
    if (short_name == "temp") {
      d_food %<>% mutate(foods = foods - 273.15)
    }
    
    #again, keep values only for area 
    d_food %<>% filter(GID_4 %in% cant_list)
    
    rm(r_food)
  } else d_food <- NULL
  
  #merge if necessary
  df <- d_cant 
  rm(d_cant)
  if (!is.null(d_farm)) {
    df %<>% merge(d_farm, by="GID_4")
    rm(d_farm)
  }
  if (!is.null(d_food)) {
    df %<>% merge(d_food, by="GID_4")  
    rm(d_food)
  }
  #add dates 
  df %<>% mutate(date = paste0(gsub("X", "", colnam)))
  
  return(tibble(df))
}
