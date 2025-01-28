#define function to rasterize 
rasterize_farms <- function(x, r_template, lyr, file, fid_list, incr.res=1, temp.fld, type) {
  
  #keep food-only and simplify geometries 
  tic(paste0("* ", t,": ", "(",x,")", "  whole block"))
  
    # extract polygons from group x using SQL
    maxid = max(unlist(fid_list[x]))
    minid = min(unlist(fid_list[x]))
    sql_phrasing <- paste0(
      'SELECT * FROM "', lyr,'" WHERE FID BETWEEN ', minid, ' AND ', maxid)
    pcl_shp <- st_read(file, query = sql_phrasing, quiet=T) %>% 
      st_make_valid() %>% ms_simplify(keep_shapes = T) 
    
    #crop raster and refine resolution if needed  
    if (incr.res != 1) {
      tic(paste0("* ", t, " increasing resolution of template ", incr.res, " times"))
        rt <- st_crop(read_stars(r_template), pcl_shp) %>%
          terra::rast() %>% terra::disagg(incr.res) %>% raster::raster() %>% st_as_stars(proxy=T) 
    
      toc()
    } else {
      rt <- read_stars(r_template) 
    }
    
    #exclude some code groups 
    excluded_cg <- conditions_by_type[[type]]$excluded_code_groups
    if (any(!is.na(excluded_cg))) {
      pcl_shp %<>% filter(!(CODE_GROUP %in% excluded_cg))
    }
    
    # Exclude specific code_cultu
    specific_excl <- conditions_by_type[[type]]$specific_exclusions
    if (!is.null(specific_excl) && any(!is.na(specific_excl))) {
      if (is.list(specific_excl[[1]])) {
        # Handle case when specific_exclusions is a list of lists
        for (condition in specific_excl) {
          pcl_shp <- pcl_shp %>%
            filter(!(CODE_GROUP == condition$code_group & CODE_CULTU %in% condition$code_cultu))
        }
      } else {
        # Handle case when specific_exclusions is a single list
        pcl_shp <- pcl_shp %>%
          filter(!(CODE_GROUP == specific_excl$code_group & CODE_CULTU %in% specific_excl$code_cultu))
      }
    }
    
    #rasterize 
    r_pcl <- st_rasterize(pcl_shp, template=rt, align=T) 
  toc()

  if (!dir.exists(temp.fld)) {
    dir.create(temp.fld)
  } 
  
  write_stars(r_pcl, file.path(temp.fld, paste0(type, "_ra", x, ".tif"))) 
  
  #return raster 
  return()
} 
