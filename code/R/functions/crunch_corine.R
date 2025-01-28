#function starts here 
crunch_corine <- function(area) {
  
  one_canton <- filter(all_gadm, GID_4 == gadm_list[area])
  one_canton %<>% mutate(canton_area = st_area(one_canton))
  
  # create string with WKT of all_gadm box in Corine projection 
  bb_gadm <- st_bbox(one_canton)
  bb_gadm_sf <- st_as_sfc(bb_gadm, crs = st_crs(one_canton))
  bb_gadm_sf_trans <- st_transform(bb_gadm_sf, 3035)
  bb_gadm_trans <- st_bbox(bb_gadm_sf_trans)
  
  # Create a WKT POLYGON string
  bb_gadm_string <- paste(
    "POLYGON((",
    bb_gadm_trans["xmin"], " ", bb_gadm_trans["ymin"], ",",
    bb_gadm_trans["xmin"], " ", bb_gadm_trans["ymax"], ",",
    bb_gadm_trans["xmax"], " ", bb_gadm_trans["ymax"], ",",
    bb_gadm_trans["xmax"], " ", bb_gadm_trans["ymin"], ",",
    bb_gadm_trans["xmin"], " ", bb_gadm_trans["ymin"], "))", 
    sep = ""
  )
  
  #Load Corine 
  one_landcover <- st_read(pathcori, 
     layer = "U2018_CLC2018_V2020_20u1", 
     wkt_filter = bb_gadm_string, 
     quiet = T
  )
  # transform projection if necessary
  if (st_crs(one_landcover) != st_crs(one_canton)) {
    one_landcover <- st_transform(one_landcover, st_crs(one_canton))
  }
  
  #intersect 
  one_intersect <- st_intersection(one_landcover, one_canton) 
  one_intersect <- one_intersect[!st_is_empty(one_intersect), ]
  one_intersect <- one_intersect %>%
    left_join(leg, by = c("Code_18" = "CLC_CODE"))
  
  #visual check 
  # tmap_mode("view")
  # tmap_options(check.and.fix = TRUE)
  # tm_shape(one_intersect) + tm_fill(col="LABEL1") + 
  #   tm_shape(one_canton) + tm_borders()
  
  #compute areas 
  one_intersect <- dplyr::select(one_intersect, 
                                 GID_4, Code_18, LABEL1, LABEL3, canton_area, RGB, Shape) %>%
    arrange(Code_18) %>% mutate(obj_area = st_area(one_intersect))
  
  #export table 
  one_intersect <-  tibble(
    canton = one_intersect$GID_4, 
    LABEL1 = one_intersect$LABEL1,
    obj_area = one_intersect$obj_area,
    canton_area = one_intersect$canton_area) 
  one_intersect %<>% group_by(LABEL1, canton) %>%
    summarise(obj_area=sum(obj_area), canton_area=mean(canton_area), .groups="keep") %>%
    mutate(obj_pct = obj_area / canton_area * 100) %>% ungroup() %>%
    mutate(sum_pct = sum(obj_pct))
  
  #count iterations 
  ctr = area / length(gadm_list) * 100
  print(
    paste0(
      "Started at", start, " - ", 
      format(ctr, digits = 2), "% of progress at ",
      Sys.time()
    )
  )
  
  return(one_intersect)
}
