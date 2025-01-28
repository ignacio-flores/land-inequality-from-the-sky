summarise_forests <- function(area) {
  
  #isolate canton 
  one_canton <- filter(all_gadm, GID_4 == gadm_list[area])
  one_canton %<>% mutate(canton_area = st_area(one_canton))
  
  #intersect 
  one_intersect <- st_intersection(all_forests, one_canton) 
  one_intersect %<>% mutate(forest_area = st_area(one_intersect))
  
  #summarize % of forest area 
  one_forest_pct <-  tibble(
    canton = one_intersect$GID_4, 
    forest_area = one_intersect$forest_area,
    canton_area = one_intersect$canton_area) %>% 
    group_by(canton, canton_area) %>% 
    summarise(forest_area = sum(forest_area)) %>%
    mutate(forest_pct = forest_area / canton_area * 100)
  
  ctr = area / length(gadm_list) * 100
  print(
    paste0(
      "Started at", start, " - ", 
      format(ctr, digits = 2), "% of progress at ",
      Sys.time()
    )
  )
  
  return(one_forest_pct)
}
