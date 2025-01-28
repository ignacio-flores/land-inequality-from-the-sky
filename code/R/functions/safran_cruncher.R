
safran_cruncher <- function(sql_db, tvars, date_n) {
  
  date <- dates[date_n]
  tic(paste0("crunching ", date, " (", round(date_n/length(dates)*100, digits = 2), "%)..."))
  
  # prepare query 
  con <- dbConnect(RSQLite::SQLite(), dbname = sql_db)
  query <- paste("SELECT LAMBX, LAMBY, DATE,", paste(tvars, collapse = ", "), 
                 "FROM weather_data WHERE DATE = '", date, "'", sep = " ")
  
  # get data and bind with coordinates 
  
  selected_data <- dbGetQuery(con, query)
  selected_data <- selected_data %>% 
    left_join(coordinates_data, by = c("LAMBX" = "LAMBX (hm)", "LAMBY" = "LAMBY (hm)"))
  
  # convert to spatial data frame
  weather_sf <- st_as_sf(selected_data, coords = c("LON_DG", "LAT_DG"), crs = 4326)
  weather_sf <- st_transform(weather_sf, crs = 2154)
  
  # Set the resolution to 8 km (8000 meters) as specified in the documentation
  resolution <- 8000
  
  # Create raster from point data using specified resolution
  r <- raster(extent(weather_sf), resolution = resolution, crs = st_crs(weather_sf)$proj4string)
  weather_raster <- rasterize(weather_sf, r, field = tvars, fun = mean)
  
  # Extract spatial mean temperature for each polygon
  mean_temps <- exact_extract(
    weather_raster, gadm[["gadm4"]], 'mean', force_df=T,
    append_cols=c('GID_4'), stack_apply = T, progress = FALSE) %>%
    rename_with(~ sub("^mean\\.", "", .x), starts_with("mean.")) %>%
    mutate(date = date)
  
  #check results visually
  # gadm0 <- gadm[["gadm0"]]
  # cantons <- left_join(gadm[["gadm4"]], mean_temps, by = "GID_4")
  # tmap_mode("plot")
  # 
  # # Example plotting code
  # map <- tm_shape(weather_raster$TSUP_H_Q) +
  #   tm_raster(col = "TSUP_H_Q", title = "Max Temp °C", style = "cont") +
  #   tm_shape(gadm0) + 
  #   tm_borders() +
  #   tm_layout(legend.position = c("left", "bottom"), legend.title.size = 1.5,
  #             legend.text.size = 1.5, legend.height = 1.5)
  # tmap_save(map, filename = paste0("maps/safran_raster_map_20150719.png"), width = 10, height = 7, dpi = 300)
  # 
  toc()
  return(mean_temps)
}