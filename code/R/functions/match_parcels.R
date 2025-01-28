# Match parcels to farms and cantons

match_parcels <- function(x) {
  
  # Get the canton border and change crs to RGF93
  gadm4 <- vect(file.path(pathgadm, "gadm36_FRA_4.shp"))
  canton <- gadm4[gadm4$GID_4 == x] %>% project("epsg:2154")
  rm(gadm4)
  
  # Create a 10 km buffer around canton border (units in m)
  canton_buffer <- aggregate(terra::buffer(canton, 10000))
  
  # Load parcel data
  if (t < 2019) {
    parcel <- vect(
      file.path(pathcada, t, "PARCELLES_GRAPHIQUES.shp"), 
      filter = canton_buffer
    )
  }
  if (t >= 2019) {
    parcel <- vect(
      file.path(pathcada, t, "PARCELLES_GRAPHIQUES.gpkg"), 
      filter = canton_buffer
    )
  }
  
  # Drop duplicates and fix geo
  parcel <- parcel[!duplicated(parcel$ID_PARCEL), ]
  parcel %<>% terra::makeValid()
  #parcel <- sf::st_as_sf(parcel) %>% st_make_valid() %>% vect()
  
  # Calculate parcel area and perimeter
  parcel$parcel_perim <- perim(parcel)
  parcel$parcel_area <- expanse(parcel)/10000 
  
  # Convert parcel polygons to points on surface using sf package
  parcel %<>% sf::st_as_sf()
  st_agr(parcel) = "constant"  # to suppress the warning message
  parcel %<>% st_point_on_surface() %>% vect()
  
  # Load farm data
  if (t < 2019) {
    farm <- vect(
      file.path(pathcada, t, "ILOTS_ANONYMES.shp"), 
      filter = canton_buffer
    )
  }
  if (t >= 2019) {
    farm <- vect(
      file.path(pathcada, t, "ILOTS_ANONYMES.gpkg"), 
      filter = canton_buffer
    )
  }
  
  # Drop duplicates and fix geo
  farm <- farm[!duplicated(farm$ID_ILOT), ]
  rm(canton_buffer)
  farm %<>% terra::makeValid()
  #farm <- sf::st_as_sf(farm) %>% st_make_valid() %>% vect()
  
  # Calculate farm area in ha and perimeter
  farm$farm_area <- expanse(farm)/10000 
  farm$farm_perim <- perim(farm)
  
  # Match parcels to farms using intersect
  matched <- terra::intersect(farm, parcel)
  matched %<>% as.data.frame()
  rm(parcel)
  
  # Convert farm polygons to points on surface using sf package
  farm %<>% sf::st_as_sf()
  st_agr(farm) = "constant"  # to suppress the warning message
  farm %<>% st_point_on_surface() %>% vect() 
  
  # Crop farm using canton border
  farm %<>% crop(canton)
  farm %<>% as.data.frame()
  
  # Remove farms that are not in the canton
  matched %<>% merge(farm)
  
  # Calculate canton area and perimeter
  canton$canton_perim <- perim(canton)
  canton$canton_area <- expanse(canton)/10000 
  
  # Merge canton info with matched data
  canton %<>% as.data.frame()
  matched %<>% merge(canton)

  rm(farm, canton)
  
  # Return
  return(tibble(matched))
}

# # Example code to plot the matching strategy
#   png("figures/intuitive/parcel_to_ilot_match.png", 
#        width=350, height=233, units='mm', res = 200)
#   plot(canton, col="lightgray")
#   plot(parcel, add=TRUE)
#   plot(farm, add=TRUE, col="purple")  
#   dev.off()