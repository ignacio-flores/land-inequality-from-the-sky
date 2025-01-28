
# Activate google key 
library(googleway)
source("code/R/google_api/IF_key.R")
googleway::set_key(key = goog_key, api = "elevation")

#create grid for interpolation 
grid <- st_centroid(gadm) %>% dplyr::select(GID_4, geometry) 

# Extract latitudes and longitudes to prepare query 
coordinates <- st_coordinates(grid)
coord_df <- data.frame(
  lat = coordinates[, "Y"], 
  lon = coordinates[, "X"]
)

# split data frame into smaller chunks
split_data <- function(df, chunk_size = 100) {
  split(df, ceiling(seq_len(nrow(df))/chunk_size))
}
chunks <- split_data(coord_df)

# Loop through chunks 
elevation_results <- list()
for (i in seq_along(chunks)) {
  #encode coordinates 
  latitudes <- chunks[[i]]$lat
  longitudes <- chunks[[i]]$lon
  encoded_polyline <- encode_pl(
    lat = latitudes, 
    lon = longitudes)
  #query 
  elevation_results[[i]] <- google_elevation(
    polyline = encoded_polyline, 
    simplify = TRUE
  )
}
#clean memory 
rm(coordinates, coord_df, chunks)

#organize results 
google_query <- bind_rows(elevation_results, .id = NULL)
google_query <- google_query$results %>% dplyr::select(elevation) #select location too to check lng and lat
grid <- bind_cols(grid, google_query) %>% 
  rename(altitude = `elevation`)

# Save local version 
write_sf(grid, path_gadm4_elev)
rm(google_query)