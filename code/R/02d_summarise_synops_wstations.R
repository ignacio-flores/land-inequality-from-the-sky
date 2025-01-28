# Preamble 
gc()
setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs)
source("code/R/functions/ipol_wdata.R")
library(zoo)

#use api or local for altitudes? 
mode <- "local" #"google_api"

# Read weather-station records 
cols_to_read <- 
  c("ID OMM station", "Date", "Température (°C)", 
     "Latitude", "Longitude"
    #"Pression au niveau mer", "Humidité", "Vitesse du vent moyen 10 mn",
    #"Pression station", "Précipitations dans la dernière heure",
    #"EPCI (name)", "EPCI (code)",
  )
data <- read_delim(path_synop_data, show_col_types = F) %>% 
  dplyr::select(all_of(cols_to_read)) %>% clean_names()
data %<>% arrange(id_omm_station, date) %>% 
  filter(!is.na(temperature_c))

length(unique(data$id_omm_station))

# Get daily info  
mtdb <- data %>% 
  mutate(dmy = format(date, "%Y-%m-%d")) %>%
  group_by(id_omm_station, dmy) %>% 
  mutate(hr = as.numeric(format(date, "%H")), 
         aft = if_else(hr %in% 12:15 , 1, NA),
         aftmax = aft * max(temperature_c, na.rm = TRUE),
         aft = aft * temperature_c,
         ) %>%
  summarise(
    maxtemp = max(temperature_c, na.rm = TRUE),
    aftmean = mean(aft, na.rm = TRUE)
  ) %>% 
  filter(!is.na(aftmean))
head(mtdb, n=20)

# Georeference stations and crop to continental FR
stos <- read_sf(path_synop_coor) %>% clean_names() 
gadm <- read_sf(path_gadm4) 
if (st_crs(stos) != st_crs(gadm)) {
  stos <- st_transform(stos, st_crs(gadm))
}
stos <- st_crop(stos, gadm)
mtdb <- left_join(stos, mtdb, by = c("id" = "id_omm_station"))
rm(stos, data)

#load altitude data for cantons 
if (mode == "google_api") {
  client_key <- "code/R/google_api/IF_key.R"
  if (file.exists(client_key)) {
    source("code/R/functions/get_gadm4_elevations_with_google.R")
  } else {
    print("You must define a key to use Google's API")
    break 
  }
} 
if (mode == "local") {
  grid <- read_sf(path_gadm4_elev)
}

#check elevation data visually  
#sample_stations <- filter(mtdb, dmy == "2015-07-13")
#tmap_mode("view")
#tm_shape(grid) + tm_dots(col = "altitude", size = .025) + 
#  tm_shape(sample_stations) + tm_dots(col = "black", size = .05)

#list all possible dates 
all_dates <- unique(mtdb$dmy)
all_dates <- all_dates[-grep("2023", all_dates)]

#interpolate temperatures 
ipv <- "aftmean"
mod <- "kri" #idw
tic(paste0(ipv, " interpolation"))
  results <- purrr::map_dfr(all_dates, 
     ~ipol_wdata(
       date = .x, 
       weather.data = mtdb, 
       wvar = ipv,
       target = grid, 
       mode = mod 
     )
)
toc()

#export full set of results 
write.csv(
  results, 
  paste0("data/FR/synop_france/ipol/ipol_canton_daily_", ipv , "_", mod,".csv")
)
