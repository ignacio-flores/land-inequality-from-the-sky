setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/reproject_gadm.R")
source("code/R/functions/safran_cruncher.R")
libraries(stdlibs, geolibs)

ncores = 3
plan(multicore, workers=ncores)

#load grid coordintes 
coordinates_data <- read_delim(
  paste0(path_safr, "raw/coordonnees_grille_safran_lambert-2-etendu.csv"),
  delim = ";", locale = locale(decimal_mark = ",")) %>%
  mutate(LAT_DG = as.numeric(str_replace(LAT_DG, ",", ".")),
         LON_DG = as.numeric(str_replace(LON_DG, ",", ".")))

# Create list of dates
dates <- seq.Date(from = as.Date("2015-01-01"), to = as.Date("2021-12-31"), by = "day")
dates <- format(dates, "%Y%m%d")

# do the work 
extracted_data <- 
  future_map_dfr(
    1:length(dates), 
    ~safran_cruncher(
      sql_db = paste0(path_safr, "sql/safran_weather_data_sql"),
      tvars = tvars <- c('T_Q', 'TINF_H_Q', 'TSUP_H_Q', 'PRELIQ_Q', 'SWI_Q', 'HU_Q', 'WG_RACINE_Q', 'WGI_RACINE_Q', 'PE_Q', 'FF_Q', 'EVAP_Q'),
      date_n = .x
    )
  )

#export full set of results 
write.csv(
  extracted_data, 
  paste0("data/FR/safran_france/exported/spatmean_canton_daily.csv")
)