setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)

# Define the database path
db_path <- paste0(path_safr, "sql/safran_weather_data_sql")

# Remove the database file if it already exists
if (file.exists(db_path)) {
  file.remove(db_path)
}

# Create a connection to a new SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# open weather data files 
tic("opening data")
weather_data_10_19 <- read_delim(paste0(path_safr, "raw/SIM2_2010_2019.csv"), delim = ";")
weather_data_20_pl <- read_delim(paste0(path_safr, "raw/QUOT_SIM2_previous-2020-202406.csv"), delim = ";")
toc()
tic("binding")
full_db <- rbind(weather_data_10_19, weather_data_20_pl)
rm(weather_data_10_19, weather_data_20_pl)
toc()

# save and disconnect
tic("saving")
dbWriteTable(con, "weather_data", full_db, overwrite = TRUE)
toc()
dbDisconnect(con)