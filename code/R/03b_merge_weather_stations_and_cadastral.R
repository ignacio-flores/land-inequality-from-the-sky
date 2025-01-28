# Preamble 
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/summarize_temp_bins.R")
libraries(stdlibs)

#read cadastral data
cad <- read_delim("data/FR/cadastre/temp/all_yearly_pa28.csv", show_col_types = F) %>% 
  rename(GID_4 = `gid_4`) 
shockvars <- c('avg_shock', 'min_shock', 'max_shock')
wsh <- select(cad, year, GID_4, all_of(shockvars), type) %>% 
  filter(type == "foods") %>% arrange(GID_4, year) %>% select(-c("type"))

# read safran temperatures 
saf <- read_delim(paste0(path_safr, "exported/spatmean_canton_daily.csv")) %>% 
  select(-c("...1")) %>% rename(canton_id = `GID_4`) %>%
  mutate(date = as.character(date)) %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  arrange(canton_id, date)
safcols <- c('T_Q', 'TINF_H_Q', 'TSUP_H_Q', 'PRELIQ_Q', 'SWI_Q', 'HU_Q', 'WG_RACINE_Q', 'WGI_RACINE_Q', 'PE_Q', 'FF_Q', 'EVAP_Q')

# read interpolated temps 
ipo <- read_delim(paste0("data/FR/synop_france/ipol/ipol_canton_daily_aftmean_kri.csv")) %>% 
  arrange(canton_id, date)

# read most granular gpp files 
gpp_files <- list.files(
  path = "georeferenced_data/MODIS/Exported_data", 
  full.names = TRUE
)
gpp_files <- gpp_files[grep("gadm4_gpp_p", gpp_files)]
gpp <- lapply(gpp_files, read.csv) %>% bind_rows() %>%
 rename(gpp_canton = `canton`, gpp_farms = `farms`, gpp_foods = `foods`) %>% 
 dplyr::select(-c("X", "identifier")) %>%
  pivot_longer(cols = c(gpp_canton, gpp_farms, gpp_foods),
               names_to = "type", values_to = "gpp") %>% 
  mutate(type = sub("gpp_", "", type), date = as.Date(date, format="%Y.%m.%d")) %>% 
  filter(!is.na(gpp) & type != "canton")
head(gpp)

# group obs by time intervals 
intervals <- unique(gpp$date)
ipo$int <- cut.Date(ipo$date, breaks = as.Date(intervals))
saf$int <- cut.Date(saf$date, breaks = as.Date(intervals))
ipo %<>% group_by(canton_id, int) %>% mutate(date = as.Date(date))

# add critical temperatures to saf 
saf %<>% mutate(saf, year = format(date, "%Y")) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(wsh, by = c("canton_id" = "GID_4", "year" = "year")) %>%
  select(-c("year")) %>%group_by(canton_id, int) %>% 
  mutate(date = as.Date(date))

#summarize temperatures by interval 
bintemp <- summarize_temp_bins(saf) %>% select(-c("NA"))

#aggregate to 8days intervals 
agg_ipo <- ipo %>% summarise(ipol_aftmean = mean(ipol_aftmean)) %>% 
  rename(GID_4 = `canton_id`, date = `int`) 
agg_saf <- saf %>% summarise_at(vars(all_of(safcols)), mean, na.rm = TRUE) %>% 
  left_join(bintemp, by = c("canton_id", "int")) %>%
  rename(GID_4 = `canton_id`, date = `int`)
rm(ipo, saf)

# Convert the date column in agg from factor to date
agg_ipo$date <- as.character(agg_ipo$date)
agg_ipo$date <- as.Date(agg_ipo$date)
agg_saf$date <- as.character(agg_saf$date)
agg_saf$date <- as.Date(agg_saf$date)
agg <- left_join(agg_saf, agg_ipo, by = c("GID_4", "date"))

#merge weather station and satellite data 
mer <- left_join(agg, gpp, by = c("GID_4", "date")) %>% 
  ungroup() %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  rename(temp = `ipol_aftmean`)
rm(gpp, agg)
gc()

#merge with cadastral data and save 
all <- left_join(mer, cad, by = c("GID_4", "type", "year")) %>% 
  filter(!is.na(type)) %>% clean_names() %>% 
  mutate(month = as.numeric(substr(date, 6, 7))) 
safcols <- tolower(safcols)

all <- all %>%
  dplyr::select(type, gid_4, name_4, date, gpp, temp, tsup_h_q, hshock_avg, hshock_min, hshock_max, 
    t0_5, t5_10, t10_15, t15_20, t20_25, t25_30, t30_35, t35_40, t40_45, all_of(safcols), gini, herfindhal, 
    year, month, canton_area, farm_area_est, farm_perim_est, ccount, dherf, dcount, 
    nfarms, cv, sd_logs, avg_shock, min_shock, max_shock, small_farm, medium_farm, large_farm, vlarge_farm,
    small_farm_pt, medium_farm_pt, large_farm_pt, vlarge_farm_pt,
    crop1, crop2, crop3, crop4, crop5, crop6, crop7, crop8, crop9, crop11, 
    crop14, crop15, crop16, crop17, crop18, crop19, crop20, crop21, crop22, 
    crop23, crop24, crop25, crop28) %>%
    rename(wshock = `avg_shock`)
write.csv(all, "data/FR/_part_c28/working_panel_8days_all_pa28.csv")  

