#preamble 
#preliminary 
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
source("code/R/functions/check_gid_name.R")
libraries(stdlibs)

#prepare yearly data
yearly <- read.csv("data/FR/cadastre/temp/all_yearly_pa28.csv")

#open temperatures 
m_temp <- read.csv(
    file.path(pathmodi, "gadm4_temp_pll.csv")) %>%
  mutate(year_month = substring(date, first=1, last=7)) %>%
  dplyr::select(-c(id, date)) %>% 
  pivot_longer(
    cols = c(temp_farms, temp_foods, temp_canton), 
    names_to = "type", 
    values_to = "temp", 
    values_drop_na = TRUE,
    names_prefix = "temp_"
  )

#open gpp 
gpplist <- list.files(pathmodi, pattern="gadm4_gpp_p", full.names = T)
m_gpp <- map_dfr(gpplist, ~{
  print(.x)
  read.csv(.x)
  })
m_gpp %<>% 
  mutate(year_month = substring(date, first=1, last=7), 
         year_month = gsub("\\.", "-", year_month)) %>%
  group_by(GID_4, year_month) %>% 
  summarise(
    gpp_canton = sum(canton), 
    gpp_farms =  sum(farms), 
    gpp_foods =  sum(foods)) %>%
  ungroup() %>%
  pivot_longer(
    cols=c(gpp_farms, gpp_foods, gpp_canton), 
    names_to = "type", 
    values_to = "gpp", 
    values_drop_na = TRUE,
    names_prefix = "gpp_")

#merge monthly data 
monthly_dat <- full_join(m_gpp, m_temp, by = c("GID_4", "year_month", "type")) %>% 
  mutate(
    year  = as.numeric(substring(year_month, first = 1, last = 4)), 
    month = as.numeric(substring(year_month, first = 6, last = 8))
  ) %>% 
  rename(gid_4 = `GID_4`) %>% 
  dplyr::select(-c(year_month)) 
rm(m_gpp, m_temp)  
head(monthly_dat)

#merge yearly to monthly  
fullpanel <- full_join(yearly, monthly_dat, by = c("gid_4", "year", "type")) %>% 
  dplyr::select(type, gid_4, name_4, year, month, canton_area, farm_area_est, 
  farm_perim_est, gpp, temp, gini, herfindhal, ccount, dherf, dcount, 
  nfarms, cv, sd_logs, avg_shock, min_shock, max_shock, small_farm, 
  medium_farm, large_farm, vlarge_farm, 
  small_farm_pt, medium_farm_pt, large_farm_pt, vlarge_farm_pt,
  crop1, crop2, crop3, crop4, crop5, crop6, crop7, crop8, crop9, crop11, 
  crop14, crop15, crop16, crop17, crop18, crop19, crop20, crop21, crop22, 
  crop23, crop24, crop25, crop28) %>%
  rename(wshock = `avg_shock`)
rm(yearly, monthly_dat) 
write.csv(fullpanel, "data/FR/_part_c28/working_panel_month_all_pa28.csv")


