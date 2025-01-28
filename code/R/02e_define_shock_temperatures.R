#preamble 
gc()
setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)
library(kableExtra)

#bring info from papers 
pap <- "data/temp_thresholds/inputs/papers_clean_table.xlsx"
pap %<>% readxl::read_xlsx() %>% clean_names() %>% 
  select(crop, summer_tipping_point_o_c, reference) %>%
  rename(thr = `summer_tipping_point_o_c`) 
pap$thr[pap$crop == "Rapeseed"] <- "27" 
pap %<>% mutate(thr = as.numeric(thr))

#classify crops 
crop_codes <- c(
  "Winter wheat"  = "BTH", 
  "Spring wheat"  = "BTP",
  "Winter barley" = "ORH", 
  "Spring barley" = "ORP",
  "Corn/Maize"    = "MIS",
  "Rapeseed"      = "CZH", 
  "Sunflower"     = "TRN", 
  "Alfalfa"       = "LUD",
  "Soybean"       = "SOJ"
)
pap$matcher <- NA
pap$matcher <- crop_codes[pap$crop]

#store ecocrop in memory 
ecocrop <- "data/temp_thresholds/inputs/Crops OAMA.xlsx" %>% 
  readxl::read_xlsx() %>% clean_names() %>% 
  dplyr::select(code_culture, libelle_culture, absolute_max, optimal_max) %>% 
  mutate(absolute_max = as.numeric(absolute_max), 
         optimal_max =  as.numeric(optimal_max),
         avgtemp_max = (absolute_max + optimal_max)/2) %>% 
  mutate(code_culture = gsub("[[:digit:]]", "_", code_culture)) %>%
  group_by(code_culture, libelle_culture) %>% 
  summarise(absolute_max = mean(absolute_max), 
            optimal_max =  mean(optimal_max), 
            avgtemp_max =  mean(avgtemp_max)) %>%
  rename(code_cult2 = `code_culture`, crop_fr = `libelle_culture`) %>%
  mutate(matcher = code_cult2) 

#define special cases
ecocrop$matcher[ecocrop$code_cult2 %in% c("CHT", "BDH", "TTH")] <- "BTH"
ecocrop$matcher[ecocrop$code_cult2 %in% c("CPT", "BDP", "BDT", "TTP")] <- "BTP"
ecocrop$matcher[ecocrop$code_cult2 %in% c("MIE", "MID")] <- "MIS"
ecocrop$matcher[ecocrop$code_cult2 %in% c("CZP")] <- "CZH"
ecocrop$matcher[ecocrop$code_cult2 %in% c("LU_", "LUZ")] <- "LUD"

#match papers and ecocrop 
matched <- left_join(ecocrop, pap, by="matcher")
#matched %<>% mutate(thr = if_else(is.na(thr), avgtemp_max, thr))
#matched$reference[is.na(matched$reference) & !is.na(matched$thr)] <- "EcoCrop"

#clean table and save 
matched %<>% select(crop, crop_fr, matcher, code_cult2, thr, reference) %>% 
  arrange(thr) %>% 
  mutate(
    thr = if_else(is.na(thr), 30, thr), 
    reference = if_else(is.na(reference), "Imputed", reference)
  )
write_csv(matched, "data/temp_thresholds/maxtemp_by_crop.csv")
rm(pap, ecocrop)

#export nice table with composition 
comp <- read.csv(paste0(pathmatch, "crop_composition2021.csv")) %>% 
  filter(!is.na(pct_farmland)) %>% arrange(-pct_farmland) %>%
  select(cod_crop, pct_farmland, lab_crop2)

cpcd <- c(
   "VRC" = "Grapevine", 
   "BTN" = "Beetroot" , 
   "PTC" = "Potato" 
)

#make nice table 
nice <- full_join(matched, comp, by=c("code_cult2" = "cod_crop")) %>%
  arrange(-pct_farmland) %>% ungroup() %>% 
  filter(!is.na(pct_farmland) & !is.na(thr))
nice$aux1 <- NA
nice$aux1 <- cpcd[nice$matcher]
nice %<>% mutate(crop = ifelse(is.na(crop), aux1, crop),
                 crop = ifelse(is.na(crop), "Other (<1%)", crop)) %>% 
  group_by(crop, thr, reference) %>% 
  summarise(pct_farmland=sum(pct_farmland)) %>%
  ungroup() %>% arrange(-pct_farmland) 

last <- filter(nice, nice$crop == "Other (<1%)")
nice %<>% filter(crop != "Other (<1%)") %>% 
  bind_rows(last) %>% mutate(cumulative = cumsum(pct_farmland)) %>%
  select(crop, thr, pct_farmland, cumulative, reference)

kbl(nice, booktabs = T, 
    col.names = c("Crop", "Max. temp (ºC)", "Land share", "Cumulative", "Reference"),
    digits = 1, align = c('l', 'c', 'c', 'c', 'l'),
    format = "latex")




