#preamble 
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)
library(ggplot2)
library(kableExtra)

comp <- read_delim(
  paste0(pathmatch, "crop_composition2021.csv"), 
  show_col_types = FALSE
  ) %>% 
  filter(!is.na(pct_farmland)) 

#store ecocrop in memory 
ecocrop <- "data/EcoCrop/Crops OAMA.xlsx" %>% readxl::read_xlsx() %>% 
  clean_names() %>% dplyr::select(code_culture, referenced_plant_fao, 
  contains("optimal"), contains("absolute")) %>% 
  mutate_at(c("absolute_min", "optimal_min", "optimal_max", "absolute_max"), 
  as.numeric, na.rm=TRUE) %>% mutate(bound_min = 0, bound_max = 60) %>%
  rename(cod_crop = `code_culture`)

#merge datasets 
united <- left_join(comp, ecocrop, by="cod_crop") %>% 
  arrange(-pct_farmland) 
united$referenced_plant_fao[is.na(united$optimal_max)] <- "Unmatched"

#clean more 
united %<>% mutate(
    cumulative = cumsum(pct_farmland),
    f_clab = referenced_plant_fao, pct_farmland
    )

#isolate main variables (all 124 matched crops, out of 230)
united %<>% # dplyr::select(f_clab, pct_farmland, optimal_max, absolute_max) %>% 
  group_by(f_clab) %>% mutate(pct_farmland = sum(pct_farmland)) %>% ungroup() %>% 
  distinct(.keep_all = TRUE) 

#isolate unmatched 
unmatched <- filter(united, f_clab == "Unmatched")
  
#Isolate first 20 crops
united %<>% filter(f_clab != "Unmatched") %>% arrange(-pct_farmland) %>%
  mutate(n = rank(-pct_farmland))
united$f_clab[united$n>13] <- "Other crops (smaller %)" 
united %<>% group_by(f_clab) %>% mutate(pct_farmland = sum(pct_farmland),
  optimal_max = mean(optimal_max), absolute_max = mean(absolute_max)) %>% 
  ungroup() %>% select(-n) %>% distinct(.keep_all = TRUE) %>% 
  rbind(unmatched) %>% mutate(cumulative = cumsum(pct_farmland)) %>% 
  select(f_clab, pct_farmland, cumulative, optimal_max, absolute_max)
head(united)

print("start--------------------------")
kbl(united, 
    format = "latex", 
    digits = 1, 
    align = c("l", "c", "c",  "c", "c"), 
    booktabs = T, 
    col.names = c("Crop type", "% of total", "Cumulative" , "Optimal", "Absolute")) %>%
  add_header_above(c(" " = 1, "Surface" = 2, "Max. temperatures (C)" = 2))
print("end--------------------------")

# Plot
# ggplot(united, aes(x=f_clab, xend=f_clab)) +
#   geom_segment(aes(y=bound_min, yend=bound_max), color="#CD2626") +
#   geom_segment(aes(y=absolute_min, yend=absolute_max), color="#FFD700") +
#   geom_segment(aes(y=optimal_min, yend=optimal_max), color="chartreuse3") +
#   coord_flip() + ylab("temperature (ºC)") + xlab("Crop") + 
#   geom_hline(yintercept = 30, linetype="dashed") +
#   theme_minimal()
  