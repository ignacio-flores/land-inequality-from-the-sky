#preamble 
gc()
setwd("~/Dropbox/land_ineq_degradation/")
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)
library(ggplot2)
library(kableExtra)

year <- 2020 
df <- read_delim(paste0("data/FDS_DEVELOPPE_2/FDS_DEVELOPPE_", year,".csv")) %>%
  select(-c("NOM")) %>% filter(!grepl("\\.", N306_MOD))

unique(df$N306_MOD)
unique(df$N306_LIB)