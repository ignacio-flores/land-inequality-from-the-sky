#preamble 
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs)

#call functions and parameters 
source("code/R/functions/summarise_cadastral_canton.R")
source("code/R/functions/get_gini.R")
source("code/R/pushoverr/keys_IF.R")
plan(multicore, workers = 6)

#############
ver = "_wi28"
#############

#bring crop-specific tempreature thresholds 
thr <- "data/temp_thresholds/maxtemp_by_crop.csv"
thr %<>% read_delim(show_col_types = FALSE) 

#clean for later 
thr <- thr %>% select(code_cult2, thr) %>% arrange(thr) %>%
  distinct()

#run program with all crops
options(dplyr.summarise.inform = FALSE)
tic("Preparing farm info")
  output_farms <- future_map_dfr(
     2015:2021,
     ~summarise_cadastral_canton(
       t = .x,
       debug = F,
       cropdrop = NA
     )
   )
   output_farms %<>% mutate(type = "farms")
   write.csv(output_farms, "data/FR/cadastre/temp/farm_yearly.csv")
toc()

#now only with food crops 
tic("Preparing food info")
  if (ver == "_no28") {notfood <- c(11, 17, 18, 19, 28)}
  if (ver == "_pa28") {notfood <- c(11, 17, 18, 19)} #make sure line 37 in summarize_cadastral_canton() is active (operated manually)
  if (ver == "_wi28") {notfood <- c(11, 17, 18, 19)} #make sure line 37 in summarize_cadastral_canton() is inactive (operated manually)
  output_foods <- future_map_dfr(
    2015:2021, 
    ~summarise_cadastral_canton(
      t=.x,  
      debug =  F,
      cropdrop = notfood
    )
  )
  output_foods %<>% mutate(type = "foods")
  write.csv(output_foods, paste0("data/FR/cadastre/temp/food_yearly", ver, ".csv"))
toc()

#save together 
tic("merging everything")
   alltogether <- bind_rows(output_farms, output_foods)
   alltogether %<>% arrange(gid_4, year, type)
   write.csv(alltogether, paste0("data/FR/cadastre/temp/all_yearly", ver, ".csv"))
toc()  

