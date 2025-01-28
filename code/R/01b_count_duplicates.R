##  Count no of duplicates in matched data and compare with raw data

 # Setup
 library(terra) 
 library(readr)
 library(tidyverse)
 library(magrittr)

 sfiles <- "georeferenced_data/cadastre/FR"
 dfiles <- "data/FR/cadastre"

 # Delete the outcome file first because of appending loop
   if (file.exists(file.path(dfiles, "dup_count.csv"))) {
   unlink(file.path(dfiles, "dup_count.csv"))
 }

 # Loop over years to count no of parcel id duplicates
   for (t in 2015:2021) {
  
   # Read in matched and raw data
   match <- read_csv(file.path(dfiles, paste0("matched", t, ".csv"))) %>% 
     as.data.frame()
  
   if (t < 2019) {
     raw <- vect(file.path(sfiles, t, "PARCELLES_GRAPHIQUES.shp")) %>% 
       as.data.frame()
   }
   
   if (t >= 2019) {
     raw <- vect(file.path(sfiles, t, "PARCELLES_GRAPHIQUES.gpkg")) %>% 
       as.data.frame()
   }
   
   # Calculate the number of duplicates
   dmatch = sum(duplicated(match$ID_PARCEL))
   draw = sum(duplicated(raw$ID_PARCEL))
   
   obsmatch = nrow(match)
   obsraw = nrow(raw)
   rm(raw)
   
   # Calculate the number of duplicates that are repeated obs
   # (as opposed to different a match)
   
   dups_df <- match %>%
     group_by(ID_PARCEL) %>%
     mutate(flag = +(n() > 1)) %>%
     ungroup() %>%
     filter(flag == 1) %>% 
     select(c(15,18)) 
   rm(match)
   
   reps = sum(duplicated(dups_df[c(1,2)]))
   
   # Create a dataframe
   dups <- data.frame(dups_parcel = c(dmatch, draw),
                      reps = c(reps, "."),
                      no_obs = c(obsmatch, obsraw),
                      df = c("matched", "raw"),
                      year = t)
   
   # Export as csv
   write_csv(dups, file.path(dfiles, "dup_count.csv"), append = T)
   } 

 # Fix the column names resulting from the append
   dups <- read_csv(file.path(dfiles, "dup_count.csv"), col_names = F)
   dups %<>% rename(dups_parcel = X1, reps = X2, no_obs = X3, df = X4, year = X5)
   write_csv(dups, file.path(dfiles, "dup_count.csv"), append = F)

 # # Plot the duplicates
   # parcel <- vect(file.path(sfiles, 2015, "PARCELLES_GRAPHIQUES.shp"))
   # new_dups <- merge(parcel, new_dups)
   # png("figures/intuitive/parcel_to_ilot_dups15.png", width=350, height=233, units='mm', res = 200)
   # plot(new_dups)
   # dev.off()