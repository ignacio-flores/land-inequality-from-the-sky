##  Calculate mean differences in total parcel and farm areas
 # Setup
 library(terra) 
 library(readr)
 library(tidyverse)
 library(magrittr)

 sfiles <- "georeferenced_data/cadastre/FR"
 dfiles <- "data/FR/cadastre"
 
 # Delete the outcome file first because of appending loop
 if (file.exists(file.path(dfiles, "mean_area_diff.csv"))) {
   unlink(file.path(dfiles, "mean_area_diff.csv"))
 }

 # Loop over years to calculate the mean differences in areas
   for (t in 2015:2021) {
  
   # Read in the matched csv
   matched_df <- read_csv(file.path(dfiles, paste0("matched", t, ".csv")))
  
   # Return mean area difference between total parcels and farm
    matched_df %<>%
      group_by(ID_ILOT) %>%
      summarize(diff_areas = (((abs(sum(parcel_area) - farm_area)) / farm_area)*100),
              year = t, dept = GID_2) %>%
      group_by(dept) %>%
      summarize(mean_diff_areas = mean(diff_areas), year = t)
  
    write_csv(matched_df, file.path(dfiles, "mean_area_diff.csv"), append = T)
   }
 
 # Fix the column names resulting from the append
 diff_areas <- read_csv(file.path(dfiles, "mean_area_diff.csv"), col_names = F)
 diff_areas %<>% rename(dept = X1, mean_diff_areas = X2, year = X3)
 
 # Flag problematic departments where the difference in areas is more than 1%

 # Create a flag variable
 diff_areas$problem <- ifelse(diff_areas$mean_diff_areas < 1, 0, 1)

 # Export the csv
 write_csv(diff_areas, file.path(dfiles, "mean_area_diff.csv"), append = F)
 attach(diff_areas)

 # Count the number of departments with/without problem and save table
 table <- table(problem,year) %>% data.frame()
 write_csv(table, file.path(dfiles, "problem_count_byyear.csv"), append = F)
 