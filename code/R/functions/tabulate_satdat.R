tabulate_satdat <- function(df, dest) {
  
  cols_to_select <- c("GID_4", "identifier", "date", "canton", "farms", "foods") 
  cols_to_rename <- list(
    gpp_canton = "canton",
    gpp_farms = "farms",
    gpp_foods = "foods", 
    id = "identifier" 
  )
  cols_to_rename <- cols_to_rename[intersect(names(cols_to_rename), names(df))]
  
  #make pretty 
  df %<>% arrange(GID_4, date) %>% 
    dplyr::select(intersect(names(df), cols_to_select)) %>%
    rename(!!!cols_to_rename)
  #save file 
  if (debug != "yes") {
    write.csv(df, dest)  
  }
}
