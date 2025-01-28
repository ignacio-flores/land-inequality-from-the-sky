##  Match parcels to farms and cantons
  #preliminary 
  gc()
  source("code/R/00a_paths.R")
  source("code/R/00b_libraries.R")
  source("code/R/functions/match_parcels.R")
  libraries(stdlibs, geolibs, "Recocrop")
  
  # Setup
  debug <- "no"
  no_cores <- availableCores() - 4
  plan(multisession, workers = no_cores)
  
  #list canton names 
  if (debug == "yes") {
    canton_list <- canton_list[c(1:2)]
    x <- canton_list[[12]]
    t <- 2015
  } else {
    #excl. Paris ("FRA.8.3")
    gadm4 <- vect(file.path(pathgadm, "gadm36_FRA_4.shp"))
    canton_list <- as.list(unique(gadm4$GID_4))
    canton_list <- canton_list[-c(1928:1947)]
  }
  
  # Loop through years and canton list 
  for (t in 2015:2021) {
    
    # Use the match_parcels  function
    tic(paste0("matching year ", t))
      matched <- future_map_dfr(
        canton_list, 
        ~match_parcels(.x)
      )
    toc()
    
    #save as table 
    write_csv(matched, 
      file.path(pathcadad, paste0("matched", t, ".csv")), append = FALSE)
  }
  