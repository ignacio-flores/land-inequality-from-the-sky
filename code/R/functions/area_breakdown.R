area_breakdown <- function(shp, times = 2, sufix="XX") {
  
  shp <- get(shp)
  
  #intersect with grid 
  ar_gr <- st_make_grid(shp, n = times, square = F)
  ar_cr <- st_intersection(shp, ar_gr)
  
  #dadd id to chunk 
  ar_cr <- mutate(ar_cr, 
    n = row_number(), 
    chunk_id = paste0("chunk", "-" , n) 
  ) 
  
  #clean memory 
  rm(ar_gr, shp)
  gc()
  
  return(ar_cr)
}