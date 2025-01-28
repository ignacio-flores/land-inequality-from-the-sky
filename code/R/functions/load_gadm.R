load_gadm <- function(n=4, mode="normal") {
  
  #load and repair 
  all_gadm <<- st_read(paste0(pathgadm, "gadm36_FRA_", n, ".shp"), quiet=T) %>% 
    st_make_valid() 
  
  #reduced size for debugging 
  if (mode == "debug") {
    all_gadm <- all_gadm[1:3,]
  }
  
  #list cantons 
  gadm_list <<- as.list(unique(all_gadm$GID_4))
  
}