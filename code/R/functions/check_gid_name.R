check_gid_name <- function(df, gv = "gid_4", nv = "name_4", dropNA = FALSE) {
  
  library(foreign)
  scols <- c(gv, nv)
  gv <- sym(gv)
  nv <- sym(nv)
  
  #open original reference 
  gadm <- read.dbf(file.path(pathgadm, "gadm36_FRA_4.dbf")) %>% clean_names() %>%
    dplyr::select(gid_4, name_4) %>% distinct() %>% 
    group_by(gid_4) %>% mutate(n = row_number(), nmax = max(n))
  
  #inform original 
  print(paste0("Original data has: ", 
    length(unique(gadm$gid_4)), 
    " unique combinations")
  )
  
  #count unique in compared 
  compare <- df %>% select(all_of(scols)) %>% distinct() %>% 
    arrange(!!gv, !!nv) 
  if (dropNA == T) {
    compare <- compare %>% filter(!is.na(!!nv))
  }
  compare <- compare %>% group_by(!!gv) %>% 
    mutate(n = row_number(), nmax = max(n))
  print(paste0("Compared data has: ", 
     nrow(unique(compare[, gv])), 
     " unique combinations")
  )
  
  # count multiple matches 
  prob <- compare %>% filter(nmax != 1)
  print(paste0("Multiple matching in compared: ", nrow(unique(prob[, gv]))))
  
  return(compare)
}

  
