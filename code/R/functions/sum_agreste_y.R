
sum_agreste_y <- function(t) {
  
  tic(paste0("cleaning and shaping: ", flist[[t]]))
    
    #open raw data
    tdir <- flist[[t]]
    ydat <- read_delim(tdir, show_col_types = FALSE) %>% clean_names()
    
    #select observations 
    ydat %<>% filter((frdom == "..." & region == "...") | (dep != "..."))
    ydat$detail[ydat$dep=="..."] <- "national"
    ydat$detail[ydat$dep!="..."] <- "departement"
    
    #clean data 
    ydat %<>% 
      mutate(level = str_count(n306_mod, "\\."), 
             n027_lib = gsub(" .*", "", n027_lib),   # everything after the first white space 
             n027_lib = tolower(n027_lib)  # converts string to lowercase
      ) %>% 
      select(annref, detail, dep, n306_lib, n306_mod, level, n027_lib, n027_mod, valeur) 
    
    #reshape  
    ydat %<>% 
      pivot_wider(id_cols = c("annref", "detail", "dep", "level", "n306_mod", "n306_lib"), 
                  names_from = n027_lib, values_from = valeur) %>%
      arrange(detail, level, dep, n306_mod) %>%
      rename(year = `annref`) %>% 
      mutate(pvity=production/superficie)
    
  toc()
  
  return(ydat)
}