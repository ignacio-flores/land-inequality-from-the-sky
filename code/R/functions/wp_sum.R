# define a function that reads and cleans the relevant data
wp_sum <- function(var_name, dta, geo, stat = "none", sel.year = NULL) {
  
  idvars <- c("adm_can", "year", "month", "p")
  
  #select columns 
  if (var_name == "cadastral"){
    colstosel <- c("wshock", "dcount_o", "ccount_o", "gini_can", 
                   "dfct_r", "lfarms", "seminat", "fsize", "hshock_min",
                   "herfindahl", "dherf_o", "prairies", "crop28", "nfarms",
                   "lfarms", "cv_o")
    #open data 
    dta <- read_dta(dta) %>% 
      dplyr::select(all_of(c(idvars, colstosel))) %>%
      mutate(across(-adm_can, as.numeric)) %>% 
      mutate(seminat = seminat * 100,
             gini_can = gini_can * 100,
             crop28 = crop28 * 100,
             prairies = prairies * 100) %>%
      filter(p==1) %>% group_by(adm_can) 
    
  } else {
    colstosel <- c(var_name)
    dta <- read_dta(dta) %>% 
      dplyr::select(all_of(c(idvars, colstosel, var_name))) %>%
      mutate(across(-adm_can, as.numeric)) %>% group_by(adm_can)
  }
  
  #filter to year data if needed
  if (!is.null(sel.year)) {
    dta <- dta %>% filter(year == sel.year)
  }
  if (var_name == "the_shock_s_y") {
    dta %<>% filter(p == 45) 
    # summarise(the_shock_s_y=sum(the_shock_s_y))
  }
  if (stat == "add") {
    dta %<>% summarise(across(colstosel, sum))
  }
  if (stat == "mean")  {
    dta %<>% summarise(across(colstosel, mean))
  }

  #merge to shapefile 
  merged <- st_read(geo) %>% 
    right_join(dta, by = c("GID_4" = "adm_can")) 
  
  #export 
  return(merged)
}