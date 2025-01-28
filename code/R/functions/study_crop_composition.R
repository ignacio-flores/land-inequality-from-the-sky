#define function 
study_crop_composition <- function(t=2015, debug=FALSE, cropdrop=NULL, gid4=NULL) {
  gc()
  
  # Import parcel data 
  if (is.null(gid4)) {
    scols <- c("ID_PARCEL", "parcel_area", "CODE_GROUP", "CODE_CULTU")
  } else {
    scols <- c("GID_4", "NAME_4", "canton_area", 
               "ID_PARCEL", "parcel_area", "CODE_GROUP", "CODE_CULTU")
  }
  
  #debug mode? 
  if (debug == FALSE) {
    nmax=Inf
  } else {
    nmax=5000
  }
  
  #open csv 
  m_dfr <- read_delim(
    paste0(pathmatch, "matched", t,".csv"), 
    col_select = all_of(scols), n_max=nmax, 
    show_col_types = FALSE
  ) %>% 
    clean_names() 
  toc()  
  
  # delete crops if necessary 
  m_dfr %<>% filter(!(code_group %in% cropdrop))
  
  # keep only one/some canton(s) if necessary 
  if (!is.null(gid4)) {
    m_dfr %<>% filter(gid_4 %in% gid4)
  } else {
    m_dfr %<>% mutate(gid_4 = "FRA.1", name_4 = "France", canton_area = NA)
  }
  
  #ignore differences btw crops with year of plantation   
  m_dfr %<>% mutate(code_cult2 = gsub("[[:digit:]]", "_", code_cultu))
  
  #summarise areas at fine crop level 
  s_dfr <- group_by(m_dfr, gid_4, name_4, canton_area, code_group, code_cult2) %>%
    summarise(parcel_area=sum(parcel_area)) %>% ungroup() %>%
    arrange(gid_4, code_group, code_cult2)
  
  #define parcel areas as % of canton farmland 
  s_dfr %<>% group_by(gid_4, name_4) %>%
    mutate(
      parc_area_tot = sum(parcel_area),
      pct_farmland = parcel_area/parc_area_tot*100,
      year = t,
      can_tester = sum(pct_farmland)
    ) %>%
    arrange(gid_4, -pct_farmland)
  
  #format 
  attr(s_dfr$pct_farmland, "format") <- "%.2f"
  s_dfr %<>% ungroup() %>% 
    dplyr::select(gid_4, name_4, year, canton_area, code_group, code_cult2, pct_farmland) %>%
    rename(cod_crop = `code_cult2`)
  
  return(s_dfr)
}  