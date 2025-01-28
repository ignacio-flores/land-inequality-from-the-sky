#requires gini 

summarise_cadastral_canton <- function(t, debug=FALSE, cropdrop=NULL) {
  
  gc()
  tic(paste0(t, ": finished"))
  
    # load gini function 
    source("code/R/functions/get_gini.R")
    
    # Import parcel data 
    scols <- c(
      "ID_ILOT", "farm_area", "ID_PARCEL", "parcel_area", "parcel_perim", 
      "CODE_GROUP", "CODE_CULTU", "GID_4", "NAME_4", "canton_area"
    )
    
    #debug mode? 
    if (debug == FALSE) {
      nmax=Inf
    }
    if (debug == TRUE) {
      nmax=50000
    }
    
    #open csv 
    tic(paste0(t, ": opening data"))
      m_dfr <- readr::read_delim(
          paste0(pathmatch, "matched", t,".csv"), 
          col_select = all_of(scols), n_max=nmax, show_col_types = FALSE) %>% 
          arrange(GID_4, ID_ILOT, ID_PARCEL) %>% 
          clean_names() 
    toc()  
    
    # delete crops if necessary 
    if (!is.na(cropdrop)) {
      m_dfr %<>% filter(!(code_group %in% cropdrop)) #%>%
        #filter(!(code_group == 28 & code_cultu %in% c("SNE", "SBO", "ROS", "MRS")))
    } 
    
    #ignore differences due to year of plantation   
    m_dfr %<>% mutate(code_cult2 = gsub("[[:digit:]]", "_", code_cultu))
   
    tic(paste0(t, ": summarising info"))
    
      #I. DETAILED PARSING   
    
      #summarise areas at fine crop level within farms
      s_dfr <- group_by(m_dfr, gid_4, name_4, canton_area, 
                        id_ilot, code_group, code_cult2) %>%
        summarise(
          farm_area    = median(farm_area), 
          parcel_area  = sum(parcel_area), 
          parcel_perim = sum(parcel_perim)
        )
      
      #compute farm area from parcels 
      s_dfr %<>% group_by(gid_4, name_4, id_ilot) %>% 
        mutate(
         farm_area_est = sum(parcel_area), 
         farm_perim_est = sum(parcel_perim), 
         rank = row_number(),
         small_farm     = if_else(farm_area_est < 2 & rank == 1, 1, 0),
         small_farm_ha  = if_else(farm_area_est < 2 & rank == 1, farm_area_est, 0),
         medium_farm    = if_else(farm_area_est >= 2 & farm_area_est < 50 & rank == 1, 1, 0),
         medium_farm_ha = if_else(farm_area_est >= 2 & farm_area_est < 50 & rank == 1, farm_area_est, 0),
         large_farm     = if_else(farm_area_est >= 50  & rank == 1, 1, 0),
         large_farm_ha  = if_else(farm_area_est >= 50  & rank == 1, farm_area_est, 0),
         vlarge_farm    = if_else(farm_area_est >= 100  & rank == 1, 1, 0),
         vlarge_farm_ha = if_else(farm_area_est >= 100  & rank == 1, farm_area_est, 0)
        )
      
      #define areas as % of canton farmland
      s_dfr %<>% group_by(gid_4, name_4) %>%
        mutate(
          farm_area_tot   = sum(parcel_area),
          small_farm      = sum(small_farm),
          small_farm_pt   = sum(small_farm_ha) / farm_area_tot * 100,
          medium_farm     = sum(medium_farm), 
          medium_farm_pt  = sum(medium_farm_ha) / farm_area_tot * 100, 
          large_farm      = sum(large_farm),
          large_farm_pt   = sum(large_farm_ha) / farm_area_tot * 100,
          vlarge_farm     = sum(vlarge_farm),
          vlarge_farm_pt  = sum(vlarge_farm_ha / farm_area_tot * 100),
          parcel_area     = parcel_area / farm_area_tot,
          year            = t, 
          dcount          = n_distinct(code_cult2), 
          nfarms          = n_distinct(id_ilot)
        ) 
      
      #summarize at crop level 
      s_dfr <- s_dfr %>% arrange(gid_4, code_group, code_cult2) %>% 
        group_by(gid_4, name_4, canton_area, code_group, code_cult2) %>% 
        summarise(
          parcel_area  = sum(parcel_area), 
          parcel_perim = sum(parcel_perim),
          dcount = mean(dcount), 
          year = mean(year),
          nfarms = mean(nfarms),
          small_farm = mean(small_farm), 
          medium_farm = mean(medium_farm), 
          large_farm = mean(large_farm),
          vlarge_farm = mean(vlarge_farm),
          small_farm_pt = mean(small_farm_pt), 
          medium_farm_pt = mean(medium_farm_pt), 
          large_farm_pt = mean(large_farm_pt),
          vlarge_farm_pt = mean(vlarge_farm_pt),
        )
      
      #join with threshold temperatures 
      s_dfr <- left_join(s_dfr, thr) %>%
        mutate(thr = if_else(is.na(thr), 30, thr))  %>% 
        group_by(gid_4, name_4) %>%
        mutate(avg_shock = weighted.mean(thr, parcel_area),
               min_shock = min(thr),
               max_shock = max(thr))
    
      # pivot wide
      s_dfr %<>% group_by(gid_4, name_4, canton_area) %>%
        pivot_wider(
          id_cols = c(gid_4, name_4, canton_area, year, dcount, nfarms,
                      avg_shock, min_shock, max_shock,
                      small_farm, medium_farm, large_farm, vlarge_farm,
                      small_farm_pt, medium_farm_pt, large_farm_pt, vlarge_farm_pt),
          names_from = code_cult2, 
          values_from = parcel_area,
          names_prefix = "dcro" , 
          values_fill = 0
        ) %>% ungroup()
      
      #list dcro variables 
      dcropvars <- grep("dcro", colnames(s_dfr), value=TRUE)
        
      ws <- select(
        s_dfr, gid_4, avg_shock, min_shock, max_shock, nfarms, 
        small_farm, medium_farm, large_farm, vlarge_farm,
        small_farm_pt, medium_farm_pt, large_farm_pt, vlarge_farm_pt,
        all_of(dcropvars)
      )
      
      #define simple square function 
      sqf <- function(x){x = x^2}
      
      # estimate HHI 
      df_hhd <- s_dfr %>% select(gid_4, all_of(dcropvars)) 
      dctr <- s_dfr %>% select(gid_4, dcount)
      df_hhd %<>% 
        mutate(dhh_check = rowSums(.[dcropvars], na.rm = TRUE)) %>% 
        mutate_if(is.numeric, sqf) %>%
        mutate(dherf = rowSums(.[dcropvars], na.rm = TRUE)) %>% select(-all_of(dcropvars)) %>%
        full_join(dctr, by= "gid_4")
      rm(s_dfr)
        
      # II. BROAD PARSING  
      
      # summarise areas at broader crop level 
      m_dfr %<>% 
        group_by(gid_4, name_4, canton_area, id_ilot, code_group) %>% 
        summarise(
          farm_area    = median(farm_area), 
          parcel_area  = sum(parcel_area), 
          parcel_perim=sum(parcel_perim)
        )
      
      #compute estimated farm area (from parcels)
      m_dfr %<>% 
        group_by(gid_4, name_4, id_ilot) %>% 
        mutate(
          farm_area_est  = sum(parcel_area), 
          farm_perim_est = sum(parcel_perim)
        )
      
      #define parcel areas as % of canton farmland
      #and coeficient of variation 
      m_dfr %<>% group_by(gid_4, name_4) %>%
        mutate(
          farm_area_tot = sum(parcel_area),
          parcel_area = parcel_area/farm_area_tot,
          year = t, 
          ccount = n_distinct(code_group),
          cv =  sd(farm_area_est) / mean(farm_area_est), 
          sd_logs = sqrt(mean((log(mean(farm_area_est)) - log(farm_area_est))^2))
        )  
      
      # summarise to farm-level and pivot wide 
      m_dfr %<>% group_by(gid_4, name_4, canton_area, id_ilot) %>% 
        pivot_wider(
          id_cols = c(gid_4, name_4, canton_area, year, id_ilot, ccount,
          farm_area_est, cv, sd_logs, farm_perim_est, farm_area_tot), 
          names_from = code_group, values_from = parcel_area, 
          names_prefix = "crop", values_fill = 0
        )
    toc()
    
    tic(paste0(t, ": computing indicators"))
      # get inequality levels separately 
      ineq_can <- get_gini(
        dfr = m_dfr,
        grp = "gid_4",
        yvar = "farm_area_est"
      )
      
      # summarise crops at canton level
      m_dfr %<>% group_by(gid_4, name_4, canton_area, ccount, year, cv, sd_logs) %>% 
        summarise_all(sum) %>% 
        subset(select = -c(farm_area_tot, id_ilot)) %>%
        ungroup()
      
      #define simple square function 
      sqf <- function(x){x = x^2}
      
      # estimate HHI 
      cropvars <- grep("crop", colnames(m_dfr), value=TRUE)
      df_hhi <- m_dfr %>% select(gid_4, all_of(cropvars)) 
      df_hhi %<>% 
        mutate(hhi_check = rowSums(.[cropvars], na.rm = TRUE)) %>% 
        mutate_if(is.numeric, sqf) %>%
        mutate(herfindhal = rowSums(.[cropvars], na.rm = TRUE)) %>%
        select(-all_of(cropvars)) 
    toc() 
    
    #join all  
    m_dfr <- full_join(m_dfr, ineq_can, by = "gid_4") %>%
             full_join(df_hhi, by = "gid_4") %>% 
             full_join(df_hhd, by = "gid_4") %>% 
             full_join(ws, by = "gid_4")
    
  toc()  
  
  return(m_dfr)
}
