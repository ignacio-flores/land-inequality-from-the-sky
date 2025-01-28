# Define function for diversification ratio 
compute_diversification_ratio <- function(data, n, piv) {
  
  print(paste0(round(n/nrow(cadastre) * 100, digits = 3), 
    "% completed ...")
  )
  
  # Check if n is within the row range of the data frame
  if(n > 0 & n <= nrow(data)) {
    
    # separate one row and reshape 
    subset <- data[n, , drop = FALSE] %>% 
      select(type, gid_4, name_4, year, all_of(cadcrop_cod)) %>% 
      pivot_longer(
        cols = all_of(piv), 
        names_to = "code_culture", 
        names_prefix = "dcro", 
        values_to = "w", 
        values_drop_na = F) %>%
      mutate(w = as.numeric(w)) %>%
      filter(!is.na(w) & w != 0) %>% 
      left_join(codc_all, by = "code_culture") %>%
      left_join(match_cad_agr, by = "code_culture") 
    
    # how much land was matched? 
    sum_matched <- subset %>% 
      mutate(matched = if_else(!is.na(n306_mod), 1, 0)) %>%
      select(gid_4, matched, w) %>%
      group_by(gid_4, matched) %>%
      summarise(sum_w = sum(w, na.rm = TRUE), .groups = "keep") %>%
      pivot_wider(
        #id_cols = "matched", 
        values_from = sum_w,
        names_from = "matched",
        names_prefix = "div_match_")
    
    if (length(sum_matched) == 2 & colnames(sum_matched[, 2]) == "div_match_1") {
      sum_matched %<>% mutate(div_match_0 = 1 - div_match_1)
    }
    if (length(sum_matched) == 2 & colnames(sum_matched[, 2]) == "div_match_0") {
      sum_matched %<>% mutate(div_match_1 = 1 - div_match_0)
    }
    
    # consolidate by agreste code 
    msub <- subset %>% filter(!is.na(n306_mod)) %>% arrange(n306_mod) %>%
      group_by(type, gid_4, name_4, year, n306_mod, n306_lib) %>% 
      summarise(w = sum(w), .groups = "keep") 
      
    if (nrow(msub) != 0 & nrow(msub) != 1) {
      
      # normalize weights 
      msub <- msub %>% mutate(w = w * 1/sum(msub$w))
      
      # list matched crops 
      mcrops <- unique(na.omit(subset$n306_mod)) %>% sort()
      sub_cov_matrix <- cov_matrix[mcrops, mcrops]
      
      # separate weight column 
      w <- msub$w
      
      if (w[1] != 1) {
        #compute portfolio variance 
        portfolio_var <- sum(w * (sapply(1:length(mcrops), function(i) {
          sapply(1:length(mcrops), function(j) {
            w[i] * w[j] * sub_cov_matrix[i,j]
          })
        })))
        
        #compute weighted average variance 
        w_avg_var <- sum(sqrt(diag(sub_cov_matrix)) * w)
        
        #diversification ratio 
        diversification_r = w_avg_var / sqrt(portfolio_var) 
        
        #organise results 
        expt <- cbind(
          unique(msub$type),
          unique(msub$gid_4),
          as.numeric(unique(msub$year)), 
          sum_matched[, c(2, 3)],
          w_avg_var,
          portfolio_var,
          diversification_r
        )
        colnames(expt) <- c(
          "type", 
          "gid_4", 
          "year", 
          "div_match_0", 
          "div_match_1", 
          "wavg_var", 
          "pfolio_var", 
          "dfct_r"
        )
      }
    #if no crops or a single crop were matched 
    } else {
      
      #if none were matched 
      if (nrow(msub) != 0) {
        #organise results 
        expt <- cbind(
          unique(subset$type),
          unique(subset$gid_4),
          unique(subset$year), 
          c(1),
          c(0),
          NA,
          NA,
          NA
        ) %>% as.data.frame() 
        colnames(expt) <- c(
          "type", 
          "gid_4", 
          "year", 
          "div_match_0", 
          "div_match_1", 
          "wavg_var", 
          "pfolio_var", 
          "dfct_r"
        )
        expt %<>% mutate(year = as.numeric(year),
                         div_match_0 = as.numeric(div_match_0),
                         div_match_1 = as.numeric(div_match_1),
                         wavg_var = as.numeric(wavg_var), 
                         pfolio_var = as.numeric(pfolio_var),
                         dfct_r = as.numeric(dfct_r))
      }
      
      #if only one was matched 
      if (nrow(msub) != 1) {
        portfolio_var <- NA
        w_avg_var <- NA 
        diversification_r = 1
        
        #organise results 
        expt <- cbind(
          unique(subset$type),
          unique(subset$gid_4),
          unique(subset$year), 
          c(0),
          c(1),
          NA,
          NA,
          1
        ) %>% as.data.frame()
        colnames(expt) <- c(
          "type", 
          "gid_4", 
          "year", 
          "div_match_0", 
          "div_match_1", 
          "wavg_var", 
          "pfolio_var", 
          "dfct_r"
        )
        expt %<>% mutate(year = as.numeric(year),
                         div_match_0 = as.numeric(div_match_0),
                         div_match_1 = as.numeric(div_match_1),
                         wavg_var = as.numeric(wavg_var), 
                         pfolio_var = as.numeric(pfolio_var),
                         dfct_r = as.numeric(dfct_r))
      }
    }
    
    
    return(expt)
    
    #warning  
  } else {
    # Return a message if n is out of range
    warning("n is out of the range of rows in the data frame")
  }
}
