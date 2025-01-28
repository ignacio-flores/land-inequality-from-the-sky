get_gini <- function(dfr, grp, yvar) {
  
  #make up a new df 
  vvv <- c(grp, yvar)
  df_temp <- dfr[, vvv]
  colnames(df_temp) <- c("tv_grp", "tv_yvar")
  
  #estimate gini 
  df_temp %<>% group_by(tv_grp) %>% 
    arrange(tv_grp, tv_yvar) %>% 
    mutate(
      n = 1,
      freq = 1/sum(n),
      bigF = cumsum(n)/sum(n),
      fy = tv_yvar * freq, 
      cumfy = cumsum(fy), 
      bigL = cumfy / max(cumfy), 
      d_eq = (bigF - bigL) * freq, 
      gini = sum(d_eq) * 2, 
    ) %>% 
    select(tv_grp, gini) %>%
    summarise(gini=median(gini))
  
  #redefine names of columns 
  colnames(df_temp) <- c(grp, "gini")
  
  return(df_temp)
}