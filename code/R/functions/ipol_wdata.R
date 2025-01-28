#define interpolation function 
ipol_wdata <- function(date, weather.data, wvar, target, mode) {
  
  print(paste0("interpolating (", mode, ") ", date, " ..."))
  
  #subset data 
  subset_data <- weather.data %>% 
    filter(dmy == date & !is.na(get(wvar))) %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude), 
           altitude = as.numeric(altitude))

  #Kriging 
  if (mode == "kri") {
    
    #create variogram and fitting 
    emp_var <- var(subset_data$aftmean)
    variogram_mod <- variogram(get(wvar) ~ 1, data = subset_data)
    variogram_fit <- fit.variogram(
      variogram_mod, model = vgm(psill = emp_var, "Exp", 300)
    )
    #plot(variogram_mod, model = variogram_fit)
    
    #define model 
    kriging_model <- gstat(
      formula = aftmean ~ altitude, 
      data = subset_data,
      model = variogram_fit
    )
    
    #predict 
    kir_result <- predict(kriging_model, target)
    
    #organize results 
    result_df <- data.frame(
      canton_id = grid$GID_4,
      date = date, 
      ipol = kir_result$var1.pred, 
      ipol_var = kir_result$var1.var 
    )
    colnames(result_df) <- c("canton_id", "date", paste0("ipol_", wvar), paste0("var_", wvar))
  }
    
  #inverse distance weighting
  if (mode == "idw") {
    idw_result <- idw(formula = get(wvar) ~ 1, 
                      locations = subset_data, 
                      newdata = target)
    
    # Create a dataframe with the results
    result_df <- data.frame(
      canton_id = grid$GID_4,  
      date = date,
      ipol = idw_result$var1.pred
    )
    colnames(result_df) <- c("canton_id", "date", paste0("ipol_", wvar))
  }
  
  #report activity 
  return(result_df)
}
