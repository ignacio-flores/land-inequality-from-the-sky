

overlay_hist <- function(data1=output_farms, data2=output_foods, var_name) {
  
  # Create histogram of output_farms$avg_shock
  hist_farms <- ggplot(data1, aes_string(x = var_name)) +
    geom_histogram(fill = "blue", alpha = 0.5) +
    labs(title = "Histogram of Average Shock on Farms", x = "Average Shock", y = "Count")
  
  # Combine the histograms
  combined_hist <- hist_farms + 
    geom_histogram(data = data2, aes_string(x = var_name), fill = "green", alpha = 0.5) +
    labs(title = "Farms (blue), Foods (green)", x = var_name, y = "Count")
  
  # Display the combined histogram
  return(combined_hist)  
  
}

overlay_hist(var_name = "avg_shock")
