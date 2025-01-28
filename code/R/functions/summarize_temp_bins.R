# Function to summarize temperature into bins
summarize_temp_bins <- function(data, bin_width = 5, start = 0, end = 50) {
  
  # Define bins
  bins <- seq(start, end, by = bin_width)
  labels <- paste0("t", bins[-length(bins)], "_", bins[-1])
  
  # Summarize temperature bins
  summary <- data %>% filter(!is.na(int)) %>% 
  group_by(canton_id, int) %>%
    mutate(temp_bin = cut(TSUP_H_Q, breaks = bins, labels = labels, include.lowest = TRUE)) %>%
    count(temp_bin) %>%
    pivot_wider(names_from = temp_bin, values_from = n, values_fill = list(n = 0)) %>%
    arrange(canton_id, int)
  
    summary2 <- data %>% 
      mutate(hshock_avg = if_else(TSUP_H_Q > avg_shock, 1, NA),
             hshock_min = if_else(TSUP_H_Q > min_shock, 1, NA),
             hshock_max = if_else(TSUP_H_Q > max_shock, 1, NA)) %>%
      group_by(canton_id, int) %>% 
      summarise(
        hshock_avg = sum(hshock_avg, na.rm = T),
        hshock_min = sum(hshock_min, na.rm = T), 
        hshock_max = sum(hshock_max, na.rm = T)
      )
    summary %<>% left_join(summary2, by = c("canton_id", "int"))
  
  return(summary)
}