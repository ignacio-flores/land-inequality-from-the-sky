wp_map <- function(data, var, title = "", 
                     breaks = NULL, 
                     labels = NULL,
                     palette = "YlOrRd", 
                     file_name = "map_output.png", 
                     legend_title_size = 1.8, 
                     legend_text_size = 1.5, 
                     legend_position = c("left", "bottom"),
                     legend_bg_color = "white", 
                     legend_bg_alpha = 0.5, 
                     style= "cont",
                     frame = FALSE, 
                     add_borders = TRUE,  # Optional borders
                     border_lwd = 0.5, 
                     border_col = "black", 
                     add_legend = TRUE,  # Optional custom legend
                     legend_title = "Borders", 
                     legend_labels = "Canton",
                     as.count = F,
                     n = 5,
                     digits = 1) {
  
  # Generate the base map 
  map <- tm_shape(data) + 
    tm_fill(var, breaks = breaks, labels = labels, palette = palette, 
            title = title, style = style, as.count = as.count, n = n)
  
  # Optionally add borders
  if (add_borders) {
    map <- map + tm_borders(lwd = border_lwd, col = border_col)
    map <- map + tm_add_legend(title = legend_title,
                               type = "line", 
                               col = border_col,
                               lwd = border_lwd,
                               lty = "solid",
                               labels = legend_labels)
  }

  # Add layout settings
  map <- map + tm_layout(legend.title.size = legend_title_size,
                         legend.text.size = legend_text_size,
                         legend.position = legend_position,
                         legend.bg.color = legend_bg_color,
                         legend.bg.alpha = legend_bg_alpha,
                         frame = frame, 
                         legend.format = list(digits = digits))
  
  # Save the map
  tmap_save(map, file_name, dpi = 300)
  
  return(map)
}
