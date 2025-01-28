gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs)

#load shapefiles 
pra <- read_sf(path_rashp) 
can <- read_sf(path_gadm4) %>% clean_names()

# Perform the intersection
pra <- st_transform(pra, crs = st_crs(can))
can$area_can <- st_area(can)
int <- st_intersection(can, pra)

# Calculate intersection area
int$area_intersection <- st_area(int)

# Calculate the percentage of each canton's area that overlaps with each PRA
int <- int %>%
  mutate(percent_overlap = 100 * as.numeric(area_intersection) / as.numeric(area_can))

# Summarize the overlap information for each canton
coverage_summary <- int %>%
  group_by(gid_4) %>%
  summarize(
    total_overlap = sum(percent_overlap),  # Total percentage overlap
    num_pra_overlap = n(),                 # Number of overlapping PRAs
    pras = paste(unique(ra_code), collapse = ", "),  # List of overlapping PRAs
    fully_covered = total_overlap >= 99.99            # Adjusted threshold for percentages
  )

# View the coverage summary
print(coverage_summary)

# View detailed overlap percentages between cantons and PRAs
detailed_overlap <- int %>%
  dplyr::select(gid_4, percent_overlap, ra_code) %>% 
  arrange(gid_4, desc(percent_overlap)) %>% 
  group_by(gid_4) %>% 
  mutate(csum = sum(percent_overlap), rank = row_number())

first <- detailed_overlap %>% filter(rank == 1)

# Define the function
get_statistics <- function(data, column_name) {
  # Check if the column exists in the data
  if (!column_name %in% names(data)) {
    stop("The specified column does not exist in the data.")
  }
  
  # Extract the column
  column_data <- data[[column_name]]
  
  # Ensure the column is numeric
  if (!is.numeric(column_data)) {
    stop("The specified column must be numeric.")
  }
  
  # Compute statistics
  stats <- list(
    Mean = mean(column_data, na.rm = TRUE),
    Standard_Deviation = sd(column_data, na.rm = TRUE),
    Deciles = quantile(column_data, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE),
    Centiles = quantile(column_data, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
  )
  
  return(stats)
}

# Example: Using the function with the detailed_overlap object
result <- get_statistics(first, "percent_overlap")

# View the result
result

#hist(first$percent_overlap)

tab <- dplyr::select(first, gid_4, percent_overlap) %>% st_drop_geometry()
can <- can %>% left_join(tab, by = "gid_4")

saver <- dplyr::select(first, gid_4, percent_overlap, ra_code) %>% 
  st_drop_geometry()
write_csv(saver, "data/PRA/matched_can_pra.csv")

# tmap_mode("plot")
# plot <- tm_shape(can) + tm_fill(col = "percent_overlap", style = "cont") + 
#   tm_shape(pra) + tm_borders(col = "white", lwd = 1) 
# plot 
