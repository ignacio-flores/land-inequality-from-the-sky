gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs)


#load shapefiles 
pra <- read_sf(path_rashp) 
can <- read_sf(path_canshp) %>% 
  mutate(area_can = st_area(geometry))


#ensure geometries match 
if (st_crs(pra) != st_crs(can)) {
  can <- st_transform(can, st_crs(pra))
}


# Compute spatial relationships
covered_by_pra <- st_covered_by(can, pra)
overlaps_pra <- st_overlaps(can, pra)


# Add counts to 'can' dataset
can <- can %>%
  mutate(
    num_pra_covering = lengths(covered_by_pra),
    num_pra_overlapping = lengths(overlaps_pra)
  )


#the pra perspective 
pra_covering_can <- st_covers(pra, can)
pra_overlapping_can <- st_overlaps(pra, can)
pra <- pra %>%
  mutate(num_can_covered = lengths(pra_covering_can),
         num_can_overlapping = lengths(pra_overlapping_can))


can_single_pra_covering <- can %>% filter(num_pra_covering == 1)
can_multiple_pra_overlapping <- can %>% filter(num_pra_overlapping > 1)


table(can$num_pra_covering)
table(can$num_pra_overlapping)
# 
# tmap_mode("view")
# plot <- tm_shape(pra) + tm_fill("num_can_covered", style = "cont") + 
#   tm_shape(pra) + tm_fill("num_can_overlapping", style = "cont") + 
#   tm_shape(can) + tm_borders(col = "green") +
#   tm_shape(pra) + tm_borders(col = "black")
# plot 




# Prepare a data frame of overlaps using 'tibble' instead of 'data.frame'
overlaps_pra_df <- tibble(
  can_index = rep(seq_len(nrow(can)), lengths(overlaps_pra)),
  pra_index = unlist(overlaps_pra)
)


# Check the column names in 'can'
print(names(can))


# Replace 'can_2013' with the correct column name if necessary
overlaps_pra_df <- overlaps_pra_df %>%
  mutate(
    can_2013 = can$can_2013[can_index],  # Change 'can_2013' if needed
    ra_code = pra$ra_code[pra_index],
    can_geom = can$geometry[can_index],
    pra_geom = pra$geometry[pra_index],
    area_can = as.numeric(can$area_can[can_index])
  )


# Verify that 'can_2013' has been added
print(names(overlaps_pra_df))


# Compute intersection geometries and areas with corrected syntax
overlaps_pra_df <- overlaps_pra_df %>%
  mutate(
    intersection_geom = map2(
      can_geom,
      pra_geom,
      ~ st_intersection(.x, .y)
    ),
    intersection_area = map_dbl(
      intersection_geom,
      ~ ifelse(st_is_empty(.x), 0, as.numeric(st_area(.x)))
    ),
    percent_area = (intersection_area / area_can) * 100
  )


# Rank the 'pra' polygons for each 'can' based on percentage area covered
overlaps_pra_df <- overlaps_pra_df %>%
  group_by(can_2013) %>%
  arrange(desc(percent_area)) %>%
  mutate(rank = row_number()) %>%
  ungroup()


# View the ranked overlaps
print(overlaps_pra_df)


# Compute summary statistics by 'pra'
summary_by_pra <- overlaps_pra_df %>%
  group_by(ra_code) %>%
  summarise(
    total_can_overlapping = n_distinct(can_2013),
    total_intersection_area = sum(intersection_area),
    average_percent_area = mean(percent_area),
    median_percent_area = median(percent_area),
    max_percent_area = max(percent_area)
  )


# View the summary statistics
print(summary_by_pra)


# Join the maximum percentage area data back to the 'can' data frame
can_with_percent <- can %>%
  left_join(
    overlaps_pra_df %>%
      group_by(can_2013) %>%
      summarise(max_percent_area = max(percent_area)),
    by = "can_2013"
  )


# Plot the 'can' polygons colored by the maximum percentage area covered
plot <- tm_shape(can_with_percent) +
  tm_polygons("max_percent_area", palette = "YlOrRd", title = "Max Percent Area Covered") +
  tm_shape(pra) +
  tm_borders()
plot
