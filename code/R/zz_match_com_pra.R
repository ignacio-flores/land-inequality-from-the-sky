gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs)

#list dom/tom depts 
dep_domtom <- c("971", "972", "973", "974", "976")

#load com-canton match from 2013 
com_can_2013 <- st_read(path_com2013) %>% 
  clean_names() %>%
  mutate(can = paste0(dep, ct),
         codcom = paste0(dep, com)) %>% 
  dplyr::select(codcom, can) %>% 
  rename(com_2013 = codcom, can_2013 = can) %>%
  arrange(com_2013) 

#load COM changes between 2013-2020
com_pass <- read.xlsx(path_pas, startRow = 6) %>% 
  clean_names() %>% 
  filter(nivgeo != "ARM") %>%
  dplyr::select(c(paste0("codgeo_", 2013:2020), paste0("libgeo_", 2013:2020))) %>% 
  distinct() %>% 
  left_join(com_can_2013, by = c("codgeo_2013" = "com_2013"))
rm(com_can_2013)

#check if 
# test <- com_pass %>% 
#   dplyr::select(codgeo_2020, libgeo_2020, can_2013) %>%
#   distinct() %>% 
#   arrange(codgeo_2020) %>%
#   group_by(codgeo_2020) %>% 
#   mutate(r = n())
# table(test$r)

# Load and clean PRA-COM match for 2017
pra_com_2017 <- readxl::read_xls(path_pra, sheet = "COM", skip = 5) %>%
  clean_names() %>%
  arrange(codgeo) %>%
  dplyr::select(codgeo, libgeo, epci, pra_code, ra_code, pra_lib) %>%
  rename(codgeo_pra_2017 = codgeo)

#load detailed info on COM 2020 (drop Outre Mer)
com_2020 <- read_delim(path_com, show_col_types = F) %>% 
  filter(typecom == "COM" & !(dep %in% dep_domtom)) %>%
  dplyr::select(dep, com, nccenr, libelle) %>% 
  arrange(com) 
  
#join 2020 data (as in georeferenced)
com_pass_2020 <- left_join(com_2020, com_pass, by = c("com" = "codgeo_2020"), keep = T) 
rm(com_2020, com_pass)

#join pra-com match (tag splits and merges)
all_pra_com <- full_join(pra_com_2017, com_pass_2020, by = c("codgeo_pra_2017" = "codgeo_2017"), keep = T)
all_pra_com <- all_pra_com %>% group_by(codgeo_2020) %>% mutate(merged = n() > 1) %>% ungroup()
all_pra_com <- all_pra_com %>% group_by(codgeo_2017) %>% mutate(split = n() > 1) %>% ungroup() 
write_csv(all_pra_com, "data/PRA/matched/communes_2017_2020_match_pra.csv")

#keep 2020 communes 
all_pra_com <- all_pra_com %>% 
  dplyr::select(pra_code, ra_code, pra_lib, codgeo_2020, libgeo_2020, dep, can_2013) %>% 
  distinct()

#fuze communes and create pra shapefiles
com_shp <- read_sf(path_shp_com) %>% clean_names() %>% 
  filter(!(insee_dep %in% dep_domtom)) %>% 
  dplyr::select(insee_com, code_epci, nom_com) %>% 
  right_join(all_pra_com, by = c("insee_com" = "codgeo_2020")) 

pra_shp <- com_shp %>%  group_by(pra_code) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

ra_shp <- com_shp %>%  group_by(ra_code) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

can_shp <- com_shp %>% group_by(can_2013) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop") %>% 
  st_as_sf()

#plot 
plot <- tm_shape(pra_shp) + tm_borders(col = "black") +
  tm_shape(com_shp) + tm_borders(col = "red")

# Save files
if (!dir.exists("data/PRA/shp/")) {
  dir.create("data/PRA/shp/", recursive = TRUE)
}
st_write(com_shp, path_comshp, delete_layer = TRUE)
st_write(pra_shp, path_prashp, delete_layer = TRUE)
st_write(can_shp, path_canshp, delete_layer = TRUE)
st_write(ra_shp, path_rashp, delete_layer = TRUE)

