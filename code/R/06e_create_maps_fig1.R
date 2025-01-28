# creates maps for the paper
# setup
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs, "haven")
source("code/R/functions/working_panel_mapper.R")

# define tmap options
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

### figure 1 GPP 
## panel (a): map annual gpp
# calculate cumulative annual gpp data for 2021
# brick raster layers for 2021 and reclassify null values 
year_files <- list.files(file.path(pathsatl, "Gross_PP_GapFil_8Days_500m_v6/GPP"), 
                         pattern = "MOD17A2HGF_GPP_2021", full.names = T)
gpp_stack <- year_files %>% stack(quick=T) %>% raster::reclassify(c(3.276, 4, NA))
natgpp <- calc(gpp_stack, sum) # sum all values of the brick 
names(natgpp) = "GPP"

# map
gpp_map_2021 <- tm_shape(natgpp) +
  tm_raster("GPP", palette = "YlGn", style = "cont",
            title = "Biomass growth \n (GPP)") +
  tm_layout(legend.title.size = 2,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE)
gpp_map_2021
tmap_save(gpp_map_2021, filename = "maps/gpp_map_2021_gf2.png", dpi = 300)

## panel (b): map temperature raster (day 213, 2021)
# set paths
sampletempp <- "MOD11C3_LST_Day_CMG_2021_213.tif"
temppath <- "Surf_Temp_Monthly_005dg_v6/LST_Day_CMG"

# read raster in
sampletemp <- raster(file.path(pathsatl, temppath, sampletempp))
sampletemp <- sampletemp - 273.15
names(sampletemp) = "temp"

# map smpletemp 
temp_map <- tm_shape(sampletemp) +
  tm_raster("temp", palette = "magma", style = "cont",
            title = "Average \n temperature (ºC)") +
  tm_layout(legend.title.size = 2,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE)
temp_map
tmap_save(temp_map, filename = "maps/temp_map_sat2.png", dpi = 300)

## panel (c): national coverage of farms
# read in the latest (2021) farm raster
natfarm <- raster(file.path(pathcada, 2021, "farm_parcels_light.tif"))  %>% 
  reclassify(c(-100000, 100000, 1)) # change all values to 1 to map in one color
names(natfarm) = "fill"

# read in national border of france
natborder <- st_read(file.path(pathgadm, "gadm36_FRA_0.shp")) %>%
  st_transform(crs = "epsg:2154") #reproject

# map
farmcov <- tm_shape(natborder) +
  tm_fill(col = "gray85",
          legend.show = F) +
  tm_shape(natfarm) +
  tm_raster("fill", palette = "darkgreen", 
            legend.show = F) +
  tm_layout(legend.title.size = 2,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE) +
  tm_add_legend(type = "fill", 
                border.lwd = 0,
                col = c("darkgreen", "gray85"),
                labels = c("Farmland", "Non-farmland"),
                title = "Land use")
farmcov
tmap_save(farmcov, filename = "maps/farms_cov.png", dpi = 300)

#gini 
ginicoef <- tm_shape(shockcount) + 
  tm_fill("gini_can", legend.hist = TRUE , 
          title = "Land Gini \n coefficient", 
          style="quantile", palette = "-RdYlBu", 
          n=5,
          popup.vars = c("Gini" = "gini_can", "GID_2" = "GID_2", "GID_4" = "GID_4")
  ) +
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE) 
ginicoef 
tmap_save(ginicoef, "maps/ginicoef_foods.png", dpi = 300)
