# setup
gc()
source("code/R/00a_paths.R")
source("code/R/00b_libraries.R")
libraries(stdlibs, geolibs, "haven")
source("code/R/functions/wp_sum.R")
source("code/R/functions/wp_map.R")

#define paths 
wp <- "data/FR/multi_panel_foods_pa28.dta"
gadmp <- "georeferenced_data/gadm/gadm36_FRA_shp/gadm36_FRA_4_areas.shp"

#prepare cadastral variables 
cadgeo_mean <- wp_sum(var_name = "cadastral", dta = wp, geo = gadmp, stat = "mean") 
cadgeo_2015 <- wp_sum(var_name = "cadastral", dta = wp, geo = gadmp, sel.year = 2015) 

# get temperature threshold data
wp_map(var = "wshock", data = cadgeo_2015, style = "cont", breaks = 29:33,
 title = "Temperature \n threshold (ºC), \n 2015", digits = 0,
 file_name = "maps/tempshock_foods_2015.png")
wp_map(var = "wshock", data = cadgeo_mean, style = "cont", breaks = 29:33,
  title = "Temperature \n threshold (ºC), \n mean", digits = 0,
  file_name = "maps/tempshock_foods_mean.png")

## number of weekly heatwaves
weekly_nshocks <- wp_sum(var_name = "the_shock_s", dta = wp, geo = gadmp, 
  stat = "add") 
wp_map(data = weekly_nshocks, var = "the_shock_s",
   title = "nº of shock \n weeks", 
   file_name = "maps/shocknum_foods_safran_tsup.png",
   style = "fixed",
   breaks = c(0, 1, 3, 5, 7, Inf),
   labels = c("0", "1-2", "3-4", "5-6", "7+"),
   add_borders = F)

# get annual shock count data
yearly_nshocks <- wp_sum(var_name = "the_shock_s_y", dta = wp, geo = gadmp,
  stat = "add")
wp_map(data = yearly_nshocks, var = "the_shock_s_y",
          title = "nº of shock \n years", 
          file_name = "maps/shocknumy_foods_safran_tsup2.png",
          style = "fixed", breaks = c(0:4), labels = c("0", "1", "2", "3"),
          add_borders = F, as.count = T)

## panel (c): gini
wp_map(var = "gini_can", data = cadgeo_mean, style = "quantile", n = 4,
       palette = "-RdYlBu", title = "Gini coefficient, \n mean", 
       add_borders = F, file_name = "maps/gini_mean.png")
wp_map(var = "gini_can", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "-RdYlBu", title = "Gini coefficient, \n 2015", 
       add_borders = T, file_name = "maps/gini_2015.png")

#Farm size 
wp_map(var = "fsize", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "YlOrBr", title = "Avg. farm size, \n (ha.) 2015", 
       add_borders = F, file_name = "maps/fsize_2015.png")

#Seminatural areas 
wp_map(var = "seminat", data = cadgeo_2015, style = "quantile", n = 4, digits = 0,
       palette = "Greens", title = "Semi-natural, \n areas, 2015, \n (% of farmland)", 
       add_borders = F, file_name = "maps/seminat_2015.png")

#dfct_r 
wp_map(var = "dfct_r", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "RdYlGn", title = "Diversification, \n ratio, 2015", 
       add_borders = F, file_name = "maps/dfct_r_2015.png")

#dcount
wp_map(var = "dcount_o", data = cadgeo_2015, style = "quantile", n = 4, digits = 0,
       palette = "RdYlGn", title = "nº of crop \n types, 2015 \n (detailed)", 
       add_borders = F, file_name = "maps/dcount_o_2015.png")

#ccount 
wp_map(var = "ccount_o", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "RdYlGn", title = "nº of crop \n groups, 2015", digits = 0,
       add_borders = F, file_name = "maps/ccount_o_2015.png")

#herfindahl 
wp_map(var = "herfindahl", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "-RdYlGn", title = "Hirschman- \nHerfindahl\nindex, 2015", digits = 2,
       add_borders = F, file_name = "maps/herfindahl_2015.png")

#dherf_o 
wp_map(var = "dherf_o", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "-RdYlGn", title = "Hirschman- \nHerfindahl\nindex (detailed), \n2015", digits = 2,
       add_borders = F, file_name = "maps/dherf_o_2015.png")

#prairies 
wp_map(var = "prairies", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "Greens", title = "Prairies \n(% of agri. area), \n2015", digits = 2,
       add_borders = F, file_name = "maps/prairies_2015.png")

#crop28
wp_map(var = "crop28", data = cadgeo_2015, style = "quantile", n = 4,
       palette = "Greens", title = "Crop 28, \n2015", digits = 2,
       add_borders = F, file_name = "maps/crop28_2015.png")


#crop28 nfarms lfarms cv_o 


#other variables 

## panel (b): map temperature raster (day 213, 2021)
# set paths
#sampletempp <- "MOD11C3_LST_Day_CMG_2021_213.tif"
#temppath <- "Surf_Temp_Monthly_005dg_v6/LST_Day_CMG"

# read raster in
#sampletemp <- raster(file.path(pathsatl, temppath, sampletempp))
#sampletemp <- sampletemp - 273.15
#names(sampletemp) = "temp"

# map smpletemp 
# temp_map <- tm_shape(sampletemp) +
#   tm_raster("temp", palette = "magma", style = "cont",
#             title = "Average \n temperature (ºC)") +
#   tm_layout(legend.title.size = 2,
#             legend.text.size = 1.5,
#             legend.position = c("left","bottom"),
#             legend.bg.color = "white",
#             legend.bg.alpha = 0.5,
#             frame = FALSE)
# temp_map
# tmap_save(temp_map, filename = "maps/temp_map_sat2.png", dpi = 300)

## panel (d): temp threshold by canton
