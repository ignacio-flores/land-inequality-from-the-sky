library(haven)
library(dplyr)
library(magrittr)
library(janitor)
library(sf)
library(viridis)
library(RColorBrewer)
library(rgdal)
library(tmap)
library(tmaptools)
library(osmdata)
library(osmplotr)
library(gifski)
library(av)
library(ggplot2)

#setwd("~/Dropbox/land_ineq_degradation/")

#paths 
wp <- "data/FR/working_panel_farms.dta"
gadmpath <- "georeferenced_data/gadm/gadm36_FRA_shp"
savepath <- "figures/"
theyear = 2021

#open panel 
wpanel <- read_dta(wp, .name_repair = "universal")

#summarize at year level
wpanel %<>% group_by(name_1, name_4, year) %>%
  summarise(
    gpp=sum(gpp_farmland, na.rm=T)*10, 
    npp=mean(mean_ion_npp, na.rm=T), 
    agri=mean(area_agri, na.rm=T), 
    gini=mean(gini_can, na.rm=T), 
    hold=mean(n_holdings, na.rm=T),
    herf=mean(herfindahl, na.rm=T)
  ) %>%
  mutate(dif=(gpp-npp)/gpp*100, empty = NA) %>%
  group_by(name_1, name_4, year)

#make a toy version to work faster 
wpanel %<>% filter(year == theyear) #%>% #(erase line for full mode)
  #filter(name_1 == "Occitanie") #even ligther version 

#merge with shapefile 
gadm4 <- sf::st_read(file.path(gadmpath, "gadm36_FRA_4_areas.shp"),
  quiet=TRUE)
gadm2 <- sf::st_read(file.path(gadmpath, "gadm36_FRA_2.shp"),
  quiet=TRUE)

mapped <- merge(gadm4, wpanel, by.x="NAME_4", by.y="name_4") %>%
  arrange(NAME_2, NAME_4, year)

#histogram of diff GPP - NPP
ggplot(mapped, aes(dif)) + geom_histogram() + 
  ggtitle(paste0("Gross vs Net Productivity ", theyear ," (diff as % of GPP)"))
ggsave(paste0(savepath, "productivity/hist_gpp_npp_diff", theyear, ".png"), 
  plot = last_plot())

#interactive plot of diff for one year
tmap_mode("view")
compare_prod <- 
  tm_shape(mapped) + 
  tm_fill("dif", style="jenks", legend.format=list(list(digits=1)), 
          legend.hist = TRUE, title=c("Difference (% of GPP)")) 
tmap_save(compare_prod, 
  paste0(savepath,"productivity/compare_npp_to_gpp_", theyear, ".html")
)

#Static plot of gpp vs npp 
tmap_mode("plot")
mapvars <- c("npp", "gpp", "dif")
compare_prod <-
  tm_shape(mapped) +
  tm_fill(
     mapvars, style="jenks",
     legend.format=list(list(digits=1)),
     legend.hist = TRUE,
     title=c("Net productivity", "Gross productivity", "Difference (% of GPP)")
  )
tmap_save(compare_prod,
  paste0(savepath, "productivity/compare_npp_to_gpp_", theyear,".png")
)


# #plot static 
tmap_mode("plot")
mgini <- tm_shape(mapped) + 
  tm_fill("gini", legend.hist = TRUE, style="quantile",
    palette = "BrBG", legend.format=list(list(digits=0))) 
mgpp <- tm_shape(mapped) + 
   tm_fill("gpp", legend.hist = TRUE, style="quantile",
     palette = "Greens", legend.format=list(list(digits=1)))
magri <- tm_shape(mapped) + 
   tm_fill("agri", legend.hist = TRUE, style="quantile",
     palette = "Blues", legend.format=list(list(digits=0)))
mherf <- tm_shape(mapped) + 
   tm_fill("herf", legend.hist = TRUE, style="quantile",
     palette = "BrBG", legend.format=list(list(digits=1)))
four <- tmap_arrange(mgini, mgpp, magri, mherf) 
tmap_save(mgini, paste0(savepath, "gini_by_canton", theyear,".png"), 
          width=1920, height=1080, asp=0)
tmap_save(mgpp, paste0(savepath, "gpp_by_canton", theyear,".png"), 
          width=1920, height=1080, asp=0)
tmap_save(magri, paste0(savepath, "agri_by_canton", theyear,".png"), 
          width=1920, height=1080, asp=0)
#tmap_save(four, paste0(savepath, "map_four_vars", theyear,".png"), 
#          width=1920, height=1080, asp=0)

#plot dynamic (comparable)
# tmap_mode("view")
# mapvars <- c("gini", "gpp", "agri", "herf")
# tm_shape(mapped) + 
#   tm_fill(mapvars, style="quantile", 
#   legend.format=list(list(digits=0))) 
