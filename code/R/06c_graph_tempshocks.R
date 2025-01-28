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
library(tidyr)

#setwd("~/Dropbox/land_ineq_degradation/")

#paths 
wp <- "data/FR/monthly_panel.dta"
gadmpath <- "georeferenced_data/gadm/gadm36_FRA_shp"
savepath <- "figures/"

#open panel 
wpanel <- read_dta(wp, .name_repair = "universal")

#make a toy version to work faster 
wpanel %<>% filter(year >= 2010) #%>% #(erase line for full mode)
#filter(name_1 == "Occitanie") #even ligther version 

#summarize at month level
wpanel %<>%
  mutate(
    hot=ifelse(temp_canton>25, 1, 0), 
    cold=ifelse(temp_canton<=0, 1,0)) %>%
  group_by(name_1, name_4, month) %>%
  summarise(
    temp=mean(temp_canton, na.rm=T),
    hot=sum(hot, na.rm=T),
    cold=sum(cold, na.rm=T)) %>%
  mutate(
    hotsum=sum(hot), coldsum=sum(cold)
  ) 

wpanel$monthlab[wpanel$month==1] <- "Jan"
wpanel$monthlab[wpanel$month==2] <- "Feb"
wpanel$monthlab[wpanel$month==3] <- "Mar"
wpanel$monthlab[wpanel$month==4] <- "Apr"
wpanel$monthlab[wpanel$month==5] <- "May"
wpanel$monthlab[wpanel$month==6] <- "Jun"
wpanel$monthlab[wpanel$month==7] <- "Jul"
wpanel$monthlab[wpanel$month==8] <- "Aug"
wpanel$monthlab[wpanel$month==9] <- "Sep"
wpanel$monthlab[wpanel$month==10] <- "Oct"
wpanel$monthlab[wpanel$month==11] <- "Nov"
wpanel$monthlab[wpanel$month==12] <- "Dec"

ms <- c("Jan", "Feb", "Mar", "Apr", "May",
        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 

#merge with shapefile 
gadm4 <- sf::st_read(file.path(
  gadmpath, "gadm36_FRA_4_areas.shp") , quiet=TRUE) %>%
  select(NAME_1, NAME_4, geometry) %>% 
  mutate(names = paste0(NAME_1, " - ", NAME_4))
mapped <- merge(gadm4, wpanel, by.x="NAME_4", by.y="name_4") %>%
  arrange(names, month) %>% group_by(NAME_4, month)

#put all years together 
alltogether <- wpanel %>% ungroup() %>%
  mutate(names = paste0(name_1, " - ", name_4)) %>%
  select(names, hotsum, coldsum, name_4) %>% group_by(names) %>%
  summarize(hotsum=mean(hotsum), coldsum=mean(coldsum)) %>%
  pivot_longer(cols=c("coldsum", "hotsum"))
alltogether <- merge(gadm4, alltogether, by="names") %>%
  arrange(names) %>% group_by(names) %>% 
  rename(shock_type=`name`)
colds <- filter(alltogether, shock_type=="coldsum") %>% 
  select(names, value, geometry)
hots <- filter(alltogether, shock_type=="hotsum") %>% 
     select(names, value, geometry)

#plot all together
tmap_mode("plot")
coldshocks <- tm_shape(colds) + tm_fill("value", style="quantile",
  title="Days below 0ºC between 2010-2020", palette="YlGnBu", n=5,
  legend.hist = TRUE) 
hotshocks <- tm_shape(hots) + tm_fill("value", style="quantile",
 title="Days over 25ºC between 2010-2020", palette="YlOrRd", n=5,
 legend.hist = TRUE) 
allshocks <- tmap_arrange(hotshocks, coldshocks)
tmap_save(allshocks, paste0(savepath, "temperatures/allshocks.png"),
  width=1920, height=1080, asp=0)

tmap_mode("view")
tmap_save(coldshocks, paste0(savepath, "temperatures/coldshocks.html"),
  width=1920, height=1080, asp=0)
tmap_save(coldshocks, paste0(savepath, "temperatures/hotshocks.html"),
          width=1920, height=1080, asp=0)
coldshocks
# 
# heatmaps <- 
#   tm_shape(mapped) +
#   tm_fill("hot", col="red", border.lwd=0, alpha=1, style="pretty") +
#   tm_facets(by = "monthlab") 
# 
# #graph bubbles with total in country with both hot and cold 
# 
# tmap_animation(heatmaps, 
#    delay=50, filename = "~/Desktop/heatmap_gini.mp4",
#    width=1200, height = 600, fps = 2, outer.margins = 0)

