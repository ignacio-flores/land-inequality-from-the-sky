library(raster)
library(rgdal) 
library(sf)
library(magrittr)
library(stringr)
library(animation)
library(RColorBrewer)
library(exactextractr)
library(rgeos)
library(ggplot2)
library(plotly)
library(reshape2)

setwd("~/Dropbox/land_ineq_degradation/georeferenced_data/")
i = 2010

#load farmland and non-farmland 
farms = raster(paste("cadastre/FR/",i,"/RASTER_of_ilots.tif", sep =""))
nonfarm = raster(paste("cadastre/FR/",i,"/RASTER_of_ilots_inverted.tif", sep =""))
nonfarm %<>% resample(farms, method = "ngb")


#EXAMPLE OF GPP INTERSECTION WITH FARMLAND 
#gpp %<>% reclassify(c(3.276, 4, NA)) %>% resample(farms, method = "ngb") %>% 
#  intersect(extent(farms))

#Stack rasters yearly and reclassify null values 
year_files <- list.files("MODIS/Gross_PP_8Days_500m_v6/GPP", 
                         pattern = "MOD17A2H_GPP_2020", full.names = T)
year_raster <- year_files %>% stack(quick=T) %>% reclassify(c(3.276, 4, NA))
names(year_raster)

#get yearly cumulative sum 
cumsum <- year_raster[[1]] 
the_one <- year_raster[[1]] 
for (i in 2:length(year_files)) {
  #identify layer 
  current <- year_raster[[i]] 
  #new layer to comulative sum 
  cumsum <- stack(c(current, cumsum), quick=T) %>% calc(sum)
  tempname <- sub("MODIS/Gross_PP_8Days_500m_v6/GPP/*", "", year_files[i])    
  names(cumsum) <- sub("*.tif", "", tempname)
  #add another layer to the stack 
  the_one %<>% stack(cumsum) 
}
saveGIF(
  animate(the_one, pause=.1, axes = F)
)

#graficar de aqui
df <- melt(farms)
fig <- ggplotly(
 ggplot(farms) + geom_raster(aes(fill=values))
)  
fig

#summary statistics 
last_i <- length(year_files)
layername <- sub("MODIS/Gross_PP_8Days_500m_v6/GPP/*", "", year_files[last_i]) 
layername <- sub(".tif", "", layername)

#isolate agricultural production 
annual <- the_one[[last_i]] %>% 
  resample(farms, method = "ngb") %>% 
  intersect(extent(farms))
annual_farms <- annual_farms * farms 
annual_nonfarms <- annual_farms * nonfarm
ton_by_pixel <- farms * 500^2
ton_by_pixel2 <- nonfarm * 500^2
plot(annual_farms, axes=F)

#Histogram 
#p1 <- hist(annual_farms, freq=F)
#p2 <- hist(annual_nonfarms, freq=F)
#plot(p1, col=rgb(0,0,1,1/4))  
#plot(p2, col=rgb(1,0,0,1/4), add=T)  

#get yearly stats 
fr <- read_sf(dsn="gadm/gadm36_FRA_shp/", layer="gadm36_FRA_0_reprojected")
#fr <- st_transform(fr, crs = st_crs(2154))
farmland_data <- 
  exact_extract(
    annual_farms, fr, weights=ton_by_pixel, 
    c("weighted_mean", "mean", "max", "min", "median", "weighted_sum", "sum")
  )

nonfarmland_data <- 
  exact_extract(
    annual_nonfarms, fr, weights=ton_by_pixel2, 
    c("weighted_mean", "mean", "max", "min", "median", "weighted_sum", "sum")
  )




