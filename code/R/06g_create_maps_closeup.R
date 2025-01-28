
### figure A.2
## panel (a): farms and cantons around paris 

# create a box indicating the area
e <- ext(643500, 676000, 6832500, 6865000) # define extent
box <- as.polygons(e) # convert extent to a polygon
crs(box)  <- "epsg:2154" #reproject
pariszoom <- st_as_sf(box) # convert to sf

# read in canton data
gadm <- vect(file.path(pathgadm, "gadm36_FRA_4.shp")) %>%
  terra::project("epsg:2154") %>% # reproject
  crop(box) # crop to only keep the area of interest

# read in ilot (farm) and parcel data of 2021
farm <- vect(file.path(pathcada, 2021, "ILOTS_ANONYMES.gpkg"), 
             filter = box) %>% # crop to only keep area of interest
  sf::st_as_sf() %>% # convert spatvect to sf object
  st_make_valid() %>% # fix invalid shapes
  st_crop(box) # again, crop to only keep area of interest

parcel <- vect(file.path(pathcada, 2021, "PARCELLES_GRAPHIQUES.gpkg"), 
               filter = box) %>%
  sf::st_as_sf() %>%
  st_make_valid() %>%
  st_crop(box)

# convert cantons to sf object as well
gadm %<>% st_as_sf %>%
  st_make_valid() # fix invalid shapes if there are any

# add a variable to label only paris and not others
gadm %<>%
  mutate(label = if_else(GID_4 == "FRA.8.3.11.1_1", "Paris", "", missing = NA))

# crop gpp to get the area of interest
gpp <- crop(natgpp, gadm)

# buffer and rasterize farms, then mask gpp using the rasterized farms
# (to keep the outer edges of gpp raster)
farmbuf <- st_buffer(farm, 50)
farmras <- rasterize(farmbuf, gpp, getCover=TRUE)
farmras[farmras==0] <- NA

farmras %<>% resample(gpp, method = "ngb") 
gpp_cropped <- mask(gpp, farmras)

# create another box indicating the area for the third panel
ezoom <- st_bbox(c(xmin=671000, xmax=674000,
                   ymin=6836000, ymax=6839000),
                 crs = st_crs(2154)) # define extent, reproject
parcelzoom <- st_as_sfc(ezoom) # convert to polygon

# map
parisfarm <- tm_shape(gpp_cropped) +
  tm_raster("GPP", palette = "YlGn", style = "fixed", 
            breaks = seq(0.4, 1.4, by=0.1),
            title = "Biomass growth \n (GPP)") +
  tm_shape(gadm) +
  tm_borders(col = "gray", lwd = 1) +
  tm_text("label") +
  tm_shape(farm) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(parcelzoom) +
  tm_borders(col = "red", lwd = 2.5) +
  tm_layout(inner.margins = c(0.1, 0.02, 0.02, 0.02)) +
  tm_scale_bar(text.size = 0.8, position=c("right", "bottom")) +
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE) + 
  tm_add_legend(title = "Borders",
                type = "line", 
                col = c("gray", "black"),
                lwd = c(1, 1.5),
                lty = c("solid", "solid"),
                labels = c("Canton", "Farm"))
parisfarm
tmap_save(parisfarm, filename = "maps/parisfarm.png", dpi = 300)

## panel (b): parcels with crop labels

# crop parcel and farm data to get the zoomed in area
farm_zoom <- st_crop(farm, parcelzoom)
parcel_zoom <- st_crop(parcel, parcelzoom)
gadm_zoom <- st_crop(gadm, parcelzoom)

# crop gpp data further to get the zoomed in area 
# (needed to use buffer due to big pixels)
bufa <- st_buffer(parcel_zoom, dist = 200)
gpp_zoom <-crop(gpp_cropped, bufa)

# map
parcelcrop <- tm_shape(gpp_zoom) +
  tm_raster("GPP", palette = "YlGn", style = "fixed", 
            breaks = seq(0.4, 1.4, by=0.1),
            title = "Biomass growth \n (GPP)") +
  tm_shape(parcel_zoom) +
  tm_borders(col = "black", lwd = 1, lty = "dashed") +
  tm_shape(farm_zoom) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(legend.title.size = 1.8,
            legend.text.size = 1.5,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            frame = FALSE)+
  tm_layout(inner.margins = c(0.1, 0.02, 0.02, 0.02)) +
  tm_scale_bar(text.size = 0.8, position=c("right", "bottom")) +
  tm_add_legend(title = "Borders",
                type = "line", 
                col = c("black", "black"),
                lwd = c(2, 1),
                lty = c("solid", "dashed"),
                labels = c("Farm", "Parcel")) +
  tm_layout(inner.margins = c(0.1, 0.02, 0.02, 0.02)) +
  tm_scale_bar(text.size = 0.8, position=c("right", "bottom"))
parcelcrop
tmap_save(parcelcrop, filename = "maps/parcelcrop.png", dpi = 300)


