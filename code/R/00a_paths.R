#define project paths 

#GADM 
pathgadm  <- "georeferenced_data/gadm/gadm36_FRA_shp/"
path_gadm4 <- "georeferenced_data/gadm/gadm36_FRA_shp/gadm36_FRA_4.shp"

#SATTELLITE DATA ....
pathsatl <- "georeferenced_data/MODIS" 
pathtemp <- file.path(pathsatl, 
  "Surf_Temp_Monthly_005dg_v6/Time_Series/RData/Terra/LST_Day_CMG", 
  "MOD11C3_LST_Day_CMG_32_2000_335_2021_RData.RData") 
  #used to be MOD11C3_LST_Day_CMG_32_2000_336_2020_RData.RData
pathgpp <- file.path(pathsatl, 
  "Gross_PP_GapFil_8Days_500m_v6/Time_Series/RData/Terra/GPP", 
  "MOD17A2HGF_GPP_1_2000_361_2021_RData.RData") 
  #used to be MOD17A2H_GPP_49_2000_361_2020_RData.RData
gpp_example <- file.path(pathsatl, 
   "Gross_PP_GapFil_8Days_500m_v6/GPP",
   "MOD17A2HGF_GPP_2000_001.tif")
pathnpp <- file.path(pathsatl, 
  "Net_PP_Yearly_500m_v6/Time_Series/RData/Terra/Npp" ,
  "MOD17A3HGF_Npp_49_2000_1_2020_RData.RData")
pathmatch <- "data/FR/cadastre/"
json_temp <- "georeferenced_data/MODIS/json/Temp_11jul2024.json"

#WEATHER STATIONS DATA 
path_synop_data <- "data/FR/synop_france/donnees-synop-essentielles-omm.csv"
path_synop_coor <- "data/FR/synop_france/postesSynop.json.txt"

#Communes-PRA
path_pra <- "data/PRA/raw/Referentiel_CommuneRA_PRA_2017.xls"
path_com <- "data/PRA/raw/communes2020.csv" 
path_pas <- "data/PRA/raw/table_passage_annuelle_2020.xlsx"
path_shp_com <- "georeferenced_data/INSEE/admin-express-cog_FR_20_entiere/COMMUNE.shp"
path_comshp <- "data/PRA/shp/com_shp.shp"
path_prashp <- "data/PRA/shp/pra_shp.shp"
path_rashp <- "data/PRA/shp/ra_shp.shp"
path_canshp <- "data/PRA/shp/can_shp.shp"
path_com2013 <- "data/PRA/comsimp2013.dbf"

#SAFRAN 
path_safr <- "data/FR/safran_france/"

#CADASTRAL DATA ....
pathcada  <- "georeferenced_data/cadastre/FR"
pathcadad <- "data/FR/cadastre"
pathmodi  <- "georeferenced_data/MODIS/Exported_data"

#FOREST DATA 
pathprot <- "georeferenced_data/protected_areas/"

#CORINE DATA 
pathcori <- "georeferenced_data/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"
pathlegcori <- "georeferenced_data/u2018_clc2018_v2020_20u1_geoPackage/Legend/clc_legend.xls"
pathrescori <- "georeferenced_data/u2018_clc2018_v2020_20u1_geoPackage/"

#NAMES OF THINGS 
fileilot_rast <- "farm_parcels_light"
filefood_rast <- "food_parcels_light" 

#CANTON CENTROINDS WITH ELEVATION (BY GOOGLE'S API)
path_gadm4_elev <- "data/FR/synop_france/gadm4_elevation_by_googleapi/gadm4_elevation_by_googleapi.shp"

