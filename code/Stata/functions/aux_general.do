
//PRELIMINARY SETTINGS 
global tps foods farms 
global mainvars /// 
	 gini_can lfarms fsize seminat dherf_o dfct_r 
	
//define condition 
global cutoffs 30 

global hots //empty the global 
foreach n in $cutoffs {
	global hot`n'_condition temp > `n'
	global hots $hots hot`n'
	di as result "hot`n'_condition: ${hot`n'_condition}"
}

//shock definitions 
global the_shock_condition weighted shock 
global the_shock_w_condition weighted shock (weekly, Interpolated Weather Stations)
global the_shock_m_condition weighted shock (monthly, MODIS satellite)
global the_shock_s_condition weighted shock (weekly Meteo France modelling)
di as result "hots: ${hots}"

//defining fixed effect labeling 
if "${absorber}" == "i.canton i.t" global abs c1
if inlist("${absorber}", ///
	"i.canton##i.month", "i.canton##i.month i.t", "i.canton##i.p i.t") {
	global abs c2			
} 
	

*define number of quantiles 
global q = 4 

if $q == 10 {
	global quant D
	global quant2 Deciles
	global nr = 2
}
if $q == 5 {
	global quant Q
	global quant2 Quintiles
	global nr = 1
}
if $q == 4 {
	global quant Q
	global quant2 Quartiles
	global nr = 1
}
if $q == 3 {
	global quant T
	global quant2 Thirds
	global nr = 1
}
if $q == 2 {
	global quant H
	global quant2 Halves
	global nr = 1
}

//first and last years for loops 
global first_y = 2015
global last_y = 2021

//minimum agricultural area
global min_agri = 0

//matched datasets 
global ilot_to_canton data/FR/cadastre/ilot_to_canton
global parcel_to_ilot data/FR/cadastre/parcel_to_ilot
global satl_to_canton data/FR/MODIS/satellite_to_canton
global modis_exported georeferenced_data/MODIS/Exported_data
global corine_areas ///
	georeferenced_data/u2018_clc2018_v2020_20u1_geoPackage/corine_areas

//define paths to insee shapefiles 
global adm_insee georeferenced_data/INSEE/admin-express-cog_FR_20_entiere/
global adm_insee_com ${adm_insee}COMMUNE.dbf 

//define paths for gadm
global gadm_fr georeferenced_data/gadm/gadm36_FRA_shp/
forvalues n = 0/5 {
	global gadm`n'_fr ${gadm_fr}gadm36_FRA_`n'.dbf
	global gadm`n'_fr_protected ${gadm_fr}gadm36_FRA_`n'_protected.dbf
	global gadm`n'_fr_reproject ${gadm_fr}gadm36_FRA_`n'_reprojected.dbf
	global gadm`n'_fr_aux ${gadm_fr}gadm36_FRA_`n'_aux.dbf
	global gadm`n'_fr_areas ${gadm_fr}gadm36_FRA_`n'_areas.dbf
	global gadm`n'_fr_insee ${gadm_fr}gadm36_FRA_`n'_INSEE.dbf
}

//define paths for .dta and .csv files 
global raw_fr data/FR/raw/
global ineq_fr data/FR/ineq/
global dead_fr data/FR/deces-2010-2020-csv/
forvalues y = 2010/2020 {
	global raw_fr_`y' ${raw_fr}fr_raw_`y'.dta
	global ineq_fr_`y' ${ineq_fr}fr_ineq_`y'.dta
	global dead_fr_`y' ${dead_fr}deces-`y'.csv
}

//define paths for insee-codes 
global com_codes_2020 data/FR/insee-codes/table_passage_annuelle_2020.xlsx

//define paths on nº of dead people 
global mortality_fr data/FR/deces-2010-2020-csv/mortality_panel.dta

//define paths for census data 
foreach x in "1988" "2000" "2010" {
	global census_fr_`x' data/FR/agri_census/FDS_G_2007_`x'.txt 
}
global census_fr data/FR/agri_census/census_panel.dta

//define path to cadastral panel 
global cadastral_panel_fr data/FR/cadastral_panel.dta
global working_panel_fr data/FR/working_panel.dta
global panel_test data/FR/working_test.dta
global yearly_panel_fr data/FR/working/yearly_panel.dta
global monthly_panel_fr data/FR/monthly_panel.dta

//versions 
global v1 no28 
global v2 wi28
global v3 pa28 
global v1_f _no_c28
global v2_f _with_c28
global v3_f _part_c28

//other paths 
global cadastre_fr georeferenced_data/cadastre/FR/

label define code_group_lab ///
0"Unexploited" 1"Wheat" 2"Corn" 3"Barley" 4"Other cereals" ///
5"Colza" 6"Sunflower" 7"Other oilseeds" 8"Protein crops" ///
9"Fiber plants" 10 "Seeds (old)" 11"Uncultivated land" ///
12"Uncultivated, industrial (old)" 13"Other uncultivated land (old)" ///
14"Rice" 15 "Grain legumes" 16"Feed" 17"Sum. pastures & moors" ///
18"Perm. meadow" 19"Temp. meadow" 20"Orchards" 21"Vines" 22 "Nuts" ///
23"Olive" 24 "Other indsutrial crops" 25 "Vegetables & flowers" ///
26"Sugar cane" 27"Arboriculture (old)" 28"Others" ///
99"Not identified", replace

//variable labels 
global lab_gpp "Productivity, C.kg/m{superscript:2}"
global lab_gpp_w "Productivity, C.kg/m{superscript:2} (weekly)"
global lab_gpp_m "Productivity, C.kg/m{superscript:2} (monthly)"
global lab_agpp "Productivity (log), C.kg/m{superscript:2}"
global lab_agpp_w "Productivity (log), C.kg/m{superscript:2}"
global lab_agpp_m "Productivity (log), C.kg/m{superscript:2}"
global lab_cum_gpp "Cumulated production, C.kg/m{superscript:2}"
global lab_cum_gpp_w "Cumulated production, C.kg/m{superscript:2}"
global lab_cum_gpp_m "Cumulated production, C.kg/m{superscript:2}"
global lab_acum_gpp "Cumulated production (log), C.kg/m{superscript:2}"
global lab_acum_gpp_w "Cumulated production (log), C.kg/m{superscript:2}"
global lab_acum_gpp_m "Cumulated production (log), C.kg/m{superscript:2}"
global lab_std_gpp "Productivity, standardized"
global lab_std_cum_gpp "Cumulated production, standardized"
global lab_temp "Average monthly temperature (ºC)"
global lab_temp_w "Average monthly temperature (ºC), interpolated from weather stations"
global lab_temp_m "Average monthly temperature (ºC), uncovered condition (MODIS)"
global lab_gini_can "Land inequality (Gini coefficient)"  
global lab_gini_f "Land inequality (Gini coefficient farms)"  
global lab_dfct_r "Diversification ratio"  
global lab_cv_o "Coefficient of variation (areas)"  
global lab_cv_f "Coefficient of variation farms (areas)"  
global lab_sd_logs "Standard deviation of log (areas)"  
global lab_herfindahl "Crop concentration (Intercrop HHI)"  
global lab_cherf_f "Crop concentration farms (Intercrop broad HHI)"  
global lab_dherf_o "Crop concentration (d), HHI"
global lab_dherf_f "Crop concentration farms (d), HHI"
global lab_ccount_o "Crop counter (Intercrop)"
global lab_ccount_f "Crop counter farms (Intercrop)"
global lab_dcount_o "Crop diversity (inter-intra crop)"
global lab_dcount_f "Crop diversity farms (inter-intra crop)"
global lab_dct_km2 "nº species by km2"
global lab_dct_akm2 "nº species by km2 (agri)"
global lab_fsize "Farm size"
global lab_dct_fsize "Crop count / average farm size"
global lab_max_crop "1st crop's land share'"
global lab_lfarms "Percentage of large farms (large + very)"
global lab_seminat "Percentage of seminatural areas"

//labels Safran database 
global lab_prenei_q "Précipitations solides (cumul quotidien 06-06 UTC)"
global lab_preliq_q "Précipitations liquides (cumul quotidien 06-06 UTC)"
global lab_pe_q "Pluies efficaces (cumul quotidien)"
global lab_t_q "Température (moyenne quotidienne)"
global lab_tinf_h_q "Température minimale des 24 températures horaires"
global lab_tsup_h_q "Température maximale des 24 températures horaires"
global lab_ff_q "Vent (moyenne quotidienne)"
global lab_dli_q "Rayonnement atmosphérique (cumul quotidien)"
global lab_ssi_q "Rayonnement visible (cumul quotidien)"
global lab_evap_q "Evapotranspiration réelle (cumul quotidien 06-06 UTC)"
global lab_etp_q "Evapotranspiration potentielle (formule de Penman-Monteith)"
global lab_q_q "Humidité spécifique (moyenne quotidienne)"
global lab_hu_q "Humidité relative (moyenne quotidienne)"
global lab_swi_q "Indice d'humidité des sols (moyenne quotidienne 06-06 UTC)"
global lab_drainc_q "Drainage (cumul quotidien 06-06 UTC)"
global lab_runc_q "Ruissellement (cumul quotidien 06-06 UTC)"
global lab_wg_racine_q "Contenu en eau liquide dans la couche racinaire à 06 UTC"
global lab_wgi_racine_q "Contenu en eau gelée dans la couche de racinaire à 06 UTC"
global lab_resr_neige_q "Equivalent en eau du manteau neigeux (moyenne quotidienne 06-06 UTC)"
global lab_resr_neige6_q "Equivalent en eau du manteau neigeux à 06 UTC"
global lab_hteurneige_q "Epaisseur du manteau neigeux (moyenne quotidienne 06-06 UTC)"
global lab_hteurneige6_q "Epaisseur du manteau à 06 UTC"
global lab_hteurneigex_q "Epaisseur du manteau neigeux maximum au cours de la journée"
global lab_snow_frac_q "Fraction de maille recouverte par la neige (moyenne quotidienne 06-06 UTC)"
global lab_ecoulement_q "Ecoulement à la base du manteau neigeux"
 


//Crop labels 
global crop_lab_1 "Wheat"  
global crop_lab_2 "Corn"
global crop_lab_3 "Barley"
global crop_lab_4 "Other Cereals" 
global crop_lab_5 "Colza" 
global crop_lab_6 "Sunflower" 
global crop_lab_7 "Other oilseeds" 
global crop_lab_8 "Protein crops" 
global crop_lab_9 "Fiber plants" 
global crop_lab_10 "Seeds (old)" 
global crop_lab_11 "Uncultivated land" 
global crop_lab_12 "Uncultivated, industrial (old)" 
global crop_lab_13 "Other uncultivated land (old)" 
global crop_lab_14 "Rice" 
global crop_lab_15 "Grain legumes" 
global crop_lab_16 "Feed" 
global crop_lab_17 "Sum. pastures & moors" 
global crop_lab_18 "Perm. meadow" 
global crop_lab_19 "Temp. meadow"
global crop_lab_20 "Orchards"
global crop_lab_21 "Vines"
global crop_lab_22 "Nuts"
global crop_lab_23 "Olive"
global crop_lab_24 "Other indsutrial crops"
global crop_lab_25 "Vegetables & flowers"
global crop_lab_26 "Sugar cane"
global crop_lab_27 "Arboriculture (old)"
global crop_lab_28 "Other crops"
global crop_lab_99 "Not identified"
global crop_lab_unexpl "Unexploited farmland"
global crop_lab_ilot_agri "Exploited farmland"

global col_c6 sienna*0.7 
global col_c16 sienna 
global col_c4 gold*0.3 
global col_c3 gold*0.7 
global col_c2 gold 
global col_c1 sand 
global col_c17 dkgreen*0.3 
global col_c19 dkgreen*0.7 
global col_c18 dkgreen
global col_c20 blue*.3

label define mod_com_lab ///
10"Changement de nom" 20"Création" 21"Rétablissement" ///
30"Suppression" 31"Fusion simple" ///
32"Création de commune nouvelle" 33"Fusion association" ///
34"Transformation de fusion association en fusion simple" ///
41"Changement de code dû à un changement de département" ///
50"Changement de code dû à un transfert de chef-lieu" ///
70"Transformation de commune associé en commune déléguée", replace 

label define reg_lab ///
1"Guadeloupe" 2"Martinique" 3"Guyane" 4"La Réunion" 6"Mayotte" ///
11"Île-de-France" 24"Centre-Val de Loire" 27"Bourgogne-Franche-Comté" ///
28"Normandie" 32"Hauts-de-France" 44"Grand Est" 52"Pays de la Loire" ///
53"Bretagne" 75"Nouvelle-Aquitaine" 76"Occitanie" ///
84"Auvergne-Rhône-Alpes" 93"Provence-Alpes-Côte d'Azur" 94"Corse", replace 

//administrative level labels 
global nat_lab National
global can_lab Canton
global dep_lab Departement
global dis_lab District 
global com_lab Commune
global reg_lab Regional

//month labels 
global mo1 "Jan"
global mo2 "Feb"
global mo3 "Mar"
global mo4 "Apr"
global mo5 "May"
global mo6 "Jun"
global mo7 "Jul"
global mo8 "Aug"
global mo9 "Sep"
global mo10 "Oct"
global mo11 "Nov"
global mo12 "Dec"

global col1 "99 151 183"
global col2 "192 221 231"
global col3 "239 195 98"
global col4 "194 25 29"
global sym1 o
global sym2 t
global sym3 s
global sym4 d

	
//axis label options 
global ylab_opts labsize(medium) angle(horizontal)
global xlab_opts labsize(medium) angle(45)

//last bit of a graph
global graph_scheme scheme(s1color) subtitle(,fcolor(white) ///
lcolor(bluishgray)) graphregion(color(white)) ///
plotregion(lcolor(bluishgray)) scale(1.2)

