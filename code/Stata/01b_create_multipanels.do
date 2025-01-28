//setup 
clear all 

// 0. General settings
qui do "code/Stata/functions/aux_general.do"

local wc pa28
*local wc no28
*local wc wi28

qui import delimited "data/PRA/matched_can_pra.csv", clear 
qui rename gid_4 adm_can
tempfile tf_racan 
qui save `tf_racan'

*get canton data
qui import dbase ///
	"georeferenced_data/gadm/gadm36_FRA_shp/gadm36_FRA_4.dbf", clear
qui keep GID_4 NAME_1 NAME_2
qui rename (GID_4 NAME_1 NAME_2) (adm_can region dept)  	
tempfile tfgadm
qui save `tfgadm'

*get diversification data 
qui import delimited ///
	"georeferenced_data/cadastre/FR/metadata/diversification_ratios.csv", ///
	clear 
cap drop v1 	
foreach d in wavg_var pfolio_var dfct_r {
	qui replace `d' = "." if `d' == "NA" 
	qui destring `d', replace 
}
qui rename gid_4 adm_can 
tempfile dratios 
qui save `dratios'

*loop over data types 
foreach type in farms foods {
	
	tempfile tf_`type'
	
	if "`wc'" == "no28" local wc2 _no_c28
	if "`wc'" == "pa28" local wc2 _part_c28
	if "`wc'" == "wi28" local wc2 _with_c28
	
	//extract farms data 
	if ("`type'" == "farms") {
		*report activity
		display as result "cleaning `freq' `type'"
		qui use  "data/FR/`wc2'/working_panel_month_`type'_`wc'.dta", clear
		local renamer ///
			crop1 crop18 crop19 crop28 area_agri canton_area ///
			farm_area_est name_4 gpp gini_can dcount ccount ///
			dherf herfindahl cv 
		qui keep if month == 1
		qui keep adm_can year `renamer'
		foreach v in `renamer' {
			rename `v' `v'_farms
		}
		qui save `tf_`type''
	}
	
	//merge foods' monthly and weekly 
	if ("`type'" == "foods") {
		foreach freq in _month _8days {
			
			di as result "`type' - `freq'"
			
			*report activity
			qui display as result "cleaning `freq' `type'"
			qui use  "data/FR/`wc2'/working_panel`freq'_`type'_`wc'.dta", clear
			
			*list overlaping 
			local overlap ///
				gpp temp the_shock the_shock_y canton_area name_4
						
			*merge
			if ("`freq'" == "_8days") {
				local x "w"
				
				*label safran variables 
				qui ds *_q
				foreach sv in `r(varlist)' {
					qui label var `sv' "${lab_`sv'}" 
				}
				
				local safran tsup_h_q *_q t* hshock*
				keep type adm_can year month date `overlap' `safran'
				
				foreach v in `overlap' {
					di as text "  - `v' --> `v'_`x'"
					rename `v' `v'_`x'
				}

				qui merge m:1 type adm_can year month using `tf_`type'', ///
					gen(merge_wm)			
				sort adm_can year month date 
				order adm_can year month date gpp_* temp* `safran' ///
					the_shock* the_shock_y*
			}
			if ("`freq'" == "_month") {
				local x "m"
				foreach v in `overlap' {
					di as text "  - `v' --> `v'_`x'"
					qui rename `v' `v'_`x'
				}
			}	
			
			*save 
			qui save `tf_`type'', replace 
		}
		
		*merge foods and farms 
		qui use `tf_foods', clear 
		qui merge m:1 adm_can year using `tf_farms', gen(merge_ff)
		sort adm_can year month date 	
		
		*convert seminatural to % of foods area  
		cap drop prairies 
		qui egen prairies = rowtotal(crop18_farms crop19_farms) 
		qui replace prairies = prairies * farm_area_est_farms / farm_area_est
		qui gen crop28_trans = crop28 * farm_area_est_farms / farm_area_est
		
		*redefine p and t 
		qui drop t 
		qui byso adm_can: gen t = _n
		qui byso adm_can year: gen p = _n - 1
		sort adm_can year month date
		
		*clean 
		qui drop merg* hot*
		qui drop if missing(gini_can)
		qui gen test_name = 1 if (name_4_w == name_4_m & name_4_m == name_4_farms)
		
		*check for inconsistencies 
		assert test_name == 1
		qui drop test_name 	
		qui gen test_carea = floor(canton_area_w) == floor(canton_area_m) & ///
			floor(canton_area_w) & floor(canton_area_farms)
		qui replace test_carea = round(canton_area_w) == round(canton_area_m) & ///
			round(canton_area_w) & round(canton_area_farms)	if test_carea != 1
		qui assert test_carea == 1
		*qui drop canton_area_m canton_area_w
		*qui rename canton_area_farms canton_area
		
		*add diversification ratios 
		qui merge m:1 adm_can year using `dratios', keep(3) nogen 
		
		//define seasons 
		cap drop season
		qui gen season=1 if month>0 & month<=3
		qui replace season=2 if month>3 & month<=6
		qui replace season=3 if month>6 & month<=9
		qui replace season=4 if month>9
		qui so adm_can t	
		
		//rename farm variables
		qui rename gini_can_farms gini_f
		qui rename herfindahl_farms cherf_f 
		qui rename (dcount ccount dherf cv ) /// 
			(dcount_o ccount_o dherf_o cv_o)
 		foreach v in  dcount ccount dherf cv {
			qui rename `v'_farms `v'_f
		}
		
		* create seminatural areas 
		qui egen seminat = rowtotal(crop18_farms crop19_farms crop28_farms)

		*large farms 
		qui egen lfarms = rowtotal(large_farm_pt vlarge_farm_pt)
		
		/*
		*generate adjusted temperatures 
		bysort adm_can year month: egen temp_w_mea = mean(temp_w)
		qui gen temp_w_fac = temp_w / temp_w_mea 
		qui gen temp_w_adj = temp_w_fac * temp_m 
		qui drop temp_w_fac temp_w_mea 
		qui sort adm_can t
		
		*redefine as first adjusted temperature above thr if shock month
		qui gen sho = temp_w_adj >= wshock & the_shock_m == 1
		qui gen sho2 = sho * t 
		qui replace sho2 = . if sho2 == 0
		bysort adm_can year month: egen min_sho = min(sho2) 	
		cap drop the_shock 
		qui gen the_shock_x = 0 
		qui replace the_shock_x = 1 if sho2 == min_sho & sho == 1
		*/
		
		qui gen the_shock_s = tsup_h_q >= wshock
		bys adm_can year: egen the_shock_s_y = max(the_shock_s)
		
		qui merge m:1 adm_can using `tfgadm', keep(1 3) nogen	
		
		//label variables 
		qui ds 
		foreach v in `r(varlist)' {
			qui label var `v' "${lab_`v'}"
			if "${`v'_condition}" != "" {
				qui label var `v' "${`v'_condition}"
			}
		}
		//
		qui drop type div_match* test_carea crop28_trans 
		
		qui merge m:1 adm_can using `tf_racan', keep(3) nogen
	
		*save 
		qui save "data/FR/multi_panel_foods_`wc'.dta", replace 
		
	}
}	
