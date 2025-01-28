//setup 
clear all 

// 0. General settings
qui do "code/Stata/functions/aux_general.do" 

foreach freq in _month _8days {
	
	*loop over data types 
	foreach type in $tps /*canton*/ {
		
		foreach ver in /*v1 v2*/ v3 {
			
			*report activity
			display as result "cleaning `freq' `type' (${`ver'})"

			qui import delimited ///
				"data/FR/${`ver'_f}/working_panel`freq'_all_${`ver'}.csv", ///
				clear 
			cap drop v1
			
			// 0. Data cleaning ---------------------------------------
				
			*keep only relevant rows 
			qui keep if type == "`type'"
			if "`freq'" == "_8days" & "`type'" == "canton" continue

			*destring 
			qui ds type year gid_4 name_4, not 
			local numvars `r(varlist)' 
			foreach v in `numvars' {
				cap replace `v' = "." if `v' == "NA"
				cap destring `v', replace 
			}
			qui drop if nfarms <= 30
			
			*define panel dimensions 
			qui rename (gid_4) (adm_can) 
			qui sort adm_can year month
			if "`freq'" == "_8days" {
				cap drop p per
				sort adm_can date
				qui byso adm_can year: gen p = _n - 1
				qui egen per = cut(p), at(0(4)48)
				qui replace per = (per / 4) + 1
				sort adm_can date
			}
			qui byso adm_can: gen t = _n
			
			*define farm-related info
			if !inlist("`type'", "canton") {
				
				*define some main variables 
				qui gen area_agri = farm_area_est / canton_area 
				qui rename (gini herfindhal) (gini_can herfindahl)
				qui gen fsize = farm_area_est / nfarms 
				
				*crop count by size 
				cap drop dct_fsize 
				qui gen dct_fsize = dcount / fsize 
				
				//create max_crop 
				cap drop max_crop
				egen max_crop = rowmax(crop*)
				
				*create dcount by km2 
				cap drop dct_km2
				gen dct_km2 = dcount / canton_area
				cap drop dct_akm2
				gen dct_akm2 = dcount / farm_area_est

				* Top code cantons with more than 100% agri_area
				qui sum area_agri, d
				qui replace area_agri = r(p99) ///
					if area_agri > r(p99) & !missing(area_agri)

				*define quantiles of agricultural area 
				global quants 10
				qui byso adm_can: egen ref_area=min(area_agri)
				qui xtile qagri = ref_area, nq(${quants})

			}	
			
			*set panel 
			qui sort adm_can t 
			
			// 1. Define temperature schocks on gpp -----------
	
			//setup 
			qui gen the_shock = 0 
			qui replace the_shock = 1 if temp >= wshock
			bys adm_can year: egen the_shock_y = max(the_shock)
			
			foreach cond in $hots {
				cap drop `cond' 
				qui gen `cond' = ${`cond'_condition}
				
				*identify years with shock 
				bys adm_can year: egen `cond'_y = max(`cond')
			}	

			// 2. Productivity and Temperature  --------------

			*graph gpp vs temp
			/*
			loc yl 0(.05).2
			if "`freq'" == "_8days" loc yl 0(.02).08
			binscatter ///
				gpp temp, ///
				line(connect) ytitle("${lab_gpp}") ///
				xtitle("${lab_temp}") ytit("${lab_gpp}") ///
				ylab(, angle(horizontal)) xlab(-5(5)35) n(100) ///
				mcolor(dkgreen) lcolor(dkgreen) ylab(`yl') ///
				xline(30, lpattern(dot) lcolor(black)) ///
				$graph_scheme 
			qui graph export ///
				"figures/temperatures/bins_gpp_temp`freq'_`type'_${`ver'}.png", ///
				replace
			*/	
			
			//remove empty variables if necessary 
			qui ds type adm_can name_4, not
			foreach c in `r(varlist)' {
				qui sum `c', meanonly 
				if r(mean) == . {
					di as result "`c' is empty, removing variable"
					if "`c'" != "date" qui drop `c'
				}
			} 

			qui order type adm_can name_4 t year month gpp *gpp temp 
			if "`freq'" == "_8days" {
				qui order type adm_can name_4 date per t year month ///
					gpp *gpp temp
			}
			cap drop r_agpp _reghdfe*
			
			//label crops 
			forvalues c = 1/99 {
				cap confirm variable crop`c', exact
				if "${crop_lab_`c'}" != "" & _rc == 0 {
					qui la var crop`c' " ${crop_lab_`c'} "
				}
			}

			//save 
			qui save  ///
				"data/FR/${`ver'_f}/working_panel`freq'_`type'_${`ver'}.dta", ///
				replace 
		}
	}
}

