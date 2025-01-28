//setup 
clear all 

// 0. General settings
global aux_part ""preliminary""
qui do "code/Stata/aux_general.do" 
global aux_part ""graph_basics""
qui do "code/Stata/aux_general.do" 


foreach ver in /*v1 v2*/ v3 {
	
	*open database 
	qui use "data/FR/multi_panel_foods_pa28.dta", replace 
	cap drop canton 
	qui encode adm_can, gen(canton)
	qui drop if nfarms < 30 | area_agri_farms < .1
	xtset canton t

	local convar lfarms 
	local c the_shock_s
	
	foreach v in $mainvars {
		
		*build quintiles 
		cap drop quant_`v'
		cap drop q_`v'*
		qui xtile quant_`v' = `v' if year == 2015, nq(${q})
		bysort canton: egen quant_`v'2 = mean(quant_`v') 
		qui replace quant_`v' = quant_`v'2 if missing(quant_`v')
		qui drop quant_`v'2
		qui tab quant_`v', gen(q_`v')
		qui tab1 q_`v'*
		
		*effects by quantile
		foreach qvar of var q_`v'* {
			cap drop t`c'_`qvar'
			qui gen t`c'_`qvar' = `c' * `qvar'
		}
	}

	foreach depvar in gpp_w   {
		foreach biovar in $mainvars {
			
			qui replace `depvar' = . if !inrange(p, 10, 32)
			
			if inlist("`biovar'", "gini_can", "cv", "sd_logs") {
				continue
			} 
			
			*run regression 
			estimates clear
			eststo base_`biovar'_`depvar': reghdfe `depvar' ///
				(t`c'_q_`convar'*)##(q_`biovar'2 q_`biovar'3) ///
				if year > 2015, absorb(canton i.t) ///
				vce(cl canton)
			testparm 1.t`c'_q_`convar'*, equal
			estadd sca pval_id=round(r(p), .01)

			*what?
			forval g = 1/$q {
				est restore base_`biovar'_`depvar'
				lincomest 1.t`c'_q_`convar'`g'
				eststo `convar'`g'_`biovar'1
				forval d=2/${q} {
					est restore base_`biovar'_`depvar'
					lincomest 1.t`c'_q_`convar'`g' + ///
						1.t`c'_q_`convar'`g'#1.q_`biovar'`d'
					eststo `convar'`g'_`biovar'`d'
				}
				local cpl_`biovar'_`depvar'_`ver' ///
					`cpl_`biovar'_`depvar'_`ver'' ///
					( `convar'1_`biovar'`g', rename((1) = 1) ///
					\ `convar'2_`biovar'`g', rename((1) = 2) ///
					\ `convar'3_`biovar'`g', rename((1) = 3))
			}

			//plot 
			coefplot `cpl_`biovar'_`depvar'_`ver'', ///
				ytit("Impact on `depvar'") ///
				xtitle("Thirds of `convar'") ///
				vertical recast(line) $graph_scheme ///
				legend(row(1) order(1 "T1" 3 "T2" 5 "T3") ///
				subtitle("Thirds of ${lab_`biovar'}"))
			local gname impose_q${q}_`depvar'_`convar'_o_`biovar'_${`ver'}.png
			graph export "figures/shock/8days/impose/`gname'", replace 		
		}
	}
}




