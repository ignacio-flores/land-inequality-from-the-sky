//setup 
clear all 
*ssc install did_multiplegt eststo mylabels binscatter reghdfe ftools estout coefplot mylabels 

// 0. General settings
cap cd "~/Dropbox/land_ineq_degradation/"
global aux_part ""preliminary""
do "code/Stata/aux_general.do" 
global aux_part ""graph_basics""
quietly do "code/Stata/aux_general.do" 

foreach type in /*farms*/ foods {

	foreach ver in /*v1 v2*/ v3 {
		
		*open data and prepare 
		qui use "data/FR/multi_panel_foods_pa28.dta", replace 	
		
		cap drop canton 
		qui encode adm_can, gen(canton)
		cap drop crop*farms

		*Is inequality associated with types of agriculture?
		forvalues c = 1/28 {
			cap label var crop`c' "${crop_lab_`c'}"
		}
		
		
		/*
		foreach var in $mainvars  {
			eststo `var': areg `var'  i.t crop*, absorb(canton) vce(cl canton)
			
			coefplot ///
				`var', bylabel("${lab_`l'}") ||, ///
				keep(crop*)  horizontal xline(0, lcolor(black)) ///
				sort(,descending) mfcolor(white) mcolor(dkgreen) ///
				ciopts(lcolor(dkgreen)) ylab(, nogrid) ///
				msize(small) ///
				xtitle("Correlation with `var' (2WFE)") ///
				$graph_scheme
			qui graph export ///
				"figures/correlations/cropcorr_`var'_`type'_${`ver'}.png", replace	
		}

		//what correlates with quantiles?
		foreach v in $mainvars {
			
			if "`v'" == "gini_can" local v2 "gini"
			if "`v'" == "dcount" local v2 "dcount"
			
			local iter = 1 
			forvalues q = 1/$q {
				eststo `v2'`q': areg `v' i.t crop* if q_`v'`q', ///
				absorb(canton) vce(cl canton)
				if `iter' == 1 local cpl `v2'`q'
				if `iter' != 1 local cpl `cpl' || `v2'`q'
				local iter = 0 
			}
			coefplot `cpl' ||, ///
				keep(crop*) noci horizontal xline(0, lcolor(black)) ///
				sort(,descending) mfcolor(white) mcolor(dkgreen) ///
				ciopts(lcolor(dkgreen)) ylab(, nogrid labs(vsmall)) ///
				msize(small) ///
				xtitle("Correlation with crop types (two-way fixed effects)") ///
				$graph_scheme	
			qui graph export ///
				"figures/correlations/cropcorr_q${q}_`v'_`type'.png", replace 	

		}
		*/

		//average composition of quintiles 
		foreach v in gini_f /*$mainvars*/ {
			
			*build quintiles 
			cap drop quant_`v'
			cap drop q_`v'*
			qui xtile quant_`v' = `v' if year == 2015, nq(${q})
			bysort canton: egen quant_`v'2 = mean(quant_`v') 
			qui replace quant_`v' = quant_`v'2 if missing(quant_`v')
			qui drop quant_`v'2
			qui tab quant_`v', gen(q_`v')
			qui tab1 q_`v'*
			
			
			preserve 
				qui collapse (mean) crop*, by(quant_`v')
				local iter = 1 
				forvalues c = 1/28 {
					cap label var crop`c' "${crop_lab_`c'}"
					if _rc == 0 {
						local leglab_`v'_`type'_`ver' ///
							`leglab_`v'_`type'_`ver'' `iter' "${crop_lab_`c'}"
						if "${col_c`c'}" != "" {
							local bcolor_`v'_`ver' `bcolor_`v'_`ver'' ///
								bar(`iter', fcolor(${col_c`c'}) lwidth(none))
						}
						local iter = `iter' + 1
					}
				}	
				graph bar crop*, over(quant_`v') stack ///
					legend(order(`leglab_`v'_`type'_`ver'') ///
					size(vsmall) col(4) symxsize(3pt)) ///
					b1title("${quant2} of ${lab_`v'}") /// 
					ytit("Share of land, average") ylab(, angle(horizontal)) ///
					$graph_scheme `bcolor_`v'_`ver''
				qui graph export ///
					"figures/composition/q`q'`v'_cropcomp_`type'_${`ver'}.png", replace	
			restore 
		}
		
	}
}


