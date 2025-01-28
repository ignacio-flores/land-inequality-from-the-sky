//setup 
clear all 
*ssc install did_multiplegt eststo mylabels binscatter reghdfe ftools estout coefplot mylabels 

// 0. General settings
cap cd "~/Dropbox/land_ineq_degradation/"
do "code/Stata/functions/aux_general.do" 

foreach type in $tps {
	
	foreach ver in /*v1 v2*/ v3 {
		*open data 
		*qui use  "data/FR/${`ver'_f}/working_panel_month_`type'_${`ver'}.dta", replace
		*open database 
			qui use  "data/FR/${`ver'_f}/working_panel_month_`type'_${`ver'}.dta", replace
		
		*set panel 	
		cap drop canton 
		qui encode adm_can, gen(canton)
		xtset canton t
		local ineqvar "gini_can"
		
		exit 1

		// 1. Study CEF of quintiles -----------------------------------------------
		
		*make room for aux files 
		local fold figures/concentration/temp
		shell rm -rf ~/Dropbox/land_ineq_degradation/`fold'
		*qui mkdir "`fold'"
		local varl /*dct_akm2 dct_km2 */ fsize dcount dherf 
		
		//binscatter individually 
		foreach v in `ineqvar' `varl' {
			
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
				cap drop tthe_shock_`qvar'
				qui gen tthe_shock_`qvar' = the_shock * `qvar'
			}
			
			if "`v'" != "`ineqvar'" {
				binscatter quant_`v' quant_`ineqvar', ///
					linetype(none) xtitle("${lab_`ineqvar'}") ///
					ytitle("Average quitile: ${lab_`v'}") ///
					savedata("`fold'/`type'_quant_`v'") ///
					discrete //absorb(canton)
			}
		}  
		
		//join them all 
		tempfile tf
		local iter = 1 
		foreach v in `varl' {
			
			//color 
			if "`v'" == "dcount" local mlc "green"
			if "`v'" == "dherf" local mlc "gold"
			if "`v'" == "dct_akm2" local mlc "blue"
			if "`v'" == "dct_km2" local mlc "red"
			
			
			//import 
			qui import delimited "`fold'/`type'_quant_`v'", clear 
			qui rename quant_`v' avg_quant
			qui gen variable = "`v'"
			qui order variable avg_quant 
			
			//prepare graph 
			local lgf_`ver' `lgf_`ver'' (connected avg_quant quant_`ineqvar' ///
				if variable == "`v'", mcolor(`mlc') lcolor(`mlc'))
			bysort variable: egen max = max(avg_quant)
			local ttt_`ver' `ttt_`ver'' (scatter avg_quant quant_`ineqvar' ///
				if variable == "`v'" & max == 1, ///
				mlabel(variable) mlabcolor(`mlc') ///
				msymbol(none) mlabposition(11))
			qui replace max = 0 if max != avg_quant
			qui replace max = 1 if max != 0
			
			//save 
			if `iter' != 1 append using `tf'
			qui save `tf', replace 
			qui local iter = `iter' + 1 
		}
		
		//graph 
		qui use `tf', clear 
		graph twoway `lgf_`ver'' `ttt_`ver'', legend(off) ///
			$graph_scheme ///
			xtit("${lab_`ineqvar'}") ytit("Average quantile")  ///
			xlabel( 1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5") ///
			note("type: `type'") /*ylab(1(.5)3)*/ 
		qui graph export ///
			"figures/concentration/bin_`ineqvar'-bio_`type'_${`ver'}.png", replace	
		exit 1	
	}	
}
