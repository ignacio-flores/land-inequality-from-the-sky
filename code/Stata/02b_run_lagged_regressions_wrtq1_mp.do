//setup 
clear all  

// 0. General settings
global absorber i.ndept i.t
*global absorber i.canton##i.month
qui do "code/Stata/functions/aux_general.do" 

local wc pa28 // wi28 no28
local ext w
local shk s 

local fp = 11 //0
local lp = 33 //45
 
//loop over types 
foreach type in foods /*$tps*/ {
	
	*loop over shock-types 
	foreach c in the_shock_`shk' {
		
		* Plot effects by quintile
		foreach v in gini_can $mainvars {
			 	
			*chose dependent variables 
			foreach depvar in cum_gpp_`ext' agpp_`ext' {
				
				*open data and prepare 
				qui use "data/FR/multi_panel_foods_`wc'.dta", replace
				
				local sho = substr("`depvar'", 1, 3)
				cap drop canton 
				qui encode adm_can, gen(canton)
				qui encode dept, gen(ndept)
				
				*qui drop if nfarms < 30 | area_agri_farms < .1
				
				*DEFINE SHOCK AS FIRST YEARLY OCCURRENCE
				/*
				bysort canton year: gen sum_hshock_avg = sum(hshock_avg)
				bysort canton year: egen min_hshock_avg = ///
					min(sum_hshock_avg) if sum_hshock_avg != 0	
				qui drop the_shock_s
				qui gen the_shock_s = 0 
				qui replace the_shock_s = 1 if ///
					(sum_hshock_avg == min_hshock_avg) & missing(min_hshock_avg[_n-1])
				qui drop min_hshock_avg sum_hshock_avg
				*/
				
				*DEFINE SHOCK AS AT LEAST 1 DAY ABOVE THR
				qui drop the_shock_s 
				qui gen the_shock_s = 0 
				qui replace the_shock_s = 1 if hshock_avg >= 1
				cap drop the_shock_s_y
				bys adm_can year: egen the_shock_s_y = max(the_shock_s)
				qui so canton t 
				
				*define asinh if necessary 
				if inlist("`depvar'", "agpp_`ext'", ///
					"acum_gpp_`ext'", "cum_gpp_`ext'") {
					qui gen agpp_`ext' = asinh(gpp_`ext')
					qui byso canton year: gen cum_gpp_`ext' = ///
						sum(gpp_`ext') if inrange(p, `fp', `lp')
					qui gen acum_gpp_`ext' = ln(cum_gpp_`ext')
				}
				qui so canton t 
				
				*gen period 
				local ppp 2
				cap drop aux1 
				qui gen aux1 = !mod(p, `ppp') 
				cap drop per
				qui gen per = p + aux1 
				qui replace per = round(per/`ppp')
				qui drop aux1 
				
				*build quintiles 
				cap drop quant_`v'
				cap drop q_`v'*
				qui xtile quant_`v' = `v' if year == 2015, nq(${q})
				bysort canton: ereplace quant_`v' = max(quant_`v') 
				
				//set panel 
				qui sort canton t	
				xtset canton t
				
				global for 8
				global lag 8	
				loc i = 1	
				
				local lv `c'
				
				////////////////////DEFINE REGRESSION/////////////////////////
				eststo paraqall: reghdfe `depvar' /*(l(8/1)gpp_w)`c' */ ///
					(f(${for}/0)`c' `c' l(1/${lag})`c')##b1.quant_`v' /// 
					 c.hshock_avg##c.hu_q c.ff_q c.pe_q  ///
					, absorb(${absorber}) vce(cl canton)
				//////////////////////////////////////////////////////////////

				*store coefficients 
				esttab paraqall, se keep( ///
					1F*.`c'#*.quant_`v' ///
					1L*.`c'#*.quant_`v' ///
					1.`c'#*.quant_`v')
				mat b = r(coefs)
				
				*input in database 
				local rownames : rowfullnames b
				local rows : word count `rownames'
				clear 
				svmat b 
				qui gen rownames = ""
				forvalues i = 1/`rows' {
					qui replace rownames = "`:word `i' of `rownames''" in `i'
				}
				
				*relabel 
				qui split rownames, parse(.)
				qui drop rownames
				qui drop rownames3
				
				*cleaning 
				qui replace rownames1 = substr(rownames1, 2, .)
				qui replace rownames1 = subinstr(rownames1, "F", "-", .)
				qui replace rownames1 = "1" if rownames1 == "L"
				qui replace rownames1 = subinstr(rownames1, "L", "", .)
				qui replace rownames1 = "-1" if rownames1 == "-"
				qui replace rownames1 = "0" if rownames1 == ""
				qui destring rownames1, replace 
				qui rename rownames1 period 	
				qui replace rownames2 = ///
					subinstr(rownames2, "the_shock_`shk'#", "", .)
				qui replace rownames2 = ///
					subinstr(rownames2, "hot30#", "", .)
				qui replace rownames2 = subinstr(rownames2, "bn", "", .)	
				qui destring rownames2, replace 
				qui rename rownames2 Q
				
				qui replace period = period * 8
				*add gaps 
				qui replace period = period + 1.5 if Q == 2
				qui replace period = period + 3 if Q == 3
				qui replace period = period + 4.5 if Q == 4
				qui replace period = period + 6 if Q == 5			
				local freq_gph days
				local xmax 100
				local xcond -64(16)64				
				
				*compute 
				qui gen ci_low = b1 - b2 * 1.96 
				qui gen ci_up = b1 + b2 * 1.96 
				
				forvalues x = 1/$q {
			
					*quantile labels 
					local xql_`v'_`c'_`type' ///
						`xql_`v'_`c'_`type'' `x' "${quant}`x'"
					
					*graph lines 
					local z = `x' * 1/$q
					local col black*`z'				
					if (`x' == $q) {
						local col red
					}
					
					local cfs`v'`c'`type'`sho' ///
						`cfs`v'`c'`type'`sho''  ///
						(scatter b1 period if Q == `x', ///
						mcolor(`col') msize(vsmall) msymbol(o)) 
					
					local ci`v'`c'`type'`sho' `ci`v'`c'`type'`sho'' ///
						(pccapsym ci_up period ci_low period ///
						if Q == `x', ///
						color(`col') lwidth(thin) lpattern(shortdash) ///
						msymbol(i) msize(vsmall)) 
					
			
					*legend last graph 
					local l`v'`type'`c'`sho' `l`v'`type'`c'`sho'' ///
					`x' "${quant}`x'" 
				}
				
				local space "            "
				graph twoway `cfs`v'`c'`type'`sho'' ///
					`ci`v'`c'`type'`sho'' ///
					 if period <= `xmax' ///
					, yline(0, lcolor(gs0) lpattern(dot)) ///
					xline(0, lcolor(black) lpattern(dot)) ///
					ylab(`ycond', angle(horizontal)) /// 
					xlab(`xcond', angle(45)) ///
					ytitle("Temperature shock on" "${lab_`depvar'}") ///
					xtitle("{&larr} `freq_gph' before`space'Shock`space' `freq_gph' after {&rarr}") ///
					legend(col(1) subtitle("${quant2} of ${lab_`v'}") ///
					order(`l`v'`type'`c'`sho'') ///
					row(${nr}) ring(1) position(6) symxsize(5pt)) ///
					${graph_scheme}	note("${`c'_condition}, type: `type' " ///
					"absorb(${absorber})") 
				local gname `wc'_lagrq1_`depvar'q${q}_of_`v'_`type'_`c'_${abs}.png
				exit 1
				qui graph export "figures/shock/lag_wrtq1/`gname'", replace 
			}
		}	
	}
}
