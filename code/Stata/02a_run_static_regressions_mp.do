
//setup 
clear all  

// 0. General settings
global absorber i.canton i.t 
*global absorber i.canton##i.month
qui do "code/Stata/functions/aux_general.do" 

local ext w
local shk s
 
//loop over types 
foreach type in foods /*$tps*/ {
	
	*loop over shock-types
	foreach c in the_shock_`shk' {
		
		foreach ver in /*v1 v2*/ v3 {
				
				foreach depvar in gpp_`ext' agpp_`ext'  {
							
				*open database 
				qui use "data/FR/multi_panel_foods_${`ver'}.dta", replace 
				
				*define canton 
				cap drop canton 
				qui encode adm_can, gen(canton)
				qui so canton t 
				
				*define log 
				qui gen agpp_`ext' = asinh(gpp_`ext')
				
				*define asinh if necessary 
				if inlist("`depvar'", "acum_gpp_`ext'", "cum_gpp_`ext'") {
					qui byso canton year: gen cum_gpp_`ext' = ///
						sum(gpp_`ext') if inrange(p, 10, 32) 		
				}
				sort canton year date 
				
				//effects by quantile
				foreach v in $mainvars {
					
					qui so canton year 
					
					*build quintiles 
					cap drop quant_`v'
					cap drop q_`v'*
					qui xtile quant_`v' = `v' if year == 2015, nq(${q})
					bysort canton: ereplace quant_`v' = max(quant_`v') 
					qui tab quant_`v', gen(q_`v')
					qui tab1 q_`v'*
					
					*define treatment by quintile 
					foreach var of var q_`v'* {
						cap drop t`c'_`var'
						qui gen t`c'_`var' = `c' * `var'
					}
				} 
				
				*set panel 	
				xtset canton t 
			
				* Plot effects by quintile
				foreach v in $mainvars {
					
					if inlist("`depvar'", "acum", "agpp") {
						local coor_shock = .2
					}
					if inlist("`depvar'" , "gpp") {
						local coor_shock = .01
					}
					
					*prepare some locals for graphs 
					local iter = 1 
					forvalues x = 1/$q {
					
						*quantile labels 
						local xql_`v'_`c'_`type' ///
							`xql_`v'_`c'_`type'' `x' "${quant}`x'"
						if `iter' != 1 {
							local tester `tester' = t`c'_q_`v'`x'
						}
						else {
							local tester t`c'_q_`v'`x'
							local iter = 0 
						}
						*graph lines 
						local z = `x' * 1/$q
						local coefs_`v'_`depvar'_`type' ///
							(paraq`x', lcolor(black*`z') lwidth(thick) ///
							ciopts(lcolor(black*`z'))) ///
							`coefs_`v'_`depvar'_`type'' 
							
						*legend last graph 
						local legn = 2*$q - (2*`x'-2)
						local l_`v'_`type'_`c' `l_`v'_`type'_`c'' ///
						`legn' "${quant}`x'"
					}
					
					qui sort canton year 

					*impact on productivity
					eststo `c': reghdfe `depvar' t`c'_q_`v'* q_`v'* ///
						, absorb(${absorber}) ///
						vce(cl canton)
					
					local ylb 
					if ("`v'" == "gini_can" & "`depvar'" == "agpp") {
						*local ylb 0(-0.1)-0.5
					} 
					
					*global effect 
					coefplot `c', keep(t`c'_q_`v'*) ///
						yline(0, lcolor(gs0) lpattern(dot)) ///
						vertical mfcolor(white) mcolor(dkgreen) offset(0) ///
						ciopts(lcolor(dkgreen)) ylab(`ylb', ///
						grid angle(horizontal)) ///
						ytitle("Shock on ${lab_`depvar'}") ///
						xtitle("${quant2} of ${lab_`v'}") ///
						xlab(`xql_`v'_`c'_`type'', angle(45)) ///
						note(/*"p-val Same effect by quantile: `=p_quintiles'."*/ ///
						"${`c'_condition}. absorb(${absorber})") $graph_scheme 	
					local gname ///
						`depvar'_impact_by_q${q}_of_`v'_`type'_`c'_${abs}_${`ver'}.png
					qui graph export "figures/shock/static/`gname'", replace 
				}
			}
		}	
	}
}
