//setup 
clear all 

// 0. General settings
global absorber i.canton i.p 
*global absorber i.canton##i.month
qui do "code/Stata/functions/aux_general.do"  

local extgpp w 
local ext w
local shk s

local fp = 11 //0
local lp = 33 //45

//loop over types 
foreach v in cum_gpp_`extgpp'  {	
	
	local yvar `v'
	if "`yvar'" == "acum_gpp" local ylb .1(.1)-.3
	if "`yvar'" == "cum_gpp" local ylb .15(.05)-.15
	
	foreach type in foods {
		foreach n in the_shock_`shk'  {

			*open data and prepare 
			qui use "data/FR/multi_panel_foods_pa28.dta", replace 			
			
			cap drop canton 
			qui encode adm_can, gen(canton)
			
			*REPLACE SHOCK 
			qui drop the_shock_s 
			qui gen the_shock_s = 0 
			qui replace the_shock_s = 1 if hshock_avg >= 1
			cap drop the_shock_s_y
			
			bys adm_can year: egen the_shock_s_y = max(the_shock_s)
			bys adm_can year: egen totdays_y = total(hshock_avg)
			qui sort canton t 
			
			*gen period 
			local ppp 2
			cap drop aux1 
			qui gen aux1 = !mod(p, `ppp') 
			cap drop per
			qui gen per = p + aux1 
			qui replace per = round(per/`ppp')
			qui drop aux1 
			
			*define asinh if necessary 
			if inlist("`v'", "agpp_`extgpp'", ///
				"acum_gpp_`extgpp'", "cum_gpp_`extgpp'") {
				cap drop agpp_`extgpp'
				qui gen agpp_`extgpp' = asinh(gpp_`extgpp')
				cap drop cum_gpp_`extgpp'
				qui byso canton year: gen cum_gpp_`extgpp' = sum(gpp_`extgpp') if ///
					inrange(p, `fp', `lp')
				cap drop acum_gpp_`extgpp'
				qui gen acum_gpp_`extgpp' = asinh(cum_gpp_`extgpp')
			}		
			
				
			//define treated year-period variable 
			local period p //month 
			cap drop m_*
			qui tab `period', gen(m_)
			qui sort canton year `period'
			qui sum `period' if !missing(`v')
			local maxp = r(max) 
			di as result "max: `maxp'"
			forvalues k = `fp'/`maxp' {
				cap drop mt_`k'
				qui gen mt_`k' = `n'_y * m_`k'
			}
			
		
			local mlabs 0 "Apr" 4 "May" 8 "June" 12 "Jul" 16 "Aug" 20 "Sep"

			// General effect over months 
			eststo yt: reghdfe `yvar' mt_* m_* , ///
				absorb(canton i.t) vce(cl canton)
			coefplot yt, yline(0) ///
				keep(mt_*) vertical ylab(`ylb', angle(horizontal)) ///
				$graph_scheme xlab(`mlabs') ytit("Impact on ${lab_`v'}") ///
				note("shock: ${`n'_condition}")	
			qui graph export ///
				"figures/shock/yearly/`yvar'_yr_glob_`type'_`n'.png", replace		
			local qn = 1
			
			//prepare lines second graph
			
			forvalues x = `fp'/`lp' {
				local lpm = `lp' - 1
				if (`x' <= `lpm') {
					local keepers `keepers' mt_`x' 
				}
				*local xx = `x' * 8
				 *if mod(`x', 5) == 0 {
				 *	local mlabs `mlabs' `x' "`xx'"
				 *}
			}
			
		
			foreach qv in gini_can $mainvars {
				
				*build quintiles 
				cap drop quant_`qv'
				cap drop q_`qv'*
				qui xtile quant_`qv' = `qv' if year == 2015, nq(${q})
				bysort canton: egen quant_`qv'2 = mean(quant_`qv') 
				qui replace quant_`qv' = quant_`qv'2 if missing(quant_`qv')
				qui drop quant_`qv'2
				qui tab quant_`qv', gen(q_`qv')
				qui tab1 q_`qv'*
				
				forvalues qqq = 1/$q {
					*regress over quintiles 
					local z = `qqq' * 1/$q
					eststo yt`qqq'`qv', prefix(n`qqq'): ///
						reghdfe `yvar' mt_* totdays_y if q_`qv'`qqq' == 1, ///
						absorb(${absorber}) vce(cl canton)
					*prepare lines for graph 	
					local c`qv'`n'`v' `c`qv'`n'`v'' ///
						(yt`qqq'`qv', lcolor(black*`z') ///
						lwidth(thick) ciopts(lcolor(black*`z'))) 
					*legend last graph
					local qn = `qqq' * 2
					di as result "qn: `qn'"
					local l`n'`v'`qv' ///
						`l`n'`v'`qv'' `qn' "${quant}`qqq'"
				}
				*plot 
				coefplot `c`qv'`n'`v'' ||, ///
					yline(0, lpattern(dot) lcolor(black)) ///
					/*xline(11.3,lpattern(dot) lcolor(black)) ///
					xline(22.7,lpattern(dot) lcolor(black)) ///
					xline(34,lpattern(dot) lcolor(black)) */ /// 
					keep(`keepers') vertical ylab(, angle(horizontal)) ///
					$graph_scheme xlab(`mlabs') ylab(`ylb') /// 	
					recast(line) /*offset(0)*/ legend(col(1) ///
					subtitle("${quant2} of ${lab_`qv'}") ///
					order(`l`n'`v'`qv'') row(${nr}) ///
					ring(1) position(6) symxsize(5pt)) ///
					ytit("Shock on ${lab_`v'}") ///
					note("${`n'_condition}. `type', ${absorber}")	
				exit 1	
				local gname `yvar'_yr_byQ`qv'_`type'_`n'_${abs}.png
				qui graph export "figures/shock/yearly/`gname'", replace	
			}	
		}
	}	
}




