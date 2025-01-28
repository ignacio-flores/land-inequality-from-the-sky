//setup 
clear all 

//0. General settings
quietly do "code/Stata/functions/aux_general.do" 

local wc pa28 // wi28 no28

foreach v in cum_gpp_w tsup_h_q {
	foreach n in the_shock_s /*$hots*/ {
		foreach ver in /*v1 v2*/ v3 {
			
			//open data 
			qui use "data/FR/multi_panel_foods_`wc'.dta", replace 
			
			cap drop canton 
			qui encode adm_can, gen(canton)
			qui drop if nfarms < 30 | area_agri_farms < .1
			cap drop the_shock_s_y
			bys adm_can year: egen the_shock_s_y = max(the_shock_s)
			
			cap drop cum_gpp_`w
			qui byso canton year: gen cum_gpp_w = sum(gpp_w) 
			
			*qui drop if year < 2015
			if "`type'" == "canton" & "`n'" == "the_shock" continue

			//define ps for years with and without shock 
			cap drop m_*
			qui tab p, gen(m_)
			qui sort canton year p
			forvalues k = 1/12 {
				cap drop mt_`k'
				qui gen mt_`k' = `n'_y * m_`k'
			}
			
			//save for later 
			tempfile main 
			qui save `main'

			*multi binscatter 
			local iter = 1
			tempfile tf_`type'_`n'  
				
			//loop over hot or not 
			foreach opt in `n' not {
				
				//define conditions 
				if "`opt'" == "`n'" local cond if `n'_y == 1 
				if "`opt'" == "not" local cond if `n'_y == 0 
				
				qui use `main', clear 

				binscatter `v' p `cond', ///
					n(45) xtitle("p") ytitle("`v'") ///
					line(connect) scheme(plotplainblind) ///
					absorb(canton) savedata(`opt') replace
					
				preserve 
					clear 
					do `opt'
					qui rename `v' `v'_`opt'
					qui replace p = int(p)
					if `iter' != 1 {
						qui merge 1:1 p using `tf_`type'_`n'', nogen
					}  
					qui save `tf_`type'_`n'', replace
					local iter = 0
				restore
			}
			
			qui use `tf_`type'_`n'', clear 

			
			if "`v'" == "cum_gpp_w" {
				//prepare text for graph 
				di as result "v_not : ``v'_not'; v_n: ``v'_`n''"
				qui gen diff = (`v'_not - `v'_`n')
				qui sum diff if p == 33 
				local dif45 = r(mean)
				qui sum `v'_not if p == 33 
				local not45 = r(mean)
				local txt = round((`dif45' / `not45') * 100, 0.1)
				*local txt = subinstr("`txt'", " ", "", .)
				local txt_coord .35 210
				local txt_unit %
				local yl 0(.2)1.2
				*local ldiff (line diff p, lcolor(black) lpattern(dot))
				local lgtxt 1 "Shock-years" 1 "Non-Shock-years" 
				local gap "Growing" "season" "gap"
				cap drop upper 
				qui gen upper = 1.2
				qui replace p = p * 8 + 8
			} 
			if "`v'" == "temp" {
				//prepare text for graph 
				qui sum `v'_`n' if inrange(p, 11, 33) 
				local hot12 = r(mean)
				qui sum `v'_not if inrange(p, 11, 33)
				local not12 = r(mean)
				local txt = `hot12' - `not12'
				local txt_coord 10 22
				local txt_unit ºC
				local yl 0(5)35
				local ldiff 
				local lgtxt 2 "Shock-years" 3 "Non-Shock-years" 
				local gap Summer gap 
				cap drop upper 
				qui gen upper = 35
				qui replace p = p * 8 + 8
			} 
			local txt : display %9.1f `txt'
			
			*local barcall (bar upper p if inrange(p, 88, 264), ///
				*bcolor(gs10) base(0) barwidth(8))
			
			
			graph twoway ///
				(connected `v'_`n' p, lcolor(cranberry) msize(vsmall) ///
				mcolor(cranberry)) (connected `v'_not p, ///
				lcolor(black) msize(vsmall) mcolor(black)) , ///
				xline(88, lcolor(black) lpattern(dot)) ///
				xline(264, lcolor(black) lpattern(dot)) ///
				ytitle("${lab_`v'}") xtit("Day of the year") ///
				ylab(`yl', grid angle(horizontal)) xlab(0(30)365, grid) ///
				text(`txt_coord' "`gap':" "`txt'`txt_unit'") ///
				legend(order(`lgtxt')) xtit("Day of the year") $graph_scheme
				
				*note("Includes canton fixed effects. Shock years contain at least one ocurrence within the year") 
		
			qui graph export ///
				"figures/shock/foods_`n'_or_not_yr_`v'_${`ver'}.png", replace
			
		}				
	}	
}


