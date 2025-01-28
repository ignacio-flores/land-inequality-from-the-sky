//setup 
clear all 

//0. General settings
cap cd "~/Dropbox/land_ineq_degradation/"
global aux_part ""preliminary""
do "code/Stata/aux_general.do" 
global aux_part ""graph_basics""
quietly do "code/Stata/aux_general.do" 

*make room for aux files 
local fold figures/correlations/temp
shell rm -rf ~/Dropbox/land_ineq_degradation/`fold'
qui mkdir "`fold'"
local biovar dherf
local varl `biovar' gini_can
local xvari nfarms 

foreach type in  foods /*farms*/ {
	
	*parameters 
	local limit = 10 
	local ngps = 30
	local col edkblue
	
	foreach ver in /*v1 v2*/ v3 {
				
		//open and prepare  data 
		qui use  "data/FR/${`ver'_f}/working_panel_month_`type'_${`ver'}.dta", replace
		qui drop if nfarms < 30 | area_agri < .1
		qui replace gini_can = gini_can * 100
		qui gen ln_`biovar' = ln(`biovar')
		qui keep if month == 12
		
		cap drop canton 
		qui encode adm_can, gen(canton)
		
		//delete weird depts in bordeaux?
		qui decode canton, gen(canton_str)
		qui gen gid_2 = substr(canton_str, 1, 10)
		drop if inlist(gid_2, "FRA.10.7.1","FRA.10.9.1")
		cap drop canton_str, gid_2
		
		//graph expected values 
		/*
		binsreg `biovar' gini_can , name(binsreg) cb(T)  ///
			ylab(, angle(horizontal)) ///
			ytit("${lab_gini_can}") xtit("${lab_`biovar'}") ///
			$graph_scheme	
		*/	
		
		binscatter gini `biovar' if `xvari' > `limit', n(`ngps') ///
			linetype(none) mcolor(`col') lcolor(`col') msymbols(Oh) ///
			ylab(, angle(horizontal)) ///
			ytit("${lab_gini_can}") xtit("${lab_`biovar'}") ///
			$graph_scheme
		qui graph export ///
			"figures/concentration/bin_gini_`biovar'_`type'_${`ver'}.png", replace
		

				
		//report pairwise correlation 		
		pwcorr `biovar' gini if `xvari' > `limit', star(.01)
		
		//save data with expected values with respect to `xvari' 
		foreach v in `varl' {
			binscatter `v' `xvari' if `xvari' > `limit' ///
				, n(`ngps') linetype(none) ///
				savedata("`fold'/`type'_`v'_${`ver'}")
		}
		
		//join matrices 
		tempfile tf
		local iter = 1 
		foreach v in `varl' {
			
			//import and prepare display 
			qui import delimited "`fold'/`type'_`v'_${`ver'}", clear 
			qui gen variable = "`v'"
			qui rename `v' value 
			
			local xsc 
			if ("`xvari'" == "nfarms") {
				qui replace `xvari' = round(`xvari')
				local xsc 0(1000)6000
			}
			
			
			//prepare graph lines  
			local ax 
			local ax2
			local col edkblue*.5
			if "`v'" == "`biovar'"{
				local ax yaxis(2)
				local ax2 , axis(2)
				local col dkgreen*.5
				local com "(left axis)"
				
			} 
			local lgf `lgf' ///
				(connected value `xvari' if variable == "`v'", ///
				ytitle("${lab_`v'}" `ax2') `ax' mcolor(`col') lcolor(`col') ///
				mfcolor(white))
				
			local txt `txt' ///
				(scatter value `xvari' ///
				if varlab != "" & variable == "`v'", ///
				mlabel(varlab) mlabcolor(`col') ///
				msymbol(none) mlabposition(5) `ax')	
			
			//save 
			if `iter' != 1 append using `tf'
			qui save `tf', replace 
			qui local iter = `iter' + 1 
		}
		
		
		*prepare labels 
		qui gen varlab = "Land inequality" if variable == "gini_can" 
		qui replace varlab = "${lab_`biovar'}" if variable == "`biovar'"
		//prepare labels
		foreach v in `varl' {
			qui sum `xvari' if variable == "`v'"  & `xvari' >= 3050
			local nfx_`v' = r(min)
			qui replace varlab = "" if variable == "`v'" & `xvari' != `nfx_`v''
		}

		graph twoway `lgf'  `txt', xtitle("${lab_`xvari'}") ///
			ylab(, angle(horizontal)) ///
			ylab(, angle(horizontal) axis(2)) ///
			xlab(`xsc') legend(off) $graph_scheme 	
		qui graph export ///
			"figures/concentration/bin_`xvari'_gini_`biovar'_`type'_${`ver'}.png", replace		
			
	}
}

	
