*************
*ssc install coefplot eststo twowayfeweights did_multiplegt_dyn
run "code/Stata/functions/aux_general.do"

qui u data/FR/multi_panel_foods_pa28, clear
cap drop canton
qui encode adm_can, g(canton)
qui so canton t

* List variables 
global rename "ccount dcount dherf"
foreach var in $rename {
	rename `var'_o `var'
}
global base_year 2015

qui gen testfsize = farm_area_est_farms / nfarms
qui gen tarea = area_agri if year == 2015
byso canton: ereplace tarea = max(tarea)
qui drop if tarea < .1
global het_vars gini_can dcount dherf fsize seminat lfarms dfct_r
global dep_vars "gpp_w"

foreach var in $het_vars {
	cap drop `var'_b
	qui gen `var'_b=`var'
	cap drop var1
	if "`var'"=="gini_can" {
		qui gen var1= `var' if year==${base_year}
		byso canton: ereplace `var'=max(var1)
	}

	* Create interactions for continuous het and also by quantiles
	loc q=4
	cap drop quant_`var'
	qui xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	qui tab quant_`var', gen(q_`var')
}


*define quantiles of hetvars within gini and fsize quantiles 
loc q = 4 
foreach var in $het_vars {
	
	if "`var'" == "gini_can" continue 
	cap drop wgini_can_quant_`var'
	qui gen wgini_can_quant_`var'=.
	
	forval x = 1/`q' {
		*di as result "sum `var' if quant_gini_can == `x', d"
		qui sum `var' if quant_gini_can == `x', d
		forval y = 1/`q' {
			*define locals 
			local ymin = `y' - 1
			local ln = round(`ymin'/`q' * 100)
			local hn = round(`y'/`q' * 100)
			*define condition 
			local condition `var' >= r(p`ln') & `var' < r(p`hn')
			if `y' == 1 local condition `var' < r(p`hn')
			if `y' == `q' local condition `var' >= r(p`ln')
			*replace variable 
			qui replace wgini_can_quant_`var' = `y' ///
				if quant_gini_can == `x' & `condition'			
		}
	}
}
foreach var in $het_vars {
	if "`var'" == "fsize" continue 
	cap drop wfsize_quant_`var'
	qui gen wfsize_quant_`var'=.
	
	forval x = 1/`q' {
		*di as result "sum `var' if quant_fsize == `x', d"
		qui sum `var' if quant_fsize == `x', d
		forval y = 1/`q' {
			*define locals 
			local ymin = `y' - 1
			local ln = round(`ymin'/`q' * 100)
			local hn = round(`y'/`q' * 100)
			*define condition 
			local condition `var' >= r(p`ln') & `var' < r(p`hn')
			if `y' == 1 local condition `var' < r(p`hn')
			if `y' == `q' local condition `var' >= r(p`ln')
			*replace variable 
			qui replace wfsize_quant_`var' = `y' ///
				if quant_fsize == `x' & `condition'			
		}
	}
}

*cap n gen gpp_w = gpp
cap n gen lgpp_w = ln(gpp_w)
cap n gen lcgpp_w = ln(cgpp_w)

* Cumulative GPP
so canton t
byso canton year: gen cgpp_w=sum(gpp_w)
byso canton year: egen pmax=max(p)

global shock the_shock_s
byso canton year: gen w = sum(${shock})
qui replace w = 1 if w > 0

cap drop min_p
qui gen min_p=p if w==1
byso canton year: ereplace min_p=min(min_p)
qui replace min_p=99 if min_p==.


cap drop can_year
qui egen can_year=group(canton year)
cap drop ndept
qui encode dept, g(ndept)
xtset canton t

foreach var of var gini_can fsize {
	cap drop cut_`var'
	sum `var', d
	qui gen cut_`var'=r(p99)
}  

* Centering controls
foreach var of var crop1-crop28 {
	cap drop c_`var'
	qui sum `var'
	qui gen c_`var'=`var'-r(mean)
}

estimates clear
qui replace gini_can = gini_can * 100
cap drop tquant
qui gen tquant = . 

* Productivity and temperature
cap drop cut
gen int cut=.
forval q=1/4 {
	qui sum tsup_h_q if the_shock_s & quant_gini_can == `q'
	global cut=r(min)
	replace cut = round(${cut}) if quant_gini_can == `q'
}

cap drop gpp2_w
qui gen gpp2_w = gpp_w * 1000
cap drop lgpp2_w
qui gen lgpp2_w = ln(gpp2_w)

*graph1 
tempfile tf 
binsreg gpp2_w tsup_h_q, by(quant_gini_can) ///
	absorb(can_year) samebinsby usegtools(on) savedata(`tf') 	
preserve 
	qui use `tf', clear 
	forval x = 1/4 {
		local p `p' (scatter dots_fit dots_x if quant_gini_can == `x', ///
			msize(small) color("${col`x'}") msymbol("${sym`x'}")) 
	}
	graph twoway `p', ///
		xline(27, lcolor(black) lpattern(dash)) ///
		xtitle("Temperature °C") ///
		ytitle("GPP in C.g/m{superscript:2} (weekly)") ///
		legend(order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4") region(lwidth(0)) ///
		title("Gini" "Quantiles", size(normal)) pos(0) bplace(nw) col(1)) ///
		xlabel(0(5)35) ylab(, angle(horizontal))  $graph_scheme
	graph export "figures/gpp2_temp_by_gini.png", replace 
restore 

*fsize graph 
/*
binsreg gpp_w tsup_h_q, by(quant_fsize) absorb(can_year) samebinsby  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 2 "q2" 3 "q3" 4 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(nw)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_fsize.png", replace 
*/


*graph 2 (zoom)
sum cut
loc cut = r(mean)
tempfile tf2 
binsreg gpp2_w tsup_h_q if tsup_h_q >= 27, ///
	by(quant_gini_can) absorb(can_year) samebinsby polyreg(1) usegtools(on) ///
	savedata(`tf2') 
preserve 	
	qui use `tf2', clear 
	forval x = 1/4 {
		local p `p' ///
			(scatter dots_fit dots_x if quant_gini_can == `x', ///
			msize(small) color("${col`x'}") msymbol("${sym`x'}")) 
		local l `l' ///
			(line poly_fit poly_x if quant_gini_can == `x', ///
			msize(small) color("${col`x'}") msymbol("${sym`x'}")) 
	}	
		graph twoway `p' `l', ///
			xline(27, lcolor(black) lpattern(dash)) ///
			xtitle("Temperature °C") ///
			ytitle("GPP in C.g/m{superscript:2} (weekly)") ///
			legend(order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4") ///
			region(lwidth(0) color(none)) ///
			title("Gini" "Quantiles", size(normal)) pos(0) bplace(sw) col(1)) ///
			xlabel(27(1)37) ylab(, angle(horizontal))  $graph_scheme
	graph export "figures/gpp2_temp_by_gini_zoom27.png", replace 
restore 

/*
sum cut
loc cut=r(mean)
binsreg gpp_w tsup_h_q if tsup_h_q>=`cut', by(quant_fsize) absorb(canton##year) samebinsby polyreg(3)  usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(ne)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_fsize_zoom`cut'.png", replace 
*/


* Temperature event study
cap drop tsup_h_q_r
gen tsup_h_q_r = round(tsup_h_q)

*preserve
keep if tsup_h_q_r >= 25
loc quant 4
loc base 25
foreach depvar in lgpp_w {
	
	estimates clear
	foreach var in gini_can  {
		foreach qtle in `quant' {

			cap tab quant_`var',gen(q_`var')
			forval q=2/`qtle' {
				cap drop var_q
				gen var_q=q_`var'`q'
				cap drop treatment
				eststo `var'_`q': reghdfe `depvar' ///
					b`base'.tsup_h_q_r##var_q  ///
					if (quant_`var'==1 | quant_`var'==`q') ///
					& tsup_h_q_r<37, absorb(canton##year) ///
					vce(cl canton) resid
			}

		}

		global xlab
		forval x=1/12 {
			loc t=`x'+24		
			global xlab " ${xlab}  `x' " `" "`t'" "' " "
		} 
		dis "${xlab}"
		global lab: dis "${xlab}"

		loc xl=6.5
		global xline=`xl'

		#delimit
		coefplot ///
			(`var'_2, m("${sym2}") mc("${col2}")  ciopts(lpattern(shortdash) lcolor("${col2}")))
			(`var'_3, m("${sym3}") mc("${col3}")  ciopts(lpattern(shortdash) lcolor("${col3}")))
			(`var'_4, m("${sym4}") mc("${col4}")  ciopts(lpattern(shortdash) lcolor("${col4}"))),
			keep(`base'.tsup_h_q_r *.tsup_h_q_r#1.var_q) 
			xlabel(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5) base 
			ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
			ylab(,angle(horizontal)) xtitle("") xline(${xline}, lcolor(red) lpattern(dot))
			$graph_scheme name(G`quant'_`depvar'_`var' , replace)
			legend(order( 2 "Q2" 4 "Q3" 6 "Q4" ) col(1)  
			region(lwidth(0) color(none)) title("Land Gini" "quantiles", 
			size(normal)) subtitle("ref. Q1 and °C=25", size(small)) 
			pos(0) bplace(sw));
		graph export "figures/temp_trends_`var'.png", replace ;
		#delimit cr
	}
}

