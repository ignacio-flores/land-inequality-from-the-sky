*open database
qui use "data/FR/multi_panel_foods_pa28.dta", clear

*define canton 
cap drop canton 
qui encode adm_can, gen(canton)
qui so canton t 

global base_year 2015

gen tarea=area_agri if year==2015
byso canton: ereplace tarea=max(tarea)
drop if tarea<.1


global het_vars gini_can dcount_o dherf_o fsize seminat dherf_o fsize lfarms dfct_r
global dep_vars "gpp_w"

loc the_shock the_shock_w

foreach var in $het_vars {
	cap drop `var'_b
	gen `var'_b=`var'
	cap drop var1
	if "`var'"=="gini_can" {
		gen var1= `var' if year==${base_year}
	}
	else {
		gen var1= `var' if year==${base_year}
	}

	byso canton: ereplace `var'=max(var1)
	qui sum `var', d


	* Create interactions for continuous het and also by quantiles
	loc q=4
	cap drop `the_shock'X`var'
	qui gen `the_shock'X`var'=`the_shock'*`var'
	cap drop quant_`var'
	qui xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	qui tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		qui gen t_q_`var'`x'=`the_shock'*q_`var'`x'
	}
}

*define quantiles of hetvars within gini quantiles 
loc the_shock the_shock_w
loc q = 4 
foreach var in $het_vars {
	
	if "`var'" == "gini_can" continue 
	cap drop wgini_quant_`var'
	qui gen wgini_quant_`var'=.
	
	forval x = 1/`q' {
		di as result "sum `var' if quant_gini_can == `x', d"
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
			replace wgini_quant_`var' = `y' if quant_gini_can == `x' & `condition'			
		}
	}

	//check if it's alright 
	di as result "`var'"
	sum quant_`var' 
	tab quant_`var' wgini_quant_`var' 
	tab wgini_quant_`var' if quant_gini_can == 1	
	tab wgini_quant_`var' if quant_gini_can == 2	
	tab wgini_quant_`var' if quant_gini_can == 3	
	tab wgini_quant_`var' if quant_gini_can == 4	
}






