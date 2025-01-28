//setup 
clear all  

// 0. General settings
global absorber i.canton i.t 
*global absorber i.canton##i.month
qui do "code/Stata/functions/aux_general.do" 


*open database 
qui use "data/FR/multi_panel_foods_pa28.dta", replace 

*define canton 
cap drop canton 
qui encode adm_can, gen(canton)
qui so canton t 

//effects by quantile
foreach v in gini_can {
	
	qui so canton year 
	
	*build quintiles 
	cap drop quant_`v'
	qui xtile quant_`v' = `v' if year == 2015, nq(${q})
	bysort canton: ereplace quant_`v' = max(quant_`v') 
} 

binsreg seminat year 


tempfile tf 
binsreg seminat year, by(quant_gini_can) vce(cl canton) savedata(`tf') 

qui use `tf', clear 
qui keep quant_gini_can dots_x dots_fit 
qui reshape wide dots_fit, i(dots_x) j(quant_gini_can)

forval x = 1/4 {
	cap drop i`x'
	qui gen i`x' = dots_fit`x' / dots_fit`x'[1] * 100 
	local gl `gl' (connected i`x' dots_x) 
}

graph twoway `gl'
