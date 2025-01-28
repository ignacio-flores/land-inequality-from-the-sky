////////////////////////////////////////////////////////////////////////////////
//
// 					Title: LAND INEQUALITY FROM THE SKY 
// 			 	   Authors: Ignacio FLORES, Dylan GLOVER
// 							    Year: 2020
//
// 								  Purpose:
// 							Run all do-files 
//
////////////////////////////////////////////////////////////////////////////////

*ssc install ereplace did_multiplegt eststo binscatter reghdfe ftools estout coefplot mylabels wid kountry 

//general settings 
macro drop _all 
clear all 
*capture cd "~/Dropbox/land_ineq_degradation/"

//list codes 
**********************************************************
global do_codes1 " "01a" " 
global do_codes2 " "02b" "02c" "02d" " 
**********************************************************

//report and save start time 
global run_everything " "ON" "
local start_t "($S_TIME)"
di as result "Started running everything working at `start_t'"

//prepare list of do-files 
forvalues n = 1/2 {

	//get do-files' name 
	foreach docode in ${do_codes`n'} { 
		local do_name : dir "code/Stata/." files "`docode'*.do" 
		local do_name = subinstr(`do_name', char(34), "", .)
		global doname_`docode' `do_name'
	}
}	

//loop over all files  
forvalues n = 1/2 {
	foreach docode in ${do_codes`n'} {
	
		********************* 
		do code/Stata/${doname_`docode'}
		*********************
		
		//record time
		global do_endtime_`docode' " - ended at ($S_TIME)"
		
		//remember work plan
		di as result "{hline 70}" 
		di as result "list of files to run, started at `start_t'"
		di as result "{hline 70}"
		forvalues x = 1/2 {
			di as result "Stage nº`x'"
			foreach docode2 in ${do_codes`x'} {
				di as text "  * " "${doname_`docode2'}" _continue
				di as text " ${do_endtime_`docode2'}"
			}
			if `x' == 2 di as result "{hline 70}"	
		}
	}
}

global run_everything " "" "

