global dir "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"

global tabs "/Users/dylanglover/Dropbox/Apps/Overleaf/Land Inequality from the Sky/tabs"
global figs "/Users/dylanglover/Dropbox/Apps/Overleaf/Land Inequality from the Sky/figures"
/*
global tabs "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/offline/tabs"
global figs "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/offline/figures"
*/
*global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
global rep "$dir"
cd "$dir"
*se maxvar 120000
*************
u data/FR/multi_panel_foods_pa28, clear
cap drop canton
encode adm_can, g(canton)
so canton t


* Month or Week as unit
global inter w
*global inter m

* List variables 
global rename "ccount dcount dherf"

foreach var in $rename {
rename `var'_o `var'
}

global base_year 2015
*gini_can fsize dherf dcount farm_area_est farm_area_est_farms nfarms crops seminat

gen testfsize=farm_area_est_farms/nfarms
sum testfsize,d

sum area_agri canton_area_w farm_area_est fsize farm_area_est_farms,d

sum nfarms if year==${base_year},d
sum area_agri if year==${base_year},d
gen tarea=area_agri if year==2015
byso canton: ereplace tarea=max(tarea)
drop if tarea<.1
sum area_agri,d
* Normalize crop count canton-size 
cap drop count_ckm2
gen count_ckm2=dcount/(canton_area_${inter})
cap drop nfarms_ckm2
gen nfarms_ckm2=nfarms/(canton_area_${inter})

cap drop count_km2
gen count_km2=dcount/farm_area_est
cap drop nfarms_km2
gen nfarms_km2=nfarms/farm_area_est

cap drop fsize_ckm2
gen fsize_ckm2=fsize/canton_area_${inter}
cap drop fsize_km2
gen fsize_km2=fsize/farm_area_est

cap drop rat_perim_area
gen rat_perim_area=farm_perim_est/farm_area_est

sum seminat
replace seminat=.0005149 if seminat==0 
sum seminat rat_perim_area,d
sum seminat
gen s_seminat=seminat/r(sd)
sum rat_perim_area
gen s_rat_perim_area=rat_perim_area/r(sd)
gen semi_pa_rat=s_seminat*s_rat_perim_area


global het_vars gini_can dcount dherf fsize seminat  rat_perim_area semi_pa_rat lfarms dfct_r
global dep_vars "gpp_${inter}"


loc the_shock the_shock_w

foreach var in $het_vars {
	cap drop `var'_b
	gen `var'_b=`var'
	cap drop var1
	if "`var'"=="gini_can" {
		gen var1= `var' if year==${base_year}
			byso canton: ereplace `var'=max(var1)
	qui sum `var', d
	}
	/*
	else {
		gen var1= `var' if year==${base_year}
	}
*/



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

loc q = 4 
foreach var in $het_vars {
	
	if "`var'" == "gini_can" continue 
	cap drop wgini_can_quant_`var'
	qui gen wgini_can_quant_`var'=.
	
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
			replace wgini_can_quant_`var' = `y' if quant_gini_can == `x' & `condition'			
		}
	}

	//check if it's alright 
	di as result "`var'"
	sum quant_`var' 
	tab quant_`var' wgini_can_quant_`var' 
	tab wgini_can_quant_`var' if quant_gini_can == 1	
	tab wgini_can_quant_`var' if quant_gini_can == 2	
	tab wgini_can_quant_`var' if quant_gini_can == 3	
	tab wgini_can_quant_`var' if quant_gini_can == 4	
}
foreach var in $het_vars {
		if "`var'" == "fsize" continue 
	cap drop wfsize_quant_`var'
	qui gen wfsize_quant_`var'=.
	
	forval x = 1/`q' {
		di as result "sum `var' if quant_fsize == `x', d"
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
			replace wfsize_quant_`var' = `y' if quant_fsize == `x' & `condition'			
		}
	}

	//check if it's alright 
	di as result "`var'"
	sum quant_`var' 
	tab quant_`var' wfsize_quant_`var' 
	tab wfsize_quant_`var' if quant_fsize == 1	
	tab wfsize_quant_`var' if quant_fsize == 2	
	tab wfsize_quant_`var' if quant_fsize == 3	
	tab wfsize_quant_`var' if quant_fsize == 4	
}


cap n gen the_shock_${inter}=the_shock
cap n gen gpp_${inter}=gpp
cap n gen lgpp_${inter}=ln(gpp_${inter})
cap n gen lcgpp_${inter}=ln(cgpp_${inter})
cap n gen temp_${inter}=temp
cap n gen the_shock_y_${inter}=the_shock_y
ta month the_shock_${inter}


	cap label var crop1 "Wheat"
	cap label var crop2 "Corn" 
	cap label var crop3 "Barley" 
	cap label var crop4 "Other cereals"
	cap label var crop5 "Colza" 
	cap label var crop6 "Sunflower" 
	cap label var crop7 "Other oilseeds" 
	cap label var crop8 "Protein crops"
	cap label var crop9 "Fiber plants" 
	cap label var crop14 "Rice" 
	cap label var crop16 "Feed" 
	cap label var crop18 "Perm. meadow"
	cap label var crop19 "Temp. meadow" 
	cap label var crop21 "Vines" 
	cap label var crop22 "Nuts" 
	cap label var crop17 "Sum. pastures & moors"
	cap label var crop28 "Others" 
	cap label var crop25 "Vegetables & flowers" 
	cap label var crop20 "Orchards"
	cap label var crop24 "Other indsutrial crops" 
	cap label var crop23 "Olive" 
	cap label var crop11 "Uncultivated land"
	cap label var crop15 "Grain legumes" 
	cap label var crop10 "Seeds (old)"  
	cap label var crop13 "Other uncultivated land (old)"
	cap label var crop26 "Sugar cane (old)" 
	cap label var crop27 "Arboriculture" 
	cap label var crop99 "Not identified"

sum crop*

* Cumulative GPP
so canton t
byso canton year: gen cgpp_${inter}=sum(gpp_${inter})
byso canton year: egen pmax=max(p)

ta p month

binscatter lgpp_${inter} p, line(connect)
global shock the_shock_s
	byso canton year: gen w = sum(${shock})
	qui replace w = 1 if w > 0

cap drop min_p
gen min_p=p if w==1
byso canton year: ereplace min_p=min(min_p)
replace min_p=99 if min_p==.


cap drop can_year
egen can_year=group(canton year)
cap drop ndept
encode dept, g(ndept)


xtset canton t

foreach var of var gini_can fsize {
	cap drop cut_`var'
sum `var', d
	gen cut_`var'=r(p99)
}  

* But where do we get our food from?
cap drop gpp_${inter}_p
gen gpp_${inter}_p=farm_area_est*gpp_${inter}*10 // Converted into Mt/ha (10000 m2 per ha * 1/1000 Mt/kg)
sum gpp_${inter}_p
cap drop cgpp_${inter}_p
gen cgpp_${inter}_p=farm_area_est*cgpp_${inter}*10
sum cgpp_${inter}_p
sum gpp_${inter}_p,d
sum gpp_${inter},d

sum cgpp_${inter}_p if p==pmax ,d
cap drop cgpp_${inter}_pmax
gen cgpp_${inter}_pmax=cgpp_${inter}_p if p==pmax
replace cgpp_${inter}_pmax=cgpp_${inter}_pmax

cap drop y_cgpp_${inter}_pmax
byso year: egen y_cgpp_${inter}_pmax=total(cgpp_${inter}_pmax)
cap drop gini_y_cgpp_${inter}_pmax
byso year quant_gini_can: egen gini_y_cgpp_${inter}_pmax=total(cgpp_${inter}_pmax)
cap drop fsize_y_cgpp_${inter}_pmax
byso year quant_fsize: egen fsize_y_cgpp_${inter}_pmax=total(cgpp_${inter}_pmax)

cap drop pct_gini_y_cgpp_${inter}
gen pct_gini_y_cgpp_${inter}=gini_y_cgpp_${inter}_pmax/y_cgpp_${inter}_pmax
cap drop pct_fsize_y_cgpp_${inter}
gen pct_fsize_y_cgpp_${inter}=fsize_y_cgpp_${inter}_pmax/y_cgpp_${inter}_pmax
byso quant_gini_can: sum pct_gini_y_cgpp_${inter}
byso quant_fsize: sum pct_fsize_y_cgpp_${inter}

* Centering controls
foreach var of var crop1-crop28 {
	cap drop c_`var'
	sum `var'
	gen c_`var'=`var'-r(mean)
}



binsreg cgpp_${inter}_pmax gini_can c_crop1-c_crop28 if gini_can<=cut_gini_can,  nbins(50) absorb(ndept##year) binspos(qs)  xtitle(Land Gini) ytitle(Total GPP in C.Mt/ha) ///
scheme(plotplainblind) name(gini_can_agg , replace) dotsplotopt(mcolor(blue))
graph export "$figs/food_agg_prod_by_gini.png", replace 
binsreg cgpp_${inter}_pmax fsize c_crop1-c_crop28  if fsize<=cut_fsize, nbins(50) absorb(ndept##year) binspos(qs)  xtitle(Avg Farm Size in ha) ytitle(Total GPP in C.Mt/ha) ///
scheme(plotplainblind) name(fsize_agg , replace) dotsplotopt(mcolor(blue))
graph export "$figs/food_agg_prod_by_fsize.png", replace 

binsreg cgpp_${inter} gini_can c_crop1-c_crop28 if p==pmax & gini_can<=cut_gini_can,  nbins(50) absorb(ndept##year) binspos(qs) line() xtitle(Land Gini) ytitle(Avg. GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(gini_can , replace) ylabel(1.05(.02)1.13) dotsplotopt(mcolor(green))
graph export "$figs/food_prod_by_gini.png", replace 
binsreg cgpp_${inter} fsize c_crop1-c_crop28  if p==pmax & fsize<=cut_fsize, nbins(50) absorb(ndept##year) binspos(qs) line() xtitle(Avg Farm Size in ha) ytitle(Avg. GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(fsize , replace) ylabel(1.05(.02)1.13) dotsplotopt(mcolor(green))
graph export "$figs/food_prod_by_fsize.png", replace 


estimates clear
preserve
replace gini_can=gini_can*100
cap drop tquant
gen tquant=.
foreach x in gini_can fsize {

eststo s_`x'_p_0: reghdfe cgpp_${inter}_p `x' if p==pmax,vce(cl canton)
*estadd local hascon0 ""
*estadd local hascon1 ""
eststo s_`x'_p: reghdfe cgpp_${inter}_p `x' if p==pmax, absorb(ndept##year) vce(cl canton)
*estadd local hascon0 "$\checkmark$"
*estadd local hascon1 ""
eststo s_`x'_p_c: reghdfe cgpp_${inter}_p `x' c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
*estadd local hascon0 "$\checkmark$"
*estadd local hascon1 "$\checkmark$"

eststo s_`x'_0: reghdfe cgpp_${inter} `x' if p==pmax, vce(cl canton)
*estadd local hascon0 ""
*estadd local hascon1 ""
eststo s_`x': reghdfe cgpp_${inter} `x' if p==pmax, absorb(ndept##year) vce(cl canton)
*estadd local hascon0 "$\checkmark$"
*estadd local hascon1 ""
eststo s_`x'_c: reghdfe cgpp_${inter} `x' c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
*estadd local hascon0 "$\checkmark$"
*estadd local hascon1 "$\checkmark$"


replace tquant=quant_`x'
eststo q_`x'_p_0: reghdfe cgpp_${inter}_p i.tquant if p==pmax,vce(cl canton)
estadd local hascon0 = "", replace : q_`x'_p_0
estadd local hascrop = "", replace : q_`x'_p_0
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'_p_0
estadd sca pval = `p', replace : q_`x'_p_0

eststo q_`x'_p: reghdfe cgpp_${inter}_p i.tquant if p==pmax, absorb(ndept##year) vce(cl canton)
estadd local hascon0  ="$\checkmark$", replace : q_`x'_p   
estadd local hascrop = "", replace  : q_`x'_p 
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'_p
estadd sca pval = `p', replace : q_`x'_p

eststo q_`x'_p_c: reghdfe cgpp_${inter}_p i.tquant c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
estadd local hascon0 = "$\checkmark$", replace : q_`x'_p_c   
estadd local hascrop = "$\checkmark$", replace  : q_`x'_p_c
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'_p_c
estadd sca pval = `p', replace : q_`x'_p_c


eststo q_`x'_0: reghdfe cgpp_${inter} i.tquant if p==pmax, vce(cl canton)
estadd local hascon0 = "", replace : q_`x'_0    
estadd local hascrop = "", replace : q_`x'_0 
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'_0
estadd sca pval = `p', replace : q_`x'_0

eststo q_`x': reghdfe cgpp_${inter} i.tquant if p==pmax, absorb(ndept##year) vce(cl canton)
estadd local hascon0 = "$\checkmark$", replace : q_`x'
estadd local hascrop = "", replace : q_`x'
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'
estadd sca pval = `p', replace : q_`x'

eststo q_`x'_c: reghdfe cgpp_${inter} i.tquant c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
estadd local hascon0 = "$\checkmark$", replace : q_`x'_c    
estadd local hascrop = "$\checkmark$", replace : q_`x'_c 
margins i.tquant, post coefleg
test _b[1bn.tquant]= _b[2.tquant]=_b[3.tquant]=_b[4.tquant]
loc p=r(p)
est restore q_`x'_c
estadd sca pval = `p', replace : q_`x'_c

}

/*
		#delimit
			esttab s_gini_can_p_0 s_gini_can_p s_gini_can_p_c s_fsize_p s_fsize_p_c  using 
				"$tabs/food_agg_prod_by_s.tex",  
				f nomti noli collabels(none) rename(gini_can Consolidation)
				keep(_cons  Consolidation) 
				varlabels(_cons "Constant" ) mgroups("Land Gini" "Farm Size", pattern(1 0 1 0)
				  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
				 style(tex) noobs cells(b(fmt(0) star) 
				se(par fmt(0))) star(* .1 ** .05 *** .01) replace;
		#delimit cr
		*/
				#delimit
			esttab s_gini_can_p_0 s_gini_can_p s_gini_can_p_c s_fsize_p_0 s_fsize_p s_fsize_p_c  using 
				"$tabs/food_agg_prod_by_s.tex",  
				f nomti noli nonum collabels(none) rename(gini_can Consolidation fsize Consolidation)
				keep(_cons  Consolidation) 
				varlabels(_cons "Constant" ) 	 
				style(tex) noobs cells(b(fmt(0) star) se(par fmt(0))) star(* .1 ** .05 *** .01) replace;
		#delimit cr

		#delimit
			esttab s_gini_can_0 s_gini_can s_gini_can_c s_fsize_0 s_fsize s_fsize_c  using 
				"$tabs/food_prod_by_s.tex",  
				f nomti noli nonum collabels(none) rename(gini_can Consolidation fsize Consolidation)
				keep(_cons  Consolidation) 
				varlabels(_cons "Constant") 
				 style(tex) noobs cells(b(fmt(4) star) 
				se(par fmt(4))) star(* .1 ** .05 *** .01) replace;
		#delimit cr


		loc stats "stats(pval r2 N hascon0 hascrop, label("p-val equal effects" "\\ R-squared" "N" "(Geo x Year) FEs" "Crop types") fmt(%9.3f %9.2f 0))"
		#delimit
			esttab q_gini_can_p_0 q_gini_can_p q_gini_can_p_c q_fsize_p_0 q_fsize_p q_fsize_p_c   using 
				"$tabs/food_agg_prod_by_q.tex",  `stats'
				f nomti noli nonum collabels(none) 
				keep(_cons  *.tquant) drop(1.tquant) order(_cons  *.tquant)
				varlabels(_cons "mean 1st quantile (ref)"
				 2.tquant "2nd quantile" 
				  3.tquant "3rd quantile"  4.tquant "4th quantile" ) 
				 style(tex) noobs cells(b(fmt(0) star) 
				se(par fmt(0))) star(* .1 ** .05 *** .01) replace;
		#delimit cr

		#delimit
			esttab q_gini_can_0 q_gini_can q_gini_can_c q_fsize_0 q_fsize q_fsize_c  using 
				"$tabs/food_prod_by_q.tex",  
				f nomti noli nonum collabels(none) 
				keep(_cons  *.tquant) drop(1.tquant) order(_cons  *.tquant) 
				varlabels(_cons "mean 1st quantile (ref)"
				 2.tquant "2nd quantile" 
				  3.tquant "3rd quantile"  4.tquant "4th quantile") 
				`stats' style(tex) noobs cells(b(fmt(3) star) 
				se(par fmt(3))) star(* .1 ** .05 *** .01) replace;
		#delimit cr


sum gini_can,d

* Productivity and temperature
cap drop cut
gen int cut=.
forval q=1/4 {
sum tsup_h_q if the_shock_s & quant_gini_can==`q',
global cut=r(min)
replace cut=round(${cut}) if quant_gini_can==`q'
}
binsreg gpp_${inter} tsup_h_q, by(quant_gini_can) absorb(can_year) samebinsby  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 2 "q2" 3 "q3" 4 "q4" )  title(Gini quantiles, size(small))   pos(0) bplace(nw)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_gini.png", replace 


binsreg gpp_${inter} tsup_h_q, by(quant_fsize) absorb(can_year) samebinsby  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 2 "q2" 3 "q3" 4 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(nw)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_fsize.png", replace 


/*
pause on
preserve
u data/FR/aux_files/bin_data1, clear
pause
*/

sum cut
loc cut=r(mean)
*se trace on
binsreg gpp_${inter} tsup_h_q if tsup_h_q>=cut, by(quant_gini_can) absorb(can_year) samebinsby polyreg(3) usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Gini quantiles, size(small))   pos(0) bplace(ne)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_gini_zoom`cut'.png", replace 

sum cut
loc cut=r(mean)
binsreg gpp_${inter} tsup_h_q if tsup_h_q>=`cut', by(quant_fsize) absorb(canton##year) samebinsby polyreg(3)  usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(ne)) scheme(plotplainblind)
graph export "$figs/gpp_temp_by_fsize_zoom`cut'.png", replace 


estimates clear
cap drop tquant
gen tquant=.
foreach x in gini_can fsize {
replace tquant=quant_`x'
eststo gpp_`x': reghdfe lgpp_${inter} c.tsup_h_q##i.tquant, absorb(canton##year) vce(cl canton)
estadd local hascon0 "$\checkmark$", replace: gpp_`x'
eststo z_gpp_`x': reghdfe lgpp_${inter} c.tsup_h_q##i.tquant if tsup_h_q>=cut, absorb(canton##year) vce(cl canton)
estadd local hascon0 "$\checkmark$", replace: z_gpp_`x'
}


		loc stats ///
			"stats(r2 N hascon0, label("R-squared" "N" "(Panel unit x Year) FEs") fmt(%9.2f %9.0f))"

	#delimit
			esttab gpp_gini_can z_gpp_gini_can   using 
				"$tabs/gpp_temp_by_gini.tex",  
				f nomti noli nonum collabels(none) 
				keep(tsup_h_q *.tquant#c.tsup_h_q) drop(1.tquant#c.tsup_h_q)
				varlabels(tsup_h_q "Temp. 1st quantile (ref)"
				 2.tquant#c.tsup_h_q "Temp. x 2nd quantile" 
				  3.tquant#c.tsup_h_q "Temp. x 3rd quantile"  4.tquant#c.tsup_h_q "Temp. x 4th quantile") /*mgroups("All" ">wgt. threshold", pattern(1 1))*/
				`stats' style(tex) noobs cells(b(fmt(5) star) 
				se(par fmt(5))) star(* .1 ** .05 *** .01) replace;
		#delimit cr



* Temperature event study
cap drop tsup_h_q_r
gen tsup_h_q_r=round(tsup_h_q)

preserve
keep if tsup_h_q_r>=25

loc quant 4
loc base 25
foreach depvar in lgpp_$inter  /*lgpp_$inter */ {
	estimates clear

foreach var in gini_can  {
	foreach qtle in `quant' {

			*keep if year>2018 & year <2021

			* Redfining quantiles or median 
			/*
			cap drop quant_`var'
			xtile quant_`var'= `var', nq(`qtle') 
			cap drop q_`var'*
			*/
			cap tab quant_`var',gen(q_`var')
			forval q=2/`qtle' {
				cap drop var_q
				gen var_q=q_`var'`q'
				cap drop treatment
				eststo `var'_`q': reghdfe `depvar' b`base'.tsup_h_q_r##var_q  ///
				if (quant_`var'==1 | quant_`var'==`q') & tsup_h_q_r<37, absorb(canton##year) vce(cl canton) resid

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
	*global xline
/*
	#delimit
	coefplot(G2_2, m(o) mc(black) connect(line) 
		lcolor(gs15) lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs9))),
		keep(1.F*_the_shock#1.var_q 1.the_shock#1.var_q 1.L*_the_shock#1.var_q) 
		xcap label(${lab})
		yline(0, lcolor(gs0)) vertical mcolor(gs5) omitted
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(black) lpattern(shortdash)) 
		scheme(plotplainblind) name(G2_`var', replace);
	*graph export "$Dtabs/trends_`var'_median.png", replace ;
	#delimit cr
*/
	#delimit
	coefplot(`var'_2, m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
		(`var'_3, m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
		(`var'_4, m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange))),
		keep(`base'.tsup_h_q_r *.tsup_h_q_r#1.var_q) 
		xlabel(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5) base 
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(red) lpattern(shortdash))
		scheme(plotplainblind) name(G`quant'_`depvar'_`var' , replace)
legend(order( 2 "q2" 4 "q3" 6 "q4" )  title(Land Gini quantiles, size(small)) subtitle(ref. q1 and °C=25, size(small))   pos(0) bplace(sw));
	graph export "$figs/temp_trends_`var'.png", replace ;
	#delimit cr
}
}



* ATEs and seminat moderation
foreach b in gini_can fsize {
	foreach var in $het_vars {
cap n gen `var'_med=quant_`var'>2 if quant_`var'!=.
cap n gen w`b'_med_`var'=w`b'_quant_`var'>2
}
}

ta p the_shock_s
ta p month
* ATEs gini and farm sizes
ta wgini_can_quant_seminat
global ylab "-.1(.02)-.22"
global ylab2 "-.1(.02)-.22"
		estimates clear
foreach shock in the_shock_s  {


loc range "p, 18, 26"
*loc range "p, 33, 45"
*loc range "p, 27, 33"
*loc range "p, 44, 44"

foreach var in gini_can fsize {

cap drop s_`var'
sum `var'
gen s_`var'=`var'/r(sd)

	loc q=4
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=`shock'*q_`var'`x'
				capture drop t_q_`var'_Hsnat`x'
		gen t_q_`var'_Hsnat`x'=t_q_`var'`x'*(w`var'_med_seminat)
		cap drop  t_q_`var'_Lsnat`x'
		gen t_q_`var'_Lsnat`x'=t_q_`var'`x'*(1-w`var'_med_seminat)

	}
		eststo `var'QC: reghdfe lgpp_${inter} c.s_`var'##`shock' if inrange(`range'), absorb(canton##year p) vce(cl canton#year)
		eststo `var'QB: reghdfe lgpp_${inter} t_q_`var'? if inrange(`range'), absorb(canton##year p) vce(cl canton#year)
		eststo `var'Q: reghdfe lgpp_${inter} t_q_`var'_H* t_q_`var'_L* if inrange(`range'), absorb(canton#year p) vce(cl canton#year)

		forval m=1/`q' {
		estimates restore `var'QB
		nlcom _b[t_q_`var'`m'], post
		eststo B`var'_`m'
				estimates restore `var'Q
				nlcom _b[t_q_`var'_Hsnat`m'], post
		eststo BH`var'_`m'
				estimates restore `var'Q
				nlcom _b[t_q_`var'_Lsnat`m'], post
		eststo BL`var'_`m'
		}


					#delimit
			coefplot (B`var'_1, rename(_nl_1=q1) m(O) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)))
			(B`var'_2, rename(_nl_1=q2) m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
			(B`var'_3, rename(_nl_1=q3) m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
			(B`var'_4, rename(_nl_1=q4) m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange))),  
				xlabel(1 "q1" 2 "q2" 
				3 "q3" 4 "q4")
				yline(0, lcolor(gs0)) vertical ylabel(${ylab})
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(het_impact_`var'_`shock',replace) legend(off);
			graph export "$figs/het_impact_`var'_`shock'.png", replace ;

						#delimit
			coefplot (BH`var'_1, rename(_nl_1=q1) m(arrow) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)))
			/*(B`var'_1, rename(_nl_1=q1) m(O) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)))*/
			(BL`var'_1, rename(_nl_1=q1) m(arrow) msangle(180) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)))

			(BH`var'_2, rename(_nl_1=q2) m(arrow) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
			/*(B`var'_2, rename(_nl_1=q2) m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))*/
			(BL`var'_2, rename(_nl_1=q2) m(arrow) msangle(180) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))

						(BH`var'_3, rename(_nl_1=q3) m(arrow) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
						/*(B`var'_3, rename(_nl_1=q3) m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))*/
						(BL`var'_3, rename(_nl_1=q3) m(arrow) msangle(180) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))

						(BH`var'_4, rename(_nl_1=q4) m(arrow) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange)))
						/*(B`var'_4, rename(_nl_1=q4) m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange)))*/
						(BL`var'_4, rename(_nl_1=q4) m(arrow) msangle(180) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange)))
,  
				xlabel(1 `""q1" "High Low""' 2 `""q2" "High Low""'  3 `""q3" "High Low""'
					4 `""q4" "High Low""')
				yline(0, lcolor(gs0)) vertical ylabel(${ylab})
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(het_impact_`var'_`shock',replace) legend(off);
			graph export "$figs/het_impact_`var'_`shock'_HL.png", replace ;
		#delimit cr	
		}
	}


xtset canton t

ta p month
twowayfeweights lgpp_${inter} canton p the_shock_s if inrange(p, 11, 33), type(feTR) summary_measures
twowayfeweights lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 30), type(feS) summary_measures
twowayfeweights lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26), type(feS) summary_measures
twowayfeweights lgpp_${inter} canton p the_shock_s if inrange(p, 18, 26), type(feS) summary_measures


loc g=4
local nume 1
loc nump 0
loc num=`nume'+`nump'+1
loc range "p, 18, 26"
*loc range "p, 27, 33"

foreach switchers in "" only_never_switchers {

forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(`range') & quant_gini_can==`q', placebo(`nump') effects(`nume') trends_nonparam(canton) `switchers'
		matrix C`q' = J(`num',3,.)
loc i `nump'
forval p=1/`nump'{
sca ate_`q'_`p'=e(Placebo_`i')	
sca se_ate_`q'_`p'=e(se_placebo_`i')
loc --i
}
/*
loc plus=`nump'+1
sca ate_`q'_`plus'=0
sca se_ate_`q'_`plus'=0
*/
forval p=1/`nume'{
loc plus=`nump'+`p'+1
sca ate_`q'_`plus'=e(Effect_`p')	
sca se_ate_`q'_`plus'=e(se_effect_`p')
}
forval t=1/`num' {
        matrix C`q'[`t',1] = ate_`q'_`t', ate_`q'_`t' + se_ate_`q'_`t'*1.96, ate_`q'_`t' - se_ate_`q'_`t'*1.96
matrix list C`q'
}
}

	matrix C = C1 , C2, C3, C4
	mat list C
	mat list C1
	mat list C2
	mat list C3
	mat list C4
	/*
#delimit
coefplot (matrix(C1[,1]), rename(r2=q1) ci((C1[,2] C1[,3])) m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(green)) ) 
(matrix(C2[,1]), rename(r2=q2) ci((C2[,2] C2[,3])) m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(green%75))) 
(matrix(C3[,1]), rename(r2=q3) ci((C3[,2] C3[,3])) m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(green%50))) 
(matrix(C4[,1]), rename(r2=q4) ci((C4[,2] C4[,3])) m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(green%50))), drop(r1)
yline(0, lcolor(gs0)) recast(line) vertical graphregion(fcolor(white)) offset(.01)
 name(testhigh, replace);
#delimit cr
*/

* TWFE and Clean controls ATEs
loc var gini_can
#delimit
coefplot (matrix(C1[,1]), rename(r2=q1) ci((C1[,2] C1[,3])) m(O) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)) ) 
(matrix(C2[,1]), rename(r2=q2) ci((C2[,2] C2[,3])) m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
(matrix(C3[,1]), rename(r2=q3) ci((C3[,2] C3[,3])) m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
(matrix(C4[,1]), rename(r2=q4) ci((C4[,2] C4[,3])) m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange))), drop(r1)
				yline(0, lcolor(gs0)) vertical ylabel(${ylab})
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) legend(off) ;
 graph export "$figs/het_impact_`var'_`shock'_clean_controls_`switchers'.png", replace ;
#delimit cr
}


preserve
matrix drop _all
loc g=4
local nume 3
loc nump 3
loc num=`nume'+`nump'+1
loc range "p, 18, 26"
*loc range "p, 27, 33"

forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(`range') & quant_gini_can==`q', effects(`nume') placebo(`nump')  trends_nonparam(canton)


		matrix C`q' = J(`num',4,.)
loc i `nump'
forval p=1/`nump'{
sca ate_`q'_`p'=e(Placebo_`i')	
sca se_ate_`q'_`p'=e(se_placebo_`i')
loc --i
}

loc plus=`nump'+1
sca ate_`q'_`plus'=0
sca se_ate_`q'_`plus'=0

forval p=1/`nume'{
loc plus=`nump'+`p'+1
sca ate_`q'_`plus'=e(Effect_`p')	
sca se_ate_`q'_`plus'=e(se_effect_`p')
}
forval t=1/`num' {
        matrix C`q'[`t',1] = ate_`q'_`t',  se_ate_`q'_`t', ate_`q'_`t' + se_ate_`q'_`t'*1.96, ate_`q'_`t' - se_ate_`q'_`t'*1.96
matrix list C`q'
}
}



	mat list C1
	mat list C2
	mat list C3
	mat list C4
	matrix C = C1 , C2, C3, C4
	mat list C

		forval q=1/`g' {
matrix  D`q' =C`q'[1,1] - C1[1,1], (C`q'[1,1] - C1[1,1]) + (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])*1.96, (C`q'[1,1] - C1[1,1]) - (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])*1.96, (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])
				forval r=2/`num' {
matrix  D`q' =D`q' \ C`q'[`r',1] - C1[`r',1], (C`q'[`r',1] - C1[`r',1]) + (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])*1.96, (C`q'[`r',1] - C1[`r',1]) - (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])*1.96, (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])
}
			mat list D`q'
}

mat D= D1,D2,D3,D4
mat list D


loc var gini_can
#delimit
coefplot (matrix(D1[,1]),  ci((D1[,2] D1[,3])) m(O) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)) ) 
(matrix(D2[,1]),  ci((D2[,2] D2[,3])) m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
(matrix(D3[,1]),  ci((D3[,2] D3[,3])) m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
(matrix(D4[,1]),  ci((D4[,2] D4[,3])) m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange))),
				xlabel(1 "w-4" 2 "w-3" 
				3 "w-2" 4 "w-1" 5 "w" 
				6 "w+1" 7 "w+2")
				yline(0, lcolor(gs0)) vertical 
				xline(4, lcolor(red) lpattern(shortdash))
				ytitle("") xtitle("") scheme(plotplainblind) legend(order(2 "q1" 4 "q2" 6 "q3" 8 "q4" ) pos(0) bplace(sw) ) ;
				 graph export "$figs/het_impact_`var'_`shock'_clean_controls_trends.png", replace ;
#delimit cr

coefplot (matrix(C1[,1]), rename(r2=q1) ci((C1[,2] C1[,3])) m(O) mc(navy)  ciopts(lpattern(shortdash) lcolor(navy)) ) 
(matrix(C2[,1]), rename(r2=q2) ci((C2[,2] C2[,3])) m(D) mc(maroon)  ciopts(lpattern(shortdash) lcolor(maroon)))
(matrix(C3[,1]), rename(r2=q3) ci((C3[,2] C3[,3])) m(T) mc(forest_green)  ciopts(lpattern(shortdash) lcolor(forest_green)))
(matrix(C4[,1]), rename(r2=q4) ci((C4[,2] C4[,3])) m(S) mc(dkorange)  ciopts(lpattern(shortdash) lcolor(dkorange))), drop(r1)
				yline(0, lcolor(gs0)) vertical ylabel(${ylab})
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) legend(off) ;

foreach var in dherf dcount fsize gini_can seminat rat_perim_area semi_pa_rat dfct_r {
	cap drop l`var'
	gen l`var'=ln(`var')
}


foreach var in gini_can {
loc range "p, 18, 26"

estimates clear
eststo M0: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var') if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M1: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M2: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M3: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf c.lseminat) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M4: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf c.lsemi_pa_rat) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid

/*estimates clear
eststo M0: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var') if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M1: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M2: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M3: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M4: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf quant_semi_pa_rat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
*/
		#delimit
			coefplot M*  , keep(c.the_shock_s#c.l`var') 
			recast(bar)
				yline(0, lcolor(gs0)) vertical barwidth(.2)
				ciopts(/*recast(rline) */lpattern(dash)) xlabel("") /*ylabel(.1(-.05)-.2)*/
				ytitle("Elasticity(GPP,x) under shock") xtitle("") scheme(plotplainblind) 
				name(mech`var',replace) legend(order(2 "No controls" 4 "Shock x (Num. Crops)" 6 "Shock x (Num. Crops + Crop concentration (HHI))" 
					8 "Shock x (Num. Crops + Crop concentration (HHI) + Seminatural areas)" 10 "Shock x (Num. Crops + Crop concentration (HHI) + (Seminatural areas) x (Perim/Area))") pos(6) row(5) size(vsmall) );
			graph export "$figs/mech_elasticities_`var'.png", replace ;
		#delimit cr

}




foreach var in gini_can {
loc range "p, 18, 26"

estimates clear
eststo M: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var') if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M0: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.lfsize) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M1: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.lfsize c.ldcount) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M2: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var'  c.lfsize c.ldcount c.ldherf) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M3: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var'  c.lfsize c.ldcount c.ldherf c.lseminat) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid
eststo M4: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var'  c.lfsize c.ldcount c.ldherf c.lsemi_pa_rat) if inrange(`range'), absorb(canton##year p) vce(cl canton#year) resid

/*estimates clear
eststo M0: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var') if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M1: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M2: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M3: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M4: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' quant_dcount quant_dherf quant_semi_pa_rat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
*/
		#delimit
			coefplot M*  , keep(c.the_shock_s#c.l`var') 
			recast(bar)
				yline(0, lcolor(gs0)) vertical barwidth(.2)
				ciopts(/*recast(rline) */lpattern(dash)) xlabel("") /*ylabel(.1(-.05)-.2)*/
				ytitle("Elasticity(GPP,x) under shock") xtitle("") scheme(plotplainblind) 
				name(mech`var',replace) legend(order(2 "No controls" 4 "Shock x (Num. Crops)" 6 "Shock x (Num. Crops + Crop concentration (HHI))" 
					8 "Shock x (Num. Crops + Crop concentration (HHI) + Seminatural areas)" 10 "Shock x (Num. Crops + Crop concentration (HHI) + (Seminatural areas) x (Perim/Area))") pos(6) row(5) size(vsmall) );
			graph export "$figs/mech_elasticities_`var'.png", replace ;
		#delimit cr

}



loc g=4
local nume 1
loc nump 0
loc num=`nume'+`nump'+1
loc range "p, 18, 26"
*loc range "p, 27, 33"

forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(`range') & quant_gini_can==`q' , placebo(`nump') effects(`nume') trends_nonparam(canton) 
		matrix C`q' = J(`num',3,.)
loc i `nump'
forval p=1/`nump'{
sca ate_`q'_`p'=e(Placebo_`i')	
sca se_ate_`q'_`p'=e(se_placebo_`i')
loc --i
}
loc plus=`nump'+1
sca ate_`q'_`plus'=0
sca se_ate_`q'_`plus'=0

forval p=1/`nume'{
loc plus=`nump'+`p'+1
sca ate_`q'_`plus'=e(Effect_`p')	
sca se_ate_`q'_`plus'=e(se_effect_`p')
}
forval t=1/`num' {
        matrix C`q'[`t',1] = ate_`q'_`t', ate_`q'_`t' + se_ate_`q'_`t'*1.96, ate_`q'_`t' - se_ate_`q'_`t'*1.96
matrix list C`q'
}
}

	matrix C = C1 , C2, C3, C4
	mat list C
	mat list C1
	mat list C2
	mat list C3
	mat list C4
#delimit
coefplot (matrix(C1[,1]), ci((C1[,2] C1[,3])) m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(green)) ) 
(matrix(C2[,1]), ci((C2[,2] C2[,3])) m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(green%75))) 
(matrix(C3[,1]), ci((C3[,2] C3[,3])) m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(green%50))) 
(matrix(C4[,1]), ci((C4[,2] C4[,3])) m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(green%50))), 
yline(0, lcolor(gs0)) recast(line) vertical graphregion(fcolor(white)) offset(.01)
 name(test, replace);
#delimit cr

ta p month
preserve
matrix drop _all
loc g=4
local nume 3
loc nump 3
loc num=`nume'+`nump'+1
loc range "p, 18, 27"
*loc range "p, 27, 33"

forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p w if inrange(`range') & quant_gini_can==`q', effects(`nume') placebo(`nump')  trends_nonparam(canton)


		matrix C`q' = J(`num',4,.)
loc i `nump'
forval p=1/`nump'{
sca ate_`q'_`p'=e(Placebo_`i')	
sca se_ate_`q'_`p'=e(se_placebo_`i')
loc --i
}

loc plus=`nump'+1
sca ate_`q'_`plus'=0
sca se_ate_`q'_`plus'=0

forval p=1/`nume'{
loc plus=`nump'+`p'+1
sca ate_`q'_`plus'=e(Effect_`p')	
sca se_ate_`q'_`plus'=e(se_effect_`p')
}
forval t=1/`num' {
        matrix C`q'[`t',1] = ate_`q'_`t',  se_ate_`q'_`t', ate_`q'_`t' + se_ate_`q'_`t'*1.96, ate_`q'_`t' - se_ate_`q'_`t'*1.96
matrix list C`q'
}
}



	mat list C1
	mat list C2
	mat list C3
	mat list C4
	matrix C = C1 , C2, C3, C4
	mat list C

		forval q=1/`g' {
matrix  D`q' =C`q'[1,1] - C1[1,1], (C`q'[1,1] - C1[1,1]) + (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])*1.96, (C`q'[1,1] - C1[1,1]) - (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])*1.96, (C`q'[1,2] - C1[1,2])/sqrt(C`q'[1,2] + C1[1,2])
				forval r=2/`num' {
matrix  D`q' =D`q' \ C`q'[`r',1] - C1[`r',1], (C`q'[`r',1] - C1[`r',1]) + (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])*1.96, (C`q'[`r',1] - C1[`r',1]) - (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])*1.96, (C`q'[`r',2] - C1[`r',2])/sqrt(C`q'[`r',2] + C1[`r',2])
}
			mat list D`q'
}

mat D= D1,D2,D3,D4
mat list D


#delimit
coefplot 
(matrix(D1[,1]), ci((D1[,2] D1[,3])) m(dh) mc(gs9)  ciopts(lpattern(shortdash) lcolor(green))) 
(matrix(D2[,1]), ci((D2[,2] D2[,3])) m(dh) mc(gs6)  ciopts(lpattern(shortdash) lcolor(green%75))) 
(matrix(D3[,1]), ci((D3[,2] D3[,3])) m(dh) mc(gs3)  ciopts(lpattern(shortdash) lcolor(green%50))) 
(matrix(D4[,1]), ci((D4[,2] D4[,3])) m(dh) mc(gs0) ciopts(lpattern(shortdash) lcolor(green%50))), 
yline(0, lcolor(gs0))  vertical graphregion(fcolor(white)) 
 name(dtest, replace);
#delimit cr


ta min_p
preserve
keep if inrange(p, 18, 24)
replace the_shock_s_y=min_p<23
collapse (mean) lgpp_${inter} the_shock_s_y quant_gini_can, by(canton year)
xtset canton year
did_multiplegt_dyn lgpp_${inter} canton year the_shock_s_y, by(quant_gini_can) placebo(2) effects(2)
eststo base: reghdfe cgpp_${inter} (F(2/1).the_shock_s_y the_shock_s_y L(1/1).the_shock_s_y)##i.quant_gini_can, a(canton year) vce(cl canton) 

