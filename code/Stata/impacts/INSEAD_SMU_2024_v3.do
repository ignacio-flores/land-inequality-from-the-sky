global dir "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
/*
global tabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky (LITE)/tabs"
global figs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky (LITE)/figures"
*/
global tabs "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/offline/tabs"
global figs "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/offline/figures"
*global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
global rep "$dir"
cd "$dir"

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



encode dept, g(ndept)
cap drop r_gpp_${inter}
reghdfe gpp_${inter}, absorb(ndept##year) resid
predict r_gpp_${inter}, r
levelsof quant_gini_can, l(q)
foreach x in `q' {
	sum gpp_${inter} if quant_gini_can==`x'
replace r_gpp_${inter}=r_gpp_${inter}+r(mean) if quant_gini_can==`x'
}
so canton t
cap drop r_cgpp_${inter}
byso canton year: gen r_cgpp_${inter}=sum(r_gpp_${inter})
cap drop pmax
byso canton year: egen pmax=max(p)
ta pmax

foreach var of var gini_can fsize {
	cap drop cut_`var'
sum `var', d
	gen cut_`var'=r(p99)
}  
cap drop canicule
gen canicule=hshock_avg>=5

reghdfe gpp_w c.hshock_avg##i.quant_gini_can if inrange(p, 11, 33), vce(cl canton)
reghdfe gpp_w c.hshock_avg##i.quant_gini_can if inrange(p, 11, 33), a(canton) vce(cl canton)


reghdfe gpp_w c.hshock_avg if inrange(p, 11, 33), vce(cl canton)

reghdfe gpp_w c.hshock_avg c.seminat if inrange(p, 11, 33),  vce(cl canton)
reghdfe gpp_w i.hshock_avg##c.seminat if inrange(p, 11, 33),   vce(cl canton)
reghdfe gpp_w i.hshock_avg##c.seminat if inrange(p, 11, 33), a(canton)  vce(cl canton)

reghdfe gpp_w the_shock_s##c.seminat if inrange(p, 11, 33),  vce(cl canton)

cap drop rseminat
gen rseminat=round(seminat*100/20,1)
ta rseminat
reghdfe gpp_w the_shock_s##b0.rseminat if inrange(p, 11, 33),  vce(cl canton)
reghdfe gpp_w the_shock_s##b0.rseminat if inrange(p, 11, 33),  vce(cl canton)

xtset canton t

reghdfe gpp_w the_shock_s##b0.rseminat if inrange(p, 11, 33),  vce(cl canton)


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

binsreg cgpp_${inter}_pmax gini_can if gini_can<=cut_gini_can, absorb(ndept##year) line(1 1) xtitle(Land Gini) ytitle(Annual GPP in C.Mt/ha) ///
scheme(plotplainblind) name(gini_can , replace)
graph export "$figs/food_agg_prod_by_gini.png", replace 
binsreg cgpp_${inter}_pmax fsize  if fsize<=cut_fsize, absorb(ndept##year) line(1 1) xtitle(Avg Farm Size in ha) ytitle(Annual GPP in C.Mt/ha) ///
scheme(plotplainblind) name(fsize , replace)
graph export "$figs/food_agg_prod_by_fsize.png", replace 

binsreg cgpp_${inter} gini_can if p==pmax & gini_can<=cut_gini_can, absorb(ndept##year) line(1 1) xtitle(Land Gini) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(gini_can , replace)
graph export "$figs/food_prod_by_gini.png", replace 
binsreg cgpp_${inter} fsize  if p==pmax & fsize<=cut_fsize, absorb(ndept##year) line(1 1) xtitle(Avg Farm Size in ha) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(fsize , replace)
graph export "$figs/food_prod_by_fsize.png", replace 

binsreg cgpp_${inter}_pmax quant_gini_can , absorb(ndept##year) line(1 1) xtitle(Land Gini) ytitle(Annual GPP in C.Mt/ha) ///
scheme(plotplainblind) name(gini_can , replace)
*graph export "$figs/food_agg_prod_by_gini.png", replace 
binsreg cgpp_${inter}_pmax quant_fsize  , absorb(ndept##year) line(1 1) xtitle(Avg Farm Size in ha) ytitle(Annual GPP in C.Mt/ha) ///
scheme(plotplainblind) name(fsize , replace)
*graph export "$figs/food_agg_prod_by_fsize.png", replace 

binsreg cgpp_${inter} gini_can if p==pmax & gini_can<=cut_gini_can, absorb(ndept##year) line(1 1) xtitle(Land Gini) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(gini_can , replace)
graph export "$figs/food_prod_by_gini.png", replace 
binsreg cgpp_${inter} fsize  if p==pmax & fsize<=cut_fsize, absorb(ndept##year) line(1 1) xtitle(Avg Farm Size in ha) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(fsize , replace)
graph export "$figs/food_prod_by_fsize.png", replace 


binsreg cgpp_${inter} gini_can if p==pmax & gini_can<=cut_gini_can, absorb(ndept##year) line(1 1) xtitle(Land Gini) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(gini_can , replace)
*graph export "$figs/food_prod_by_gini.png", replace 
binsreg cgpp_${inter} fsize  if p==pmax & fsize<=cut_fsize, absorb(ndept##year) line(1 1) xtitle(Avg Farm Size in ha) ytitle(Annual GPP in C.kg/m{superscript:2}) ///
scheme(plotplainblind) name(fsize , replace)
*graph export "$figs/food_prod_by_fsize.png", replace 


* Centering controls
foreach var of var crop1-crop28 {
	cap drop c_`var'
	sum `var'
	gen c_`var'=`var'-r(mean)
}

estimates clear
cap drop tquant
gen tquant=.
foreach x in gini_can fsize {
replace tquant=quant_`x'
eststo c_`x'_p: reghdfe cgpp_${inter}_p i.tquant if p==pmax, absorb(ndept##year) vce(cl canton)
estadd local hascon0 "$\checkmark$"
estadd local hascon1 ""
eststo c_`x'_p_c: reghdfe cgpp_${inter}_p i.tquant c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
estadd local hascon0 "$\checkmark$"
estadd local hascon1 "$\checkmark$"
eststo c_`x': reghdfe cgpp_${inter} i.tquant if p==pmax, absorb(ndept##year) vce(cl canton)
estadd local hascon0 "$\checkmark$"
estadd local hascon1 ""
eststo c_`x'_c: reghdfe cgpp_${inter} i.tquant c_crop1-c_crop28 if p==pmax, absorb(ndept##year) vce(cl canton)	
estadd local hascon0 "$\checkmark$"
estadd local hascon1 "$\checkmark$"
}


		loc stats ///
			"stats(r2 N hascon0 hascon1, label("R-squared" "N" "(Geo x Year) FEs" "Crop types") fmt(%9.2f %9.0f))"

		#delimit
			esttab c_gini_can_p c_gini_can_p_c c_fsize_p c_fsize_p_c  using 
				"$tabs/food_agg_prod_by_q.tex",  
				f nomti noli collabels(none) 
				keep(_cons  *.tquant) drop(1.tquant) order(_cons  *.tquant)
				varlabels(_cons "mean 1st quantile (ref)"
				 2.tquant "2nd quantile" 
				  3.tquant "3rd quantile"  4.tquant "4th quantile" ) mgroups("Land Gini" "Farm Size", pattern(1 0 1 0)
				  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
				`stats' style(tex) noobs cells(b(fmt(0) star) 
				se(par fmt(0))) star(* .1 ** .05 *** .01) replace;
		#delimit cr

		#delimit
			esttab c_gini_can c_gini_can_c c_fsize c_fsize_c  using 
				"$tabs/food_prod_by_q.tex",  
				nomti noli collabels(none) 
				keep(_cons  *.tquant) drop(1.tquant) order(_cons  *.tquant) 
				varlabels(_cons "mean 1st quantile (ref)"
				 2.tquant "2nd quantile" 
				  3.tquant "3rd quantile"  4.tquant "4th quantile") mgroups("Land Gini" "Farm Size", pattern(1 0 1 0)
				  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
				`stats' style(tex) noobs cells(b(fmt(3) star) 
				se(par fmt(3))) star(* .1 ** .05 *** .01) replace;
		#delimit cr

* Productivity and temperature
cap drop cut
sum tsup_h_q if the_shock_s,d
global cut=r(min)
gen int cut=round(${cut})
binsreg gpp_${inter} tsup_h_q, by(quant_gini_can) absorb(canton##year) samebinsby line(1 1)  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Gini quantiles, size(small))   pos(0) bplace(nw)) 
graph export "$figs/gpp_temp_by_gini.png", replace 

/*
pause on
preserve
u data/FR/aux_files/bin_data1, clear
pause
*/

sum cut
loc cut=r(mean)
binsreg gpp_${inter} tsup_h_q if tsup_h_q>=`cut', by(quant_gini_can) absorb(canton##year) samebinsby polyreg(1) polyregcigrid(1) usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 4 "q2" 7 "q3" 10 "q4" )  title(Gini quantiles, size(small))   pos(0) bplace(ne)) 
graph export "$figs/gpp_temp_by_gini_zoom`cut'.png", replace 

binsreg gpp_${inter} tsup_h_q, by(quant_fsize) absorb(canton##year) samebinsby line(1 1)  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1_fsize) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(nw)) 
graph export "$figs/gpp_temp_by_fsize.png", replace 

sum cut
loc cut=r(mean)
binsreg gpp_${inter} tsup_h_q if tsup_h_q>=`cut', by(quant_fsize) absorb(canton##year) samebinsby polyreg(1) polyregcigrid(1) usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 4 "q2" 7 "q3" 10 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(ne)) 
graph export "$figs/gpp_temp_by_fsize_zoom`cut'.png", replace 


estimates clear
cap drop tquant
gen tquant=.
foreach x in gini_can fsize {
replace tquant=quant_`x'
eststo gpp_`x': reghdfe gpp_${inter} c.tsup_h_q##i.tquant, absorb(canton##year) vce(cl canton)
estadd local hascon0 "$\checkmark$"
eststo z_gpp_`x': reghdfe gpp_${inter} c.tsup_h_q##i.tquant if tsup_h_q>=${cut}, absorb(canton##year) vce(cl canton)
estadd local hascon0 "$\checkmark$"
}


		loc stats ///
			"stats(r2 N hascon0, label("R-squared" "N" "(Panel unit x Year) FEs") fmt(%9.2f %9.0f))"

	#delimit
			esttab gpp_gini_can z_gpp_gini_can   using 
				"$tabs/gpp_temp_by_gini.tex",  
				nomti noli collabels(none) 
				keep(tsup_h_q *.tquant#c.tsup_h_q) drop(1.tquant#c.tsup_h_q)
				varlabels(tsup_h_q "Temp. 1st quantile (ref)"
				 2.tquant#c.tsup_h_q "Temp. x 2nd quantile" 
				  3.tquant#c.tsup_h_q "Temp. x 3rd quantile"  4.tquant#c.tsup_h_q "Temp. x 4th quantile") mgroups("All" ">wgt. threshold", pattern(1 1))
				`stats' style(tex) noobs cells(b(fmt(5) star) 
				se(par fmt(5))) star(* .1 ** .05 *** .01) replace;
		#delimit cr


estimates clear
cap drop tquant
gen tquant=.
foreach x in gini_can fsize {
replace tquant=quant_`x'
eststo gpp_`x': reghdfe gpp_${inter} c.tsup_h_q##i.tquant, absorb(canton#year##c.(c_crop1-c_crop28)) vce(cl canton)
estadd local hascon0 "$\checkmark$"
eststo z_gpp_`x': reghdfe gpp_${inter} c.tsup_h_q##i.tquant if tsup_h_q>=${cut}, absorb(canton#year##c.(c_crop1-c_crop28)) vce(cl canton)
estadd local hascon0 "$\checkmark$"
}



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
				if (quant_`var'==1 | quant_`var'==`q'), absorb(canton##year) vce(cl canton) resid

			}

	}

	global xlab
	forval x=1/14 {
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
	coefplot(`var'_2, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_3, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_4, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3))),
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

foreach var in dherf dcount fsize gini_can seminat rat_perim_area semi_pa_rat dfct_r {
	cap drop l`var'
	gen l`var'=ln(`var')
}
foreach var in gini_can fsize {


estimates clear
eststo M0: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var') if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M1: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M2: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M3: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf c.lseminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
eststo M4: reghdfe lgpp_${inter} c.the_shock_s##(c.l`var' c.ldcount c.ldherf c.lsemi_pa_rat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

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
					8 "Shock x (Num. Crops + Crop concentration (HHI) + Seminatural areas)" 10 "Shock x (Num. Crops + Crop concentration (HHI) + (Seminatural areas) x (Perim/Area)") pos(6) row(5) size(vsmall) );
			graph export "$figs/mech_elasticities_`var'.png", replace ;
		#delimit cr

}



foreach var in $het_vars {



cap drop `var'_med
cap n gen `var'_med=quant_`var'>2 if quant_`var'!=.
cap drop  wgini_med_`var'
cap n gen wgini_med_`var'=wgini_quant_`var'>2
}

		estimates clear
* ATEs gini and farm sizes moderated

foreach shock in the_shock_s  {

	loc loop 1
foreach var in gini_can fsize {

	foreach mod in wgini_med_seminat wgini_med_semi_pa_rat {
/*
reghdfe lgpp_${inter} `shock'##quant_`var' if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
testparm `shock' `shock'#quant_`var' , equal
		cap n	 sca pval_id=round(r(p), .0001)

reghdfe lgpp_${inter} `shock'##quant_`var'##`mod' if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
testparm  `shock'#quant_`var' `shock'#quant_`var'#`mod'

		cap n	sca pval_id1=round(r(p), .0001)
*/
	loc q=4
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=`shock'*q_`var'`x'
	}

		cap n	eststo `var'Q: reghdfe lgpp_${inter} t_q_`var'? if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		testparm t_q_`var'* , equal
		cap n	eststo H`var'Q: reghdfe lgpp_${inter} t_q_`var'? if `mod' & inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		testparm t_q_`var'* , equal
		cap n	eststo L`var'Q: reghdfe lgpp_${inter} t_q_`var'? if !`mod' & inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
testparm t_q_`var'* , equal
		#delimit
			coefplot (`var'Q, mcolor(green%50) ciopts(/*recast(rline) */lpattern(dash) lcolor(green%50))), drop(_cons)
				xlabel(1 "1st qtile" 2 "2nd qtile" 
				3 "3rd qtile" 4 "4th qtile")
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(`var'_`loop',replace) legend(off) note(p-val jointly equal: 0`=pval_id', pos(7) size(small));
			graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr


		#delimit
			coefplot (`var'Q, mcolor(green%50)  ciopts(/*recast(rline) */lpattern(dash) lcolor(green%50)) ) (H`var'Q, mcolor(green) lcolor(green) ciopts(/*recast(rline) */lpattern(dash) lcolor(green)))   
			(L`var'Q, mcolor(gs7%50) ciopts(/*recast(rline) */lpattern(dash) lcolor(gs7%50))) , drop(_cons)
				xlabel(1 "1st qtile" 2 "2nd qtile" 
				3 "3rd qtile" 4 "4th qtile")
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) recast(connected)
				name(`var'_het_`loop',replace) legend(order(2 "All" 4 "High Seminat" 6 "Low Seminat")) note(p-val jointly equal: 0`=pval_id1', pos(7) size(small));
			graph export "$figs/het_impact_`var'_`shock'_`mod'.png", replace ;
		#delimit cr

loc ++loop
}
}
}




		estimates clear
* ATEs gini and farm sizes
foreach shock in the_shock_s  {


foreach var in gini_can  {
	foreach mod in wgini_med_seminat {

reghdfe lgpp_${inter} c.the_shock_s##b1.quant_gini_can if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can quant_seminat quant_fsize) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can quant_seminat quant_fsize quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid


reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat quant_dherf quant_dfct_r) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.rat_perim_area) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.rat_perim_area c.seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.rat_perim_area c.seminat c.dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.rat_perim_area c.seminat c.dherf c.dfct_r) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.seminat c.fsize) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat quant_fsize quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat quant_fsize quant_dherf quant_dfct_r) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid


reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can quant_seminat quant_fsize quant_dherf quant_dfct_r) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid


reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can wgini_quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can wgini_quant_dherf) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid
reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can wgini_quant_quant_dfct_r) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid


reghdfe lgpp_${inter} c.the_shock_s##b1.(quant_gini_can##wgini_quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid

reghdfe lgpp_${inter} c.the_shock_s##(c.gini_can c.seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton) resid




* ATEs gini and farm sizes
foreach shock in the_shock_s  {


foreach var in gini_can fsize {
		estimates clear
	loc q=4
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=`shock'*q_`var'`x'
				capture drop t_q_`var'`x'_snat
		gen t_q_`var'`x'_snat=`shock'*q_`var'`x'*wgini_med_seminat
	}

		cap n	eststo `var'Q: reghdfe lgpp_${inter} t_q_`var'* if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		*cap n	eststo H`var'Q: reghdfe lgpp_${inter} t_q_`var'? hu_q if  inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		*cap n	eststo L`var'Q: reghdfe lgpp_${inter} t_q_`var'? hu_q if  inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		/*
				cap n	eststo `var'Q: reghdfe lgpp_${inter} t_q_`var'? hu_q if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		cap n	eststo H`var'Q: reghdfe lgpp_${inter} t_q_`var'? hu_q if wgini_med_seminat & inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		cap n	eststo L`var'Q: reghdfe lgpp_${inter} t_q_`var'? hu_q if !wgini_med_seminat & inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
*/
		cap n	testparm t_q_`var'*, equal
		cap n	estadd sca pval_id=round(r(p), .01)

			#delimit
			coefplot `var'Q , drop(_cons)
				xlabel(1 "1st qtile" 2 "2nd qtile" 
				3 "3rd qtile" 4 "4th qtile")
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(het_impact_`var'_`shock',replace) legend(off);
			graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr
	

/*
		#delimit
			coefplot `var'Q (H`var'Q, mcolor(green) ciopts(/*recast(rline) */lpattern(dash) lcolor(green)))   L`var'Q , drop(_cons hu_q)
				xlabel(1 "1st qtile" 2 "2nd qtile" 
				3 "3rd qtile" 4 "4th qtile")
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(`var'Q_hu,replace) legend(order(1 "All" 3 "High Seminat" 5 "Low Seminat"));
			graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr
		*/
}
}


gen wgini_med_seminat=wgini_quant_seminat>2
cap drop gini_med
gen gini_med=quant_gini_can>2
cap drop seminat_med
gen seminat_med=quant_seminat>2

reghdfe lgpp_${inter} the_shock_s##gini_med if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(gini_med wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can##wgini_quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} canicule##(gini_med##wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

reghdfe lgpp_${inter} the_shock_s##gini_med if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(gini_med wgini_med_fsize) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(gini_med##wgini_med_fsize) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

levelsof year, l(yr)
foreach y in `yr' {
reghdfe lgpp_${inter} the_shock_s##(gini_med##wgini_med_seminat) if inrange(p, 11, 33) & year==`y', absorb(canton##year) vce(cl canton)
}

sum temp_m if inrange(p, 11, 33),d
sum hu_q if inrange(p, 11, 33),d
loc cut=r(p75)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can) if inrange(p, 11, 33) & hu_q<`cut', absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can##wgini_med_seminat) if inrange(p, 11, 33) & hu_q<`cut', absorb(canton##year) vce(cl canton)

sum hu_q if the_shock_s,d
testparm the_shock_s#quant_gini_can the_shock_s#quant_gini_can#wgini_quant_seminat, equal



reghdfe lgpp_${inter} the_shock_s##(gini_med##wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)


reghdfe lgpp_${inter} the_shock_s##(gini_med seminat_med) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)


reghdfe lgpp_${inter} the_shock_s##(gini_med seminat_med dherf_med) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(gini_med seminat_med dcount_med) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)


reghdfe lgpp_${inter} the_shock_s##gini_med if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(gini_med seminat_med) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

reghdfe lgpp_${inter} the_shock_s##(gini_med##wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

reghdfe lgpp_${inter} the_shock_s##(gini_med##wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)



testparm the_shock_s#gini_med the_shock_s#gini_med#wgini_med_seminat

binscatter the_shock_s year
binscatter gini_can year

reghdfe lgpp_${inter} the_shock_s##quant_gini_can if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can##wgini_quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
reghdfe lgpp_${inter} the_shock_s##(quant_gini_can##wgini_med_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

testparm the_shock_s#quant_gini_can#wgini_quant_seminat

reghdfe gpp_${inter} the_shock_s##(quant_gini_can##wgini_quant_seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)

reghdfe lgpp_${inter} the_shock_s##(quant_gini_can c.seminat) if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)



* Evolution of land consolidation https://www.resoilag.com/blog/panorama-et-enjeux-de-l-agriculture-en-france

* ATEs gini and farm sizes controling for cropc compo
foreach var in gini_can fsize {
		estimates clear
	loc q=4
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=the_shock_s*q_`var'`x'
	}

		cap n	eststo `var'Q: reghdfe gpp_${inter} t_q_`var'? if inrange(p, 11, 33), absorb(canton##c.(c_crop1-c_crop28)) vce(cl canton)
		cap n	testparm t_q_`var'*, equal
		cap n	estadd sca pval_id=round(r(p), .01)
	


		#delimit
			coefplot `var'Q , drop(_cons)
				xlabel(1 "1st quintile" 2 "2nd quintile" 
				3 "3rd quintile" 4 "4th quintile")
				yline(0, lcolor(gs0)) vertical mcolor(gs5) 
				ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(`var'Q_cont,replace);
			graph export "$figs/het_impact_`var'_cont.png", replace ;
		#delimit cr
}

reghdfe gpp_w the_shock_s##i.quant_gini_can if inrange(p, 11, 33),  a(canton)  vce(cl canton)
reghdfe gpp_w the_shock_s##i.quant_gini_can##i.wgini_quant_seminat if inrange(p, 11, 33),  a(canton)  vce(cl canton)
reghdfe gpp_w the_shock_s##i.quant_gini_can##i.wgini_quant_seminat if inrange(p, 11, 33),  a(canton##year)  vce(cl canton)

reghdfe gpp_w the_shock_s##i.quant_gini_can##i.wgini_quant_seminat if inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe lgpp_w the_shock_s##i.quant_gini_can##i.wgini_quant_seminat hu_q if inrange(p, 11, 33),  a(canton##year)  vce(cl canton)


reghdfe gpp_w c.tsup_h_q##i.quant_gini_can hu_q if tsup_h_q>=${cut} & inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe gpp_w c.tsup_h_q##i.quant_gini_can##i.wgini_quant_seminat hu_q if tsup_h_q>=${cut} & inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe gpp_w canicule##i.quant_gini_can if inrange(p, 11, 33),  a(canton##year)  vce(cl canton)

dis ${cut}

ta tsup_h_q_r
preserve
gen wgini_med_seminat=wgini_quant_seminat>2
reghdfe lgpp_w the_shock_s##i.quant_gini_can  if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe lgpp_w the_shock_s##i.quant_gini_can  t_q if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)

reghdfe lgpp_w c.tsup_h_q##i.quant_gini_can  if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe lgpp_w c.tsup_h_q##i.quant_gini_can  if tsup_h_q>=${cut} & inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe lgpp_w the_shock_s##i.quant_gini_can##wgini_med_seminat  if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)

reghdfe lgpp_w c.canicule##i.quant_gini_can  if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe lgpp_w c.canicule##i.quant_gini_can##wgini_med_seminat  if  inrange(p, 11, 33),  a(canton##year)  vce(cl canton)


reghdfe gpp_w tsup_h_q_r##i.quant_gini_can hu_q if tsup_h_q_r>=${cut} & inrange(p, 11, 33),  a(canton##year)  vce(cl canton)
reghdfe gpp_w tsup_h_q_r##i.quant_gini_can hu_q if tsup_h_q_r>=${cut} & inrange(p, 11, 33) & wgini_quant_seminat<=2,  a(canton##year)  vce(cl canton)
reghdfe gpp_w tsup_h_q_r##i.quant_gini_can hu_q if tsup_h_q_r>=${cut} & inrange(p, 11, 33) & wgini_quant_seminat>2,  a(canton##year)  vce(cl canton)


cap drop crop_cereals
egen crop_cereals=rowtotal(crop1 crop2 crop3 crop4)
cap drop med_cereals
gen med_cereals=0
sum crop_cereals,d
forval q=1/4 {
sum crop_cereals if quant_gini_can==`q',d
replace med_cereals=1 if quant_gini_can==`q' & crop_cereals>r(p50)
}


* ATEs gini and farm sizes by cereal production
foreach var in gini_can fsize {
	estimates clear
	loc q=4
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=the_shock_s*q_`var'`x'
	}

forval m=0/1 {
			cap n	eststo `var'Q`m': reghdfe gpp_${inter} t_q_`var'? if med_cereals==`m' & inrange(p, 11, 33), absorb(canton##year) vce(cl canton)
		cap n	testparm t_q_`var'*, equal
		cap n	estadd sca pval_id=round(r(p), .01)
}

	


		#delimit
			coefplot (`var'Q0) (`var'Q1), drop(_cons)
				xlabel(1 "1st quintile" 2 "2nd quintile" 
				3 "3rd quintile" 4 "4th quintile")
				yline(0, lcolor(gs0)) vertical mcolor(gs5) 
				ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(`var'Q_med,replace)
				legend(order( 2 "Low cereal prod." 4 "High cereal prod.")  pos(0) bplace(ne));
			graph export "$figs/het_impact_`var'_cereal.png", replace ;
		#delimit cr
}

reghdfe evap_q i.quant_gini_can pe_q if inrange(p, 11, 33), absorb(ndept##year) vce(cl canton)

reghdfe tsup_h_q i.quant_gini_can if inrange(p, 11, 33), absorb(ndept##month) vce(cl canton)


reghdfe evap_q i.quant_gini_can crop1-crop28 pe_q if inrange(p, 11, 33), absorb(ndept##year) vce(cl canton)

binscatter evap_q quant_gini_can if inrange(p, 11, 33), line(connect)
binscatter evap_q quant_fsize if inrange(p, 11, 33), line(connect)
binscatter evap_q tsup_h_q if inrange(p, 11, 33), line(connect)

binscatter pe_q evap_q if inrange(p, 11, 33), line(connect)

reghdfe gpp_${inter} the_shock_s##i.quant_gini_can if inrange(p, 11, 33), absorb(canton##year) vce(cl canton)


reghdfe gpp_${inter} the_shock_s##i.quant_fsize if med_cereals, absorb(canton##year) vce(cl canton)

reghdfe gpp_${inter} the_shock_s##i.quant_gini_can if med_cereals, absorb(canton##year) vce(cl canton)
reghdfe gpp_${inter} the_shock_s##i.quant_gini_can if !med_cereals, absorb(canton##year) vce(cl canton)



cap drop crop_cereals
egen crop_cereals=rowtotal(crop1 crop2 crop3 crop4)
cap drop med_cereals
gen med_cereals=0
sum crop_cereals,d
forval q=1/4 {
sum crop_cereals if quant_gini_can==`q',d
replace med_cereals=1 if quant_gini_can==`q' & crop_cereals>r(p50)
}





eststo: reghdfe cgpp_${inter}_p c.gini_can##i.quant_gini_can, absorb(ndept##year) vce(cl canton)



binsreg cgpp_${inter} gini_can crop1-crop28 area_agri if p==pmax,  polyreg(2) 
binsreg cgpp_${inter} fsize crop1-crop28 area_agri if p==pmax & fsize<15, absorb(ndept##year) polyreg(2) polyregcigrid(1)


binsreg cgpp_${inter} gini_can if p==pmax, absorb(ndept##year) polyreg(3) polyregcigrid(1)
binsreg cgpp_${inter} fsize if p==pmax & fsize<15, absorb(ndept##year) polyreg(3) polyregcigrid(1)


binscatter r_cgpp_${inter} quant_fsize if p==pmax,  line(connect) 
binscatter r_cgpp_${inter} quant_gini_can if p==pmax,  line(connect) 




* But where do we get our food from?
cap drop gpp_${inter}_p
gen gpp_${inter}_p=farm_area_est*gpp_${inter}
cap drop cgpp_${inter}_p
gen cgpp_${inter}_p=farm_area_est*cgpp_${inter}
byso quant_gini_can: sum cgpp_${inter}_p if p==pmax & med_cereals
byso quant_fsize: sum cgpp_${inter}_p if p==pmax & med_cereals
byso quant_nfarms_km2: sum cgpp_${inter}_p if p==pmax


binscatter gpp_${inter} rat_fsize ,  line(connect) 
binscatter cgpp_${inter}_p p, by(quant_gini_can)  line(connect)
binscatter cgpp_${inter}_p p, by(quant_fsize)  line(connect)
binscatter cgpp_${inter}_p p, by(quant_count_km2)  line(connect)
binscatter cgpp_${inter}_p quant_fsize if p==pmax,  line(connect)
binscatter r_cgpp_${inter} rat_fsize ,  line(connect)

binscatter r_gpp_${inter} nfarms ,  line(connect)  
binscatter r_gpp_${inter} nfarms_km2 ,  line(connect)  
binscatter r_cgpp_${inter} gini_can if p==pmax,line(connect) 
binsreg r_cgpp_${inter} gini_can if p==pmax
binsreg r_cgpp_${inter} nfarms  if p==pmax

binscatter fsize gini_can if p==pmax,line(connect) absorb(ndept)

cap drop rat_fsize
gen rat_fsize=fsize/nfarms
sum rat_fsize,d
binscatter r_cgpp_${inter} rat_fsize if p==pmax, line(connect) absorb(canton)

binscatter cgpp_${inter} fsize if p==pmax, line(connect) absorb(canton)


binsreg r_cgpp_${inter} p, by(quant_gini_can)


binscatter r_cgpp_${inter} p if med_cereals, by(quant_gini_can)  line(connect)
binscatter r_cgpp_${inter} p if !med_cereals, by(quant_gini_can)  line(connect)
binscatter r_cgpp_${inter} gini_can if p==pmax,  by(med_cereals) line(connect)
binscatter r_cgpp_${inter} crop1 if p==pmax,  by(quant_gini_can) line(connect)
binscatter r_cgpp_${inter} fsize if p==pmax,  by(med_cereals) line(connect)

binscatter cgpp_${inter} fsize if p==pmax,  by(med_cereals) line(connect)


gen tsup_h_q_r=round(tsup_h_q)
binsreg gpp_${inter} gini_can , absorb(dept year month)
binsreg gpp_${inter} fsize, absorb(dept year month) /// area_agri fsize


binscatter r_gpp_${inter} gini_can, line(connect)
binscatter r_gpp_${inter} quant_gini_can, line(connect)

binscatter gpp_${inter} quant_gini_can, line(connect)
binscatter gpp_${inter} gini_can, line(connect)

binscatter gini_can max_crop , line(connect)

binscatter r_gpp_${inter} fsize , line(connect)


kdensity fsize

cap drop any_the_shock_y_${inter}
byso canton: egen any_the_shock_y_${inter}=max(the_shock_y_${inter})
binsreg gpp_${inter} gini_can crop1-crop28 area_agri if any_the_shock_y_${inter} & month>=6 & month<=9

binsreg gpp_${inter} tsup_h_q crop1-crop28 area_agri if month>=6 & month<=9, by(quant_gini_can)
binsreg gpp_${inter} tsup_h_q c.t if month>=6 & month<=9, by(quant_gini_can) absorb(canton) samebinsby
binsreg gpp_${inter} tsup_h_q hshock_avg if tsup_h_q>=29, by(quant_gini_can) absorb(canton##year) samebinsby polyreg(1) polyregcigrid(1)
binsreg gpp_${inter} tsup_h_q hshock_avg, by(quant_gini_can) absorb(canton##year) samebinsby

binsreg gpp_${inter} tsup_h_q if tsup_h_q>=31, by(quant_gini_can) absorb(canton##year) samebinsby polyreg(1) polyregcigrid(1)


binsreg gpp_${inter} gini_can crop1-crop28 area_agri, absorb(month dept)  line(0 0)
binsreg gpp_${inter} vlarge_farm, absorb(month)  polyreg(3) polyregcigrid(1)


reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can, absorb(i.canton i.month) vce(cl canton) resid

reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can, absorb(i.canton i.month) vce(cl canton) resid
reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can##c.dcount, absorb(i.canton i.month) vce(cl canton) resid

reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can##c.seminat c.t, absorb(i.canton i.month) vce(cl canton) resid

preserve
keep if tsup_h_q>=30
reghdfe gpp_${inter} c.tsup_h_q##c.tsup_h_q##i.quant_gini_can hshock_avg c.t, absorb(i.canton i.month) vce(cl canton) resid

sum tsup_h_q if the_shock_s,d
preserve
keep if tsup_h_q>=${cut}
reghdfe gpp_${inter} c.tsup_h_q##i.quant_gini_can, absorb(canton##year) vce(cl canton) resid
ta canton if e(sample)
reghdfe gpp_${inter} c.tsup_h_q##i.quant_gini_can crop1-crop28 area_agri if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid
reghdfe gpp_${inter} c.tsup_h_q##i.quant_gini_can##c.count_km2 if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid

reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid
reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can##b4.quant_count_km2 if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid

reghdfe gpp_${inter} b0.hshock_avg##i.quant_gini_can##c.dherf if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid
forval q=1/4 {
	reghdfe gpp_${inter} c.hshock_avg##i.quant_gini_can if month>=6 & month<=8 & quant_dherf==`q', absorb(canton##year) vce(cl canton) resid

}
cap drop dhshock_avg
gen dhshock_avg=hshock_avg>3
	reghdfe gpp_${inter} dhshock_avg##i.quant_gini_can if quant_count_km2<3 & month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid

	reghdfe gpp_${inter} dhshock_avg##i.quant_gini_can if  quant_count_km2>=3 & month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid

	reghdfe gpp_${inter} b0.hshock_avg##i.quant_gini_can if quant_count_km2<3, absorb(canton##year) vce(cl canton) resid

	reghdfe gpp_${inter} b0.hshock_avg##i.quant_gini_can if  quant_count_km2>=3, absorb(canton##year) vce(cl canton) resid

reghdfe gpp_${inter} b0.hshock_avg##i.quant_gini_can##b4.quant_dherf if month>=6 & month<=8, absorb(canton##year) vce(cl canton) resid


reghdfe gpp_${inter} hshock_avg##i.quant_gini_can hshock_avg c.t, absorb(i.canton i.month) vce(cl canton) resid



binscatter gpp_w tsup_h_q_r, by(quant_gini_can) line(connect) discrete
ta tsup_h_q_r
return list
binsreg gpp_w tsup_h_q if tsup_h_q>25, by(quant_gini_can) absorb(i.canton##i.year) name(bins,replace)
sum tsup_h_q if the_shock_w,d
preserve
keep if tsup_h_q_r>=25
*replace tsup_h_q_r=36 if tsup_h_q_r>36
cap drop p_gpp_w
reghdfe gpp_w b25.tsup_h_q_r##i.quant_gini_can if tsup_h_q_r , absorb(i.canton##i.year) vce(cl canton) resid
predict p_gpp_w, xb
coefplot, keep(*.tsup_h_q_r#*.quant_gini_can) vertical
binscatter p_gpp_w tsup_h_q_r if tsup_h_q_r, by(quant_gini_can) line(connect) name(pred,replace) discrete
restore

//loop over heterogeneity variables 
preserve
keep if tsup_h_q_r>=25
loc quant 4
foreach depvar in gpp_$inter  /*lgpp_$inter */ {
	estimates clear

foreach var in gini_can /*dherf herfindahl dcount count_km2 */ {
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
				eststo `var'_`q': reghdfe `depvar' b25.tsup_h_q_r##var_q  ///
				if (quant_`var'==1 | quant_`var'==`q') & quant_count_km2>=3, absorb(canton##year) vce(cl canton) resid

			}

	}

	global xlab
	forval x=1/13 {
		loc t=`x'+25		
		global xlab " ${xlab}  `x' " `" "`t'" "' " "
	} 
	global lab: dis "${xlab}"


	global xline 30
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
	coefplot(`var'_2, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_3, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_4, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3))),
		keep(*.tsup_h_q_r#1.var_q) 
		xcap label(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5)
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(red) lpattern(shortdash))
		scheme(plotplainblind) name(G`quant'_`depvar'_`var' , replace);
	*graph export "$Dtabs/trends_`var'_quantiles.png", replace ;
	#delimit cr
}
}
binscatter p_gpp_w tsup_h_q_r if tsup_h_q_r, by(quant_gini_can) line(connect) name(pred,replace) discrete



ta month the_shock_s if cw_tag
byso canton month: egen tot_shocks=total(the_shock_s)
ta tot_shocks

binscatter cgpp_w_shock t if any_the_shock_y_w, by(quant_gini_can) line(connect) absorb(canton)
binscatter cgpp_w_noshock t if any_the_shock_y_w, by(quant_gini_can) line(connect) absorb(canton)
binscatter cgpp_w t if month>5, by(quant_gini_can) line(connect) 


binscatter gini_can year, line(connect)

* Correlation of shocks by month
preserve
keep if month>5 & month<10 
*reghdfe the_shock_m, absorb(canton##month year) resid
*predict r_the_shock_m, r
*replace the_shock_m=r_the_shock_m
cap drop r_the_shock_m 
cap drop _reghdfe_resid
keep canton the_shock_m month year
duplicates drop 
so canton month year 
byso canton month: gen ym=_n
so canton year month 
reshape wide the_shock_m year , i(canton month) j(ym)
loc d 2015
forval m=1/7 {
rename the_shock_m`m' y`d'
	loc ++d
}
rename month mth
rename year* annee*
estpost corr y*, m
eststo correlation 
esttab correlation, unstack compress b(2) nostar not nonum f nonum nomti noli noobs
esttab correlation using "${Dtabs}/month_shock_corr.tex", replace unstack compress b(2) nostar f nonum nomti noli style(tex) noobs

forval m=6/9{
cap n estpost corr y* if mth==`m', m
eststo correlation 
esttab correlation, unstack compress b(2) nostar not nonum f nonum nomti noli noobs
esttab correlation using "${Dtabs}/month`m'_shock_corr.tex", replace unstack compress b(2) nostar f nonum nomti noli style(tex) noobs
}
restore


preserve
keep if month>5 & month<10 
reghdfe the_shock_m, absorb(canton##month year) resid
predict r_the_shock_m, r
replace the_shock_m=r_the_shock_m
cap drop r_the_shock_m 
cap drop _reghdfe_resid
keep canton the_shock_m month year
duplicates drop 
so canton month year 
byso canton month: gen ym=_n
so canton year month 
reshape wide the_shock_m year , i(canton month) j(ym)
loc d 2015
forval m=1/7 {
rename the_shock_m`m' y`d'
	loc ++d
}
rename month mth
rename year* annee*
estpost corr y*, m
eststo correlation 
esttab correlation, unstack compress b(2) nostar not nonum f nonum nomti noli noobs
esttab correlation using "${Dtabs}/month_shock_corr_resid.tex", replace unstack compress b(2) nostar f nonum nomti noli style(tex) noobs

forval m=6/9{
cap n estpost corr y* if mth==`m', m
eststo correlation 
esttab correlation, unstack compress b(2) nostar not nonum f nonum nomti noli noobs
esttab correlation using "${Dtabs}/month`m'_shock_corr_resid.tex", replace unstack compress b(2) nostar f nonum nomti noli style(tex) noobs
}
restore

preserve
sum temp_w
*gen q_temp=round(temp_w)
keep if month>5 & month<9
*se trace on
gen rtemp_w=.	
xtile q_temp=temp_w, n(100)
sum q_temp
loc min=r(min)
loc max=r(max)
	forval m=`min'/`max' {
		qui sum temp_w if q_temp==`m'
replace rtemp_w=round(r(mean),.5) if q_temp==`m'
}
ta rtemp_w

*collapse (mean) gpp temp_w (count) wgt=canton, by(month year quant_gini_can)
*replace quant_gini_can=quant_dherf
rename lgpp_w gpp
ta the_shock_${inter}
keep if rtemp_w>=28
ta the_shock_${inter} season 
reghdfe gpp , absorb(canton##year) resid
predict r_gpp, r
reghdfe gpp c.rtemp_w##b1.quant_gini_can, absorb(canton##year) vce(cl canton)
testparm quant_gini_can#c.rtemp_w, equal
collapse (mean) r_gpp (count) wgt=canton, by(quant_gini_can rtemp_w)
rename r_gpp gpp 
rename rtemp_w temp_w 
*replace gpp=ln(gpp)
*twoway (connected gpp temp_w, sort), by(quant_gini_can)
twoway (connected gpp temp_w if  quant_gini_can==1, color(gs12)  sort) ///
(connected gpp temp_w if  quant_gini_can==2, color(gs10) sort) ///
(connected gpp temp_w if  quant_gini_can==3, color(gs8) sort) ///
(connected gpp temp_w if  quant_gini_can==4, color(gs6) sort) ///
(connected gpp temp_w if  quant_gini_can==5, color(gs4) sort legend(order(1 2 3 4 5)  cap label(1 "q1") cap label(2 "q2") cap label(3 "q3") cap label(4 "q4") cap label(5 "q5") pos(6) row(2))) ///
(lfit gpp temp_w if  quant_gini_can==1, color(gs12) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==2, color(gs10) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==3, color(gs8) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==4, color(gs6) lwidth(thick)  sort) ///
(lfit gpp temp_w if  quant_gini_can==5, color(gs4) lwidth(thick)  sort) 



* Variance of crops
sum crop*,d
sum temp_w if month>3 & month<11,d
cap drop max_crop
egen max_crop=rowmax(crop1 crop2 crop3 crop4 crop5 crop6 crop7 crop8 crop9 crop14 crop15 crop20 crop21 crop22 crop23 crop24 crop25)
cap drop top_crop
gen top_crop=.
forval c=1/28 {
cap n replace top_crop=`c' if max_crop==crop`c'
}
ta top_crop
byso canton: egen mean_crop1=mean(crop1)
sum mean_crop1,d

ta the_shock_x the_shock_w
cap drop shock_count
byso canton month: egen shock_count=max(the_shock_x)
ta month shock_count


preserve
keep if month>5 & month<10 
*reghdfe the_shock_m, absorb(canton##month t) resid
*predict r_the_shock_m, r
*replace the_shock_m=r_the_shock_m
cap drop r_the_shock_m 
cap drop _reghdfe_resid
keep canton the_shock_m month year
duplicates drop 
so canton month year 
byso canton month: gen ym=_n
so canton year month 
reshape wide the_shock_m year , i(canton month) j(ym)
corr the_shock_m* 
pause


foreach t in year month p {
	foreach var in the_shock_w the_shock_m gpp_w gpp_m {
preserve
*keep if month>5 & month<9 
reghdfe `var', absorb(canton##`t') resid(res)
loc r=e(r2)
loc n=e(N)
predict FEs, d
estpost corr `var' FEs, m
eststo correlation 
estadd sca r2=`r', replace
estadd sca N=`n', replace
loc stats "stats(r2 N , cap label("R-squared" "N") fmt(%9.3f 0))" 
esttab correlation, unstack compress b(2) nostar not nonum f nonum nomti noli noobs `stats'
esttab correlation using "${Dtabs}/`t'_`var'_corr_r2.tex", replace unstack compress b(2) nostar f nonum nomti noli style(tex) noobs `stats'
restore
}
}


pause


byso canton year month: ereplace the_shock_m=max(the_shock_x)
keep canton the_shock_m month year
duplicates drop 
so canton year month 
byso canton month: gen ym=_n
so canton year month
reshape wide the_shock_m year , i(canton month) j(ym)
pause
cap drop shock_m_sd
byso canton: egen shock_m_sd=sd(the_shock_m4)
corr the_shock_m* 
sum shock_m_sd, d


cap drop can_year_tag
egen can_year_tag=tag(canton month the_shock_m)
count if can_year_tag==1 & the_shock_m==1
codebook canton if can_year_tag==1 & the_shock_m==1
estpost tabstat shock_count if can_year_tag, by(year) ///
statistics(mean sum) columns(statistics) listwise



reghdfe gpp_w if month>5 & month<10 , absorb(canton##year t)

byso canton month year: egen shock_count=total(the_shock_${inter})
ta  p month
byso canton month year:
estpost tabstat the_shock_${inter}, by(month) ///
statistics(mean count) columns(statistics) listwise


preserve
sum temp_w
*gen q_temp=round(temp_w)
keep if month>5 & month<9
*se trace on
gen rtemp_w=.	
xtile q_temp=temp_w, n(100)
sum q_temp
loc min=r(min)
loc max=r(max)
	forval m=`min'/`max' {
		qui sum temp_w if q_temp==`m'
replace rtemp_w=round(r(mean),.5) if q_temp==`m'
}
ta rtemp_w

*collapse (mean) gpp temp_w (count) wgt=canton, by(month year quant_gini_can)
replace quant_gini_can=quant_dherf
rename gpp_w gpp
ta the_shock_${inter}
keep if rtemp_w>=29
ta the_shock_${inter} season 
reghdfe gpp , absorb(canton##year month) resid
predict r_gpp, r
reghdfe gpp c.rtemp_w##b1.quant_gini_can, absorb(canton##year month) vce(cl canton)
testparm quant_gini_can#c.rtemp_w, equal
collapse (mean) r_gpp (count) wgt=canton, by(quant_gini_can rtemp_w)
rename r_gpp gpp 
rename rtemp_w temp_w 
*replace gpp=ln(gpp)
*twoway (connected gpp temp_w, sort), by(quant_gini_can)
twoway (connected gpp temp_w if  quant_gini_can==1, color(gs12)  sort) ///
(connected gpp temp_w if  quant_gini_can==2, color(gs10) sort) ///
(connected gpp temp_w if  quant_gini_can==3, color(gs8) sort) ///
(connected gpp temp_w if  quant_gini_can==4, color(gs6) sort) ///
(connected gpp temp_w if  quant_gini_can==5, color(gs4) sort legend(order(1 2 3 4 5)  cap label(1 "q1") cap label(2 "q2") cap label(3 "q3") cap label(4 "q4") cap label(5 "q5") pos(6) row(2))) ///
(lfit gpp temp_w if  quant_gini_can==1, color(gs12) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==2, color(gs10) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==3, color(gs8) lwidth(thick) sort) ///
(lfit gpp temp_w if  quant_gini_can==4, color(gs6) lwidth(thick)  sort) ///
(lfit gpp temp_w if  quant_gini_can==5, color(gs4) lwidth(thick)  sort) 
reghdfe

twoway (connected gpp temp_w if  quant_gini_can==1, sort) ///
(connected gpp temp_w if  quant_gini_can==2, sort) ///
(connected gpp temp_w if  quant_gini_can==3, sort) ///
(connected gpp temp_w if  quant_gini_can==4, sort) ///
(connected gpp temp_w if  quant_gini_can==5, sort) 

twoway (connected gpp temp_w if month>5 & month<9 & quant_gini_can==1, sort) ///
(connected gpp temp_w if month>5 & month<9 & quant_gini_can==2, sort) ///
(connected gpp temp_w if month>5 & month<9 & quant_gini_can==3, sort) ///
(connected gpp temp_w if month>5 & month<9 & quant_gini_can==4, sort) ///
(connected gpp temp_w if month>5 & month<9 & quant_gini_can==5, sort) 
preserve
xtile q_temp=temp_w, n(100)

collapse (mean) gpp temp_w (count) wgt=canton, by(quant_gini_can q_temp)
*twoway (connected gpp temp_w, sort), by(quant_gini_can)
twoway (lfit gpp q_temp if month==7 & quant_gini_can==1, sort) ///
(lfit gpp q_temp if month>5 & month<9 & quant_gini_can==2, sort) ///
(lfit gpp q_temp if month>5 & month<9 & quant_gini_can==3, sort) ///
(lfit gpp q_temp if month>5 & month<9 & quant_gini_can==4, sort) ///
(lfit gpp q_temp if month>5 & month<9 & quant_gini_can==5, sort) 
, by(quant_gini_can)


collapse (mean) gpp temp_w (count) wgt=canton, by(month year quant_gini_can)
twoway (line gpp temp_w if month==7, sort), by(quant_gini_can)
twoway (line gpp temp_w if month>5 & month<9, sort), by(quant_gini_can)

twoway (scatter gpp temp_w, sort), by(quant_gini_can)

ta p month
preserve
*replace the_shock_${inter}=0 if the_shock_${inter}==1 & month>7
*drop if the_shock_${inter}==1 & month>7
*drop if the_shock_${inter}==1 & month>7
*drop if p>25
*sum gini_can if adm_can=="FRA.1.9.2.10_1"
*sum gini_can if canton==392
*decode state, gen(stateString)
keep gpp_${inter} lgpp_${inter} adm_can canton month year t the_shock_${inter} the_shock_y_${inter} quant_* season temp_${inter} p crop*
*replace the_shock_${inter}=temp_w>30
*keep if temp_w>5
drop if p<15 | p>35


so canton t
cap drop t
so canton year p
byso canton: gen t=_n
ta t

cap drop t
so canton year month
byso canton: gen t=_n

so canton t

rename the_shock_${inter} the_shock
cap drop L* F*
forval l=1/5{
	cap drop L`l'_the_shock
	byso canton year: gen L`l'_the_shock=the_shock[_n-`l']
}

so canton t
forval l=5(-1)1 {
	cap drop F`l'_the_shock
	byso canton year: gen F`l'_the_shock=the_shock[_n+`l']
}



//loop over heterogeneity variables 
loc quant 5
foreach depvar in gpp_$inter  /*lgpp_$inter */ {
	estimates clear

foreach var in gini_can dherf /*herfindahl dcount count_km2 */ {
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
				eststo `var'_`q': reghdfe `depvar' ///
					(F*_the_shock the_shock L*_the_shock)##var_q if ///
					(quant_`var'==1 | quant_`var'==`q'), ///
					absorb(canton#p t) vce(cl canton)
			}

	}

	global xlab
	forval x=1/11 {
		loc t=`x'-6	
		global xlab " ${xlab}  `x' " `" "t`t'" "' " "
	} 
	global lab: dis "${xlab}"


	global xline 5.5
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
	coefplot(`var'_2, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_3, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_4, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3)))
		(`var'_5, m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(gs0))),
		keep(1.F*_the_shock#1.var_q 1.the_shock#1.var_q 1.L*_the_shock#1.var_q) 
		xcap label(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5) omitted
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(red) lpattern(shortdash))
		scheme(plotplainblind) name(G`quant'_`depvar'_`var' , replace);
	*graph export "$Dtabs/trends_`var'_quantiles.png", replace ;
	#delimit cr
}
}



















*global inter w
global inter m
* List variables 

global rename "ccount dcount dherf"

foreach var in $rename {
rename `var'_o `var'
}

gen canton_area=canton_area_${inter}
* Normalize crop count canton-size 
cap drop count_km2
gen count_km2=dcount/(canton_area)

global het_vars "gini_can dcount count_km2 dherf"
global dep_vars "gpp_${inter} "


* Create baseline het variables, y for gini, y-1 for crops

global base_year 2015
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
	sum `var', d

loc the_shock the_shock_${inter}
	* Create interactions for continuous het and also by quantiles
	loc q=4
	cap drop `the_shock'X`var'
	gen `the_shock'X`var'=`the_shock'*`var'
		cap drop med_`var'
	xtile med_`var'= `var', nq(`q') 
	cap drop quant_`var'
	xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=`the_shock'*q_`var'`x'
	}
}
cap drop lgpp_${inter}
gen lgpp_${inter}=ln(gpp_m)

loc the_shock the_shock_${inter}
loc y lgpp_${inter}

ta month the_shock_${inter}

global shock the_shock_${inter}
pause on 
preserve
keep canton gpp_${inter} gini_can the_shock* year t month date p the_shock_${inter}Xgini_can-t_q_dherf4  temp_w  temp_m crop*


	xtset canton t

	//define shock periods 
	cap drop tempvar
	qui gen tempvar=t if ${shock} == 1
	cap drop first_p
	byso canton year: egen first_p = min(tempvar)
	qui replace first_p = 0 if first_p == .
	so canton t 

	* create never treated group
	cap drop dinf
	cap drop tempvar
	byso canton year: egen tempvar = max(${shock})
	qui gen dinf = (tempvar - 1) * -1

	*
	qui keep year canton t ${shock} gpp_${inter} dinf quant_* p month temp_w  temp_m month crop*
	cap drop w
	byso canton year: gen w = sum(${shock})
	qui replace w = 1 if w > 0

global time_unit month
*global time_unit p

sum ${time_unit}
loc min=r(min)
loc max=r(max)
*se trace on
	forval m=`min'/`max' {
		*
		cap drop f`m'
				gen f`m'=${time_unit}==`m'
	}
		forval m=`min'/`max' {


		cap drop d`m'
		cap drop temp`m'
		qui gen temp`m' = w * f`m'
		

		 byso canton year: egen d`m' = max(temp`m')
		*clean 
		qui drop temp`m'
	}
	forval m=`min'/`max'  {
		qui replace d`m' = 0 if d`m' == .
	}

	*test something 
	cap drop test
	qui egen test = rowtotal(d`min' - d`max')
	ta test
	qui drop test
	tab1 d`min'-d`max'
	qui sort canton t


cap drop season
gen season=1 if month>0 & month<=3
replace season=2 if month>3 & month<=6
replace season=3 if month>6 & month<=9
replace season=4 if month>9
ta season

*ta p month
ta month w
cap drop med_gini_can
gen med_gini_can=quant_gini_can>2
cap drop med_dherf
gen med_dherf=quant_dherf>2
pause
sum mean_crop1,d
keep if mean_crop1>r(p50)
cap drop med_`het'
gen med_`het'=quant_`het'>2

sum crop*
byso canton: egen mean_crop1=mean(crop1)
sum mean_crop1,d
estimates clear
foreach het in gini_can dherf {

forval beg=3/5 {
preserve
loc st `beg'
loc ref =`st'+1
loc en =`st'+5


keep if month>=`st' & month<=`en'

loc T = `st'+3
loc pre = `T'-1

	qui egen test = rowtotal(d`T'-d`en')
	ta test
	gen dweek=d`T'
	loc loop 1
	foreach var of var f`st'-f`en'{
gen t`loop'=`var'
loc ++loop
	}

*ta p w
*reghdfe gpp_w c.d22#c.(f16-f26) c.d23#c.(f16-f26) if d21==0 , absorb(canton##season f15-f26  d22-d26) vce(cl canton)
eststo `het'_p`T': reghdfe gpp_m c.dweek#c.(t1-t6)##c.med_`het' if d`pre'==0 & (test==3 | test==0), absorb(canton#season##c.(crop1-crop28) f`st'-f`en'  d`T'-d`en' t) vce(cl canton)

*eststo p`T': reghdfe gpp_w c.d`T'#c.(f`st'-f`en')##c.med_gini_can if d`pre'==0 & (test==5 | test==0), absorb(canton##season f`st'-f`en'  d`T'-d`en' t) vce(cl canton)
restore
}
}









estimates clear
foreach het of var med_gini_can med_dherf {

forval beg=18/22 {
preserve
loc st `beg'
loc ref =`st'+1
loc en =`st'+8
keep if p>=`st' & p<=`en'

loc T = `st'+4
loc pre = `T'-1

	qui egen test = rowtotal(d`T'-d`en')
	ta test
	gen dweek=d`T'
	loc loop 1
	foreach var of var f`st'-f`en'{
gen t`loop'=`var'
loc ++loop
	}
*ta p w
*reghdfe gpp_w c.d22#c.(f16-f26) c.d23#c.(f16-f26) if d21==0 , absorb(canton##season f15-f26  d22-d26) vce(cl canton)
eststo `het'_p`T': reghdfe gpp_w c.dweek#c.(t1-t9)##c.`het' if d`pre'==0 & (test==5 | test==0), absorb(canton##season f`st'-f`en'  d`T'-d`en' t) vce(cl canton)

*eststo p`T': reghdfe gpp_w c.d`T'#c.(f`st'-f`en')##c.med_gini_can if d`pre'==0 & (test==5 | test==0), absorb(canton##season f`st'-f`en'  d`T'-d`en' t) vce(cl canton)
restore
}
}

coefplot med_gini_can*, keep(c.dweek#c.t*#c.med_gini_can) noci vertical xcap label(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(gini,replace) scheme(plotplainblind)


coefplot med_dherf*, keep(c.dweek#c.t*#c.med_dherf) noci vertical xcap label(4.5 "Heat shock") xline(4.5) yline(0) recast(connected)  name(herf,replace) scheme(plotplainblind)

coefplot med_gini_can_p25, keep(c.dweek#c.t*#c.med_gini_can) vertical xcap label(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(gini,replace) scheme(plotplainblind)

coefplot med_dherf_p25, keep(c.dweek#c.t*#c.med_dherf) vertical xcap label(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(herf,replace) scheme(plotplainblind)


reghdfe gpp_w c.d`T'#c.(f`st'-f`en')##b1.quant_gini_can if d`pre'==0 & (test==5 | test==0), absorb(canton##i.season f`st'-f`en'  d`T'-d`en' t) vce(cl canton)

reghdfe gpp_w (c.d22#c.(f16-f26))##b1.quant_gini_can if d21==0 | dinf==1, absorb(canton##season f16-f26  d22-d26) vce(cl canton)

reghdfe gpp_w (c.d22#c.(f16-f26))##b1.quant_gini_can if (d22==1 & d21==0) | dinf==1, absorb(canton##season f16-f26  d22-d26) vce(cl canton)

reghdfe gpp_w c.d22#c.(f16-f26)##b1.quant_gini_can if d21==0 & d23==0 & d24==0 & d25==0 & d26==0, absorb(canton##season f15-f26  d22-d26) vce(cl canton)


reghdfe gpp_w c.w#c.d6#c.(f1-f12) c.d6#c.(f1-f12) w f1-f12  d1-d12, absorb(canton##season year) vce(cl canton)


reghdfe gpp_w c.${shock}#c.(f15-f34) if d22==1 | dinf==1, absorb(canton##season) vce(cl canton)



reg y c.d4#c.f04 c.d4#c.f05 c.d4#c.f06 ///
	c.d5#c.f05 c.d5#c.f06 ///
	c.d6#c.f06 ///
	d4 d5 d6 i.year, vce(cluster id)




