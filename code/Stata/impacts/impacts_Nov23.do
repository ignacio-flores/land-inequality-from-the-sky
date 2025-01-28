global ID "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
*global ID "~/Dropbox/land_ineq_degradation/"
global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky/tabs"
*global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
global rep "$ID"
cd "$ID"

/*
u data/FR/MODIS/satellite_to_canton.dta, clear
keep adm_can name_*
duplicates drop 
duplicates report adm_can
saveold data/FR/admin_canton, replace
*/

*u data/FR/working_panel_canton.dta, clear
*u data/FR/working_panel_farms.dta, clear
clear all
set maxvar 20000
u data/FR/working_panel_8days_foods_no28.dta, clear

* Set panel 
xtset canton t
xtsum gini_can
sum area_agri,d

* Normalize crop count canton-size 
cap drop count_km2
gen count_km2=dcount/(canton_area)
sum canton_area,d
cap drop max_crop
egen max_crop=rowmax(crop*)
sum max_crop,d

* Define seasons 
cap drop season
gen season=1 if month>0 & month<=3
replace season=2 if month>3 & month<=6
replace season=3 if month>6 & month<=9
replace season=4 if month>9
ta season

* List variables 
global het_vars "gini_can dcount count_km2 max_crop"
global dep_vars "gpp cum_gpp"

* Create baseline het variables, y for gini, y-1 for crops
global base_year 2015
foreach var in $het_vars {
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

	* Create interactions for continuous het and also by quantiles
	loc q=3
	cap drop the_shockX`var'
	gen the_shockX`var'=the_shock*`var'
	cap drop quant_`var'
	xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=the_shock*q_`var'`x'
	}
}


preserve
	* keeping years where we have shocks
	*keep if year>2017 & year <2021
	foreach var in gini_can {
		estimates clear
		cap n	eststo `var'C: ///
			reghdfe gpp the_shockX`var' the_shock `var', ///
			absorb(canton##month t) vce(cl canton)
		cap n	eststo `var'Q: ///
			reghdfe gpp t_q_`var'* q_`var'*, ///
			absorb(canton##month t) vce(cl canton)
		cap n	testparm t_q_`var'*, equal
		cap n	estadd sca pval_id=round(r(p), .01)


		loc stats "stats(pval_id N, label("\\ p-val equal" "N") fmt(%9.4f %9.0f))"
/*
		#delimit
		esttab `var'C `var'Q using "$Dtabs/het_impact_`var'.tex",  
			f nonum nomti noli collabels(none) keep(the_shockX`var' t_q_`var'*) 
			varlabels(the_shockX`var' "Shock x Gini" t_q_`var'1 "1st quintile" 
			t_q_`var'2 "2nd quintile" t_q_`var'3 "3rd quintile" 
			t_q_`var'4 "4th quintile" t_q_`var'5 "5th quintile")
			`stats' style(tex) noobs cells(b(fmt(3) star) 
			se(par fmt(3))) star(* .1 ** .05 *** .01)  replace;
		#delimit cr
*/
		#delimit
		coefplot `var'Q , keep(t_q_`var'*) 
			xlabel(1 "1st quintile" 2 "2nd quintile" 3 
			"3rd quintile" 4 "4th quintile" 5 "5th quintile")
			yline(0, lcolor(gs0)) vertical mcolor(gs5) 
			ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) 
			ytitle("") xtitle("") scheme(plotplainblind) 
			name(`var'Q_`y',replace);
		*graph export "$Dtabs/het_impact_`var'.png", replace ;
		#delimit cr
	}
restore


* Documenting trends with X lags and leads: 
* isolating from approximately mid march to mid sept
ta p month

so canton t
cap drop L* F*
forval l=1/8{
	cap drop L`l'_the_shock
	byso canton year: gen L`l'_the_shock=the_shock[_n-`l']
}

so canton t
forval l=8(-1)1 {
	cap drop F`l'_the_shock
	byso canton year: gen F`l'_the_shock=the_shock[_n+`l']
}



ta month the_shock
cap drop cohort
gen cohort=month if the_shock==1
ta cohort
replace cohort=0 if cohort==.


estimates clear

//loop over heterogeneity variables 
foreach var in $het_vars {
	foreach qtle in 2 5 {
		preserve
			keep if year>2017 & year <2021

			* Redfining quantiles or median 
			cap drop quant_`var'
			xtile quant_`var'= `var', nq(`qtle') 
			cap drop q_`var'*
			tab quant_`var',gen(q_`var')
			forval q=2/`qtle' {
				cap drop var_q
				gen var_q=q_`var'`q'	
				eststo G`qtle'_`q': reghdfe gpp ///
					(F*_the_shock the_shock L*_the_shock)##var_q if ///
					(quant_`var'==1 | quant_`var'==`q'), ///
					absorb(canton##month t) vce(cl canton)
			}
		restore
	}

	global xlab
	forval x=1/17 {
		loc t=`x'-9	
		global xlab " ${xlab}  `x' " `" "t`t'" "' " "
	} 
	global lab: dis "${xlab}"


	global xline 8.5

	#delimit
	coefplot(G2_2, m(o) mc(black) connect(line) 
		lcolor(gs15) lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs9))),
		keep(1.F*_the_shock#1.var_q 1.the_shock#1.var_q 1.L*_the_shock#1.var_q) 
		xlabel(${lab})
		yline(0, lcolor(gs0)) vertical mcolor(gs5) omitted
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(black) lpattern(shortdash)) 
		scheme(plotplainblind) name(G2_`var', replace);
	graph export "$Dtabs/trends_`var'_median.png", replace ;
	#delimit cr

	#delimit
	coefplot(G5_2, m(o) mc(black) connect(line) lcolor(gs15) 
		lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs9)))
		(G5_3, m(o) mc(black) connect(line) lcolor(gs12) 
		lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs6)))
		(G5_4, m(o) mc(black) connect(line) lcolor(gs6) 
		lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs3)))
		(G5_5, m(o) mc(black) connect(line) lcolor(gs0) 
		lpattern(solid) ciopts(lpattern(shortdash) lcolor(gs0))),
		keep(1.F*_the_shock#1.var_q 1.the_shock#1.var_q 1.L*_the_shock#1.var_q) 
		xlabel(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5) omitted
		ciopts(recast(rline) lpattern(dash) lcolor(gs0)) ytitle("") 
		xtitle("") xline(${xline}, lcolor(black) lpattern(shortdash))
		scheme(plotplainblind) name(G4_`var', replace);
	graph export "$Dtabs/trends_`var'_quantiles.png", replace ;
	#delimit cr
}


* Imposing biodiversity
preserve
	* keeping years where we have shocks
	keep if year>2017 & year <2021
	foreach var in gini_can {
		
		estimates clear
		cap n	eststo `var'C: 
			reghdfe gpp the_shockX`var' the_shock `var' if ///
			quant_count_km2==5, absorb(canton##month t) vce(cl canton)
			
		cap n	eststo `var'Q: 
			reghdfe gpp t_q_`var'* q_`var'* if ///
			quant_count_km2==5, absorb(canton##month t) vce(cl canton)
		cap n	testparm t_q_`var'*, equal
		cap n	estadd sca pval_id=round(r(p), .01)

		loc stats ///
			"stats(pval_id N, label("\\ p-val equal" "N") fmt(%9.4f %9.0f))"

		#delimit
			esttab `var'C `var'Q using 
				"$Dtabs/het_impact_`var'_impose_biod.tex",  
				f nonum nomti noli collabels(none) 
				keep(the_shockX`var' t_q_`var'*) 
				varlabels(the_shockX`var' "Shock x Gini" 
				t_q_`var'1 "1st quintile" t_q_`var'2 "2nd quintile" 
				t_q_`var'3 "3rd quintile" t_q_`var'4 "4th quintile" 
				t_q_`var'5 "5th quintile")
				`stats' style(tex) noobs cells(b(fmt(3) star) 
				se(par fmt(3))) star(* .1 ** .05 *** .01) replace;
		#delimit cr

		#delimit
			coefplot `var'Q , keep(t_q_`var'*) 
				xlabel(1 "1st quintile" 2 "2nd quintile" 
				3 "3rd quintile" 4 "4th quintile" 5 "5th quintile")
				yline(0, lcolor(gs0)) vertical mcolor(gs5) 
				ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) 
				ytitle("") xtitle("") scheme(plotplainblind) 
				name(`var'Q_`y',replace);
			graph export "$Dtabs/het_impact_`var'_impose_biod.png", replace ;
		#delimit cr
	}
restore

* Diversity and farm sizes
foreach var in gini_can count_km2 {
	loc q=5
	cap drop quant_`var'
	xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=the_shock*q_`var'`x'
	}
}


	foreach var in count_km2 {
		estimates clear
			reghdfe gpp t_q_gini_can* L1.t_q_`var'*, absorb(canton##month t) vce(cl canton)
		cap n	testparm t_q_gini_can*, equal
		cap n	estadd sca pval_id=round(r(p), .01)
}

		#delimit
		coefplot,  keep(1.t_q_gini_can?) 
			xlabel(1 "1st quintile" 2 "2nd quintile" 3 
			"3rd quintile" 4 "4th quintile" 5 "5th quintile")
			yline(0, lcolor(gs0)) vertical mcolor(gs5) 
			ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) 
			ytitle("") xtitle("") scheme(plotplainblind) 
			name(gini_can_Q_`y',replace);
		*graph export "$Dtabs/het_impact_`var'.png", replace ;
		#delimit cr


	foreach var in count_km2 {
		estimates clear
			eststo base: reghdfe gpp (t_q_gini_can*)##(q_`var'2 q_`var'3 q_`var'4 q_`var'5) if year>2015, absorb(canton##month t) vce(cl canton)
			*eststo base: reghdfe agpp (t_q_gini_can*)##(q_`var'_b2 q_`var'_b3) if year>2015, absorb(canton##month t) vce(cl canton)

		cap n	testparm 1.t_q_gini_can*, equal
		cap n	estadd sca pval_id=round(r(p), .01)
}

forval g=1/5 {
est restore base
lincomest 1.t_q_gini_can`g'
eststo gini`g'_count1
forval d=2/5 {
est restore base
lincomest 1.t_q_gini_can`g' + 1.t_q_gini_can`g'#1.q_count_km2`d'
eststo gini`g'_count`d'
}
}

coefplot (gini1_count1, rename((1) = 1) ///
	\ gini2_count1, rename((1) = 2) ///
	\ gini3_count1, rename((1) = 3))  ///
(gini1_count2, rename((1) = 1) ///
	\ gini2_count2, rename((1) = 2) ///
	\ gini3_count2, rename((1) = 3))  ///
	(gini1_count3, rename((1) = 1) ///
	\ gini2_count3, rename((1) = 2) ///
	\ gini3_count3, rename((1) = 3)), vertical recast(line) scheme(plotplainblind) offset(0)

	*\ gini4_count5, rename((1) = 4)  ///
	*\ gini5_count5, rename((1) = 5)) , vertical recast(line) scheme(plotplainblind)


coefplot (gini1_count1, rename((1) = 1) ///
	\ gini2_count1, rename((1) = 2) ///
	\ gini3_count1, rename((1) = 3)  ///
	\ gini4_count1, rename((1) = 4)  ///
	\ gini5_count1, rename((1) = 5)) /// 
(gini1_count2, rename((1) = 1) ///
	\ gini2_count2, rename((1) = 2) ///
	\ gini3_count2, rename((1) = 3)  ///
	\ gini4_count2, rename((1) = 4)  ///
	\ gini5_count2, rename((1) = 5)) ///
	(gini1_count3, rename((1) = 1) ///
	\ gini2_count3, rename((1) = 2) ///
	\ gini3_count3, rename((1) = 3)  ///
	\ gini4_count3, rename((1) = 4)  ///
	\ gini5_count3, rename((1) = 5)) /// 
		(gini1_count4, rename((1) = 1) ///
	\ gini2_count4, rename((1) = 2) ///
	\ gini3_count4, rename((1) = 3)  ///
	\ gini4_count4, rename((1) = 4)  ///
	\ gini5_count4, rename((1) = 5)) ///   
	(gini1_count5, rename((1) = 1) ///
	\ gini2_count5, rename((1) = 2) ///
	\ gini3_count5, rename((1) = 3)  ///
	\ gini4_count5, rename((1) = 4)  ///
	\ gini5_count5, rename((1) = 5)) , vertical recast(line) scheme(plotplainblind) 

         gini3_count1  \ ///               
	gini4_count1  \ ///
         gini5_count1)

          int2, rename((1) = _Icat_var_2) \                     ///
          int3, rename((1) = _Icat_var_3) label(bin_var = 1))   ///
    , eform xline(1) ciopts(recast(rcap))                       ///
    coeflabels(_Icat_var_1 = "Q1 xx-xx"                         ///
               _Icat_var_2 = "Q2 xx-xx"                         ///
               _Icat_var_3 = "Q3 xx-xx")

coefplot (base, keep(_Icat_var_?) label(bin_var = 0))           ///
         (int1, rename((1) = _Icat_var_1) \                     ///
          int2, rename((1) = _Icat_var_2) \                     ///
          int3, rename((1) = _Icat_var_3) label(bin_var = 1))   ///
    , eform xline(1) ciopts(recast(rcap))                       ///
    coeflabels(_Icat_var_1 = "Q1 xx-xx"                         ///
               _Icat_var_2 = "Q2 xx-xx"                         ///
               _Icat_var_3 = "Q3 xx-xx")
