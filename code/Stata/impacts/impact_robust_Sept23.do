global ID "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
*global ID "~/Dropbox/land_ineq_degradation/"
global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky/tabs"
*global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
global rep "$ID"
cd "$ID"

conditional quasi random 
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
u data/FR/working_panel_8days_foods.dta, clear


xtset canton t

xtsum gini_can

	* Normalize crop count canton-size 
	cap drop count_km2
	gen count_km2=dcount/canton_area
	egen max_crop=rowmax(crop*)

ta year

sum gini_can,d


sum nfarms,d
drop if nfarms<30
sum crop28,d
drop if crop28>.10
ta vlarge_farm
loc q=5
foreach var in gini_can dcount dherf {
	/*
	cap drop var1
	gen var1= `var' if year==2015
	drop `var'
	byso canton: egen `var'=max(var1)
	

	sum `var', d
	if "`var'"=="gini_can" {
	drop if `var'<r(p1) | `var'>r(p99) 
	*/



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
*replace the_shock=p==22 & year==2019
ta quant_gini_can the_shock
			qui sort canton t	
			global for 8
			global lag 15

eststo FL: reghdfe gpp (f(${for}/0).the_shock ///
				l(1/${lag}).the_shock)##i.quant_gini_can, ///
				absorb(canton##i.p i.t) vce(cl canton)		

reghdfe agpp (f(${for}/0).the_shock ///
				l(1/${lag}).the_shock) if quant_gini_can==5, ///
				absorb(canton##month i.t) vce(cl canton)		


			loc end= ${lag}+ ${for}+1
			mylabels -${for}(1)${lag}, myscale(@+${for}+1) local(labels)
			

*mat list e(b)
#delimit
coefplot ,   
keep(1F*the_shock#*.quant_gini_can 1.the_shock#*.quant_gini_can 1L*the_shock#*.quant_gini_can) omitted base
 /*legend( order(2 )  label(2 "Anonymous/Total") )*/ 
ytitle("") plotregion(style(none)) graphregion(margin(2 6 2 2) fcolor(white)) xlabel(`labels')
vertical lwidth(*2) yline(0, lcolor(black) lpattern(dot)) scheme(plotplainblind) ;
#delim cr

estimates clear
foreach var in gini_can dcount dherf {
	eststo `var'C: reghdfe agpp the_shockX`var' the_shock gini_can, absorb(canton t) vce(cl canton)
eststo `var'Q: reghdfe agpp t_q_`var'* q_`var'*, absorb(canton t) vce(cl canton)
testparm t_q_`var'*, equal
estadd sca pval_id=round(r(p), .01)


loc stats "stats(pval_id N, label("\\ p-val equal" "N") fmt(%9.4f %9.0f))"

#delimit
esttab `var'C `var'Q using "$Dtabs/het_impact_`var'.tex",  f nonum nomti noli 
collabels(none) keep(the_shockX`var' t_q_`var'*) varlabels(the_shockX`var' "Shock x Gini" t_q_`var'1 "1st quintile" t_q_`var'2 "2nd quintile" t_q_`var'3 "3rd quintile" t_q_`var'4 "4th quintile" t_q_`var'5 "5th quintile")
 `stats' style(tex) noobs cells(b(fmt(3) star) 
se(par fmt(3))) star(* .1 ** .05 *** .01)  replace;
#delimit cr

#delimit
coefplot `var'Q , keep(t_q_`var'*) xlabel(1 "1st quintile" 2 "2nd quintile" 3 "3rd quintile" 4 "4th quintile" 5 "5th quintile")
 yline(0, lcolor(gs0)) vertical mcolor(gs5) 
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("") xtitle("")
scheme(plotplainblind) name(het_impact_`var', replace);
graph export "$Dtabs/het_impact_`var'.png", replace ;
#delimit cr
}


sum temp,d

cap drop Ltemper
so canton t
byso canton year: gen Ltemper=temper[_n-1]
br canton t temper Ltemper


* Cumulative graphs on residuals
ta per the_shock
ta p the_shock
ta p hot30
ta month hot30
ta p month
ta p per
ta the_shock_y



preserve
cap drop shocked_y
so canton t
replace the_shock=p==22 & year==2019
byso canton year: egen shocked_y=max(the_shock)
ta shocked_y
*drop if month<3
*so canton t
*byso canton year: replace cum_gpp=sum(gpp)
*gen temp1=the_shock==1 & month==6
*drop the_shock_y
*byso canton year: egen the_shock_y=max(temp1)
*replace the_shock=0 if month==5
*drop if month==5

*keep if shocked_y>=1
cap drop r_gpp

*xtreg gpp cL.temp##cL.temp i.t, fe 

*reghdfe gpp c.Ltemper##c.Ltemper, absorb(canton t, save) resid
*keep if month>3 & month<9
reghdfe cum_gpp, absorb(canton t, save) resid
predict r_gpp, r

*binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) /*ylabel(0(.1).25)*/
*binscatter r_gpp month, by(quant_gini_can) line(connect) name(bin, replace) /*ylabel(-.05(.01).02)*/
binscatter r_gpp p if shocked_y==0 , by(quant_gini_can) line(connect) discrete name(binC, replace) /*ylabel(-.05(.01).02)*/
binscatter r_gpp p if shocked_y==1 , by(quant_gini_can) line(connect) discrete name(binT, replace) /*ylabel(-.05(.01).02)*/

binscatter the_shock p,line(connect) discrete name(binT, replace) /*ylabel(-.05(.01).02)*/
binscatter the_shock p, by(quant_gini_can) line(connect) discrete name(binT, replace) /*ylabel(-.05(.01).02)*/
binscatter the_shock p, by(quant_gini_can) line(connect) discrete name(binT, replace) /*ylabel(-.05(.01).02)*/

binscatter hot30 p if year!=2019, by(quant_gini_can) line(connect) discrete name(binT, replace) /*ylabel(-.05(.01).02)*/

byso p: sum temp if p>20 & p<30, d
byso p year: count if temp>30

preserve 
cap drop shocked_y
*drop if month<3
*so canton t
*byso canton year: replace cum_gpp=sum(gpp)
*gen temp1=the_shock==1 & month==6
*drop the_shock_y
*byso canton year: egen the_shock_y=max(temp1)
*replace the_shock=0 if month==5
*drop if month==5
byso canton: egen shocked_y=max(the_shock_y)
ta shocked_y
*keep if shocked_y>=1
cap drop r_gpp
reghdfe gpp, absorb(i.canton##i.month, save) resid
*reghdfe temp quant_gini_can, absorb(t, save) resid
predict r_gpp, r
keep if month>5 & month<9
*binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) /*ylabel(0(.1).25)*/
*binscatter r_gpp t, by(quant_gini_can) line(connect) name(bin, replace) discrete /*ylabel(-.05(.01).02)*/
binscatter r_gpp t if the_shock_y==0 , by(quant_gini_can) line(connect) name(binC, replace) discrete /*ylabel(-.05(.01).02)*/
binscatter r_gpp t if the_shock_y==1 , by(quant_gini_can) line(connect) name(binT, replace) discrete /*ylabel(-.05(.01).02)*/



byso quant_gini_can: sum gini_can, d
preserve
cap drop shocked_y
cap drop temp
*gen temp=the_shock==1 & month==6
*drop the_shock_y
*byso canton year: egen the_shock_y=max(temp)
*replace the_shock=0 if month==5
*drop if month==5
byso canton: egen shocked_y=max(the_shock_y)

ta shocked_y
*keep if shocked_y>=1
cap drop r_gpp
reghdfe gpp, absorb(canton t, save) resid
predict r_gpp, r
*binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) /*ylabel(0(.1).25)*/
binscatter r_gpp month if the_shock_y==0 , by(quant_count_km2) line(connect) name(binC, replace) /*ylabel(-.05(.01).02)*/
binscatter r_gpp month if the_shock_y==1 , by(quant_count_km2) line(connect) name(binT, replace) /*ylabel(-.05(.01).02)*/


cap drop r_agpp
reghdfe agpp, absorb(canton t, save) resid
predict r_agpp, r
binscatter r_agpp month if the_shock_y==0 , by(quant_gini_can) line(connect) name(abinC, replace) /*ylabel(-.05(.01).02)*/
binscatter r_agpp month if the_shock_y==1 , by(quant_gini_can) line(connect) name(abinT, replace) /*ylabel(-.05(.01).02)*/


binscatter agpp month if the_shock_y==0 , by(quant_gini_can) controls(i.t) absorb(canton) line(connect) name(binC, replace)

ta the_shock month
reghdfe cum_gpp i.the_shock_y##i.quant_gini_can, absorb(canton t, save) resid
restore



preserve
cap drop shocked_y
cap drop temp
gen temp=the_shock==1 & month==6
drop the_shock_y
byso canton year: egen the_shock_y=max(temp)
byso canton: egen shocked_y=max(the_shock_y)
replace the_shock=0 if month==5
ta shocked_y
keep if shocked_y>=1
forval q=1/5 {
	sum gini_can if quant_gini_can==`q'
cap drop r_cum_gpp`q'
reghdfe gpp  if quant_gini_can==`q', absorb(canton t, save) resid
predict r_cum_gpp`q', r
*binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) /*ylabel(0(.1).25)*/
binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) ylabel(-.05(.01).02)
}

ta the_shock month
reghdfe cum_gpp i.the_shock_y##i.quant_gini_can, absorb(canton t, save) resid
restore

preserve
cap drop shocked_y
byso canton: egen shocked_y=max(hot30_y)
ta shocked_y
keep if shocked_y==1
forval q=1/5 {
	sum gini_can if quant_gini_can==`q'
cap drop r_cum_gpp`q'
reghdfe gpp if quant_gini_can==`q', absorb(canton t, save) resid
predict r_cum_gpp`q', xbd
binscatter r_cum_gpp`q' month, by(the_shock) line(connect) name(bin`q', replace) ylabel(0(.25)1.5)
*binscatter r_cum_gpp`q' month, by(the_shock_y) line(connect) name(bin`q', replace) ylabel(-.05(.01).05)
}

reghdfe cum_gpp i.hot30_y##i.quant_gini_can, absorb(canton t, save) resid
restore




binscatter r_cum_gpp5 month, by(the_shock_y) line(connect)

********************************
** ETWFE *
********************************



foreach type in 8days_foods {
	//open main panel data 
	qui use "data/FR/working_panel_`type'.dta", clear
}
	//combine with canton data 
	qui merge m:1 adm_can using "data/FR/admin_canton", nogen
	foreach var of var name_* {
		encode `var', g(n`var')
	}




	//set panel 
	xtset canton t
	*codebook canton
	gen tmonth=month/100
	ta month tmonth
	so canton month
	gen canton_month=canton+tmonth
*odebook canton_month
br if canton_month==.
drop if canton_month==.
	*xtset canton_month t





	//define shock periods 
	rename temp temper

	gen hot25=temper>25
//define shock variable 
*global shock hot25
global shock the_shock

	cap drop temp
	qui gen temp=t if ${shock} == 1
	cap drop first_shock
	byso canton: egen first_shock = min(temp)
	qui replace first_shock = 0 if first_shock == .
	br canton t month ${shock} first_shock
	so canton t 

	* create never treated group
	cap drop dinf
	cap drop temp
	byso canton year: egen temp = max(${shock})
	qui gen dinf = (temp - 1) * -1
	br canton t month ${shock} first_shock dinf
	*
	byso canton year: gen week=_n
	qui tab week, gen(f)
	cap drop w
	byso canton year: gen w = sum(${shock})
	qui replace w = 1 if w > 0

	*
qui sum week 
loc maxt=r(max)
	forval m=1/`maxt' {
		*
		cap drop d`m'
		cap drop temp`m'
		qui gen temp`m' = w * f`m'
		*
		if `m'==1{
		 byso canton year: egen d`m' = max(temp`m')
		}
		else {
			loc mt = `m' - 1
			byso canton year: egen d`m' = max(temp`m') if d`mt' == 0
		}
		*clean 
		qui drop temp`m'
	}
	forval m=1/`maxt' {
		qui replace d`m' = 0 if d`m' == .
	}

	*test something 
	cap drop test
	tab1 d1 - d46
	qui egen test = rowtotal(d1 - d46)
	ta test

	qui sort canton t

br canton year month t d5-d9 the_shock w the_shock_y
byso canton: egen shocked_y=max(the_shock_y)


sum temper,d

reghdfe gpp (c.d21 c.d22 c.d23)#c.(f1-f46), absorb(year month f1-f45 d21-d28)  vce(cl canton)

reghdfe gpp c.(d21-d28)#c.(f1-f46), absorb(t f1-f46 d21-d28)  vce(cl canton)

xtreg agpp c.(d21-d27)#c.(f13-f27) f12-f27 d21-d27 i.t if week>=12 & week<=27 & !dinf, fe vce(cl canton)


xtreg agpp c.(d22)#c.(f13-f27) f12-f27 i.t if week>=12 & week<=27, fe vce(cl canton)
xtreg agpp c.(d22)#c.(f13-f27) f12-f27 i.t if week>=12 & week<=27, fe vce(cl canton)


reghdfe gpp c.d21#c.(f1-f45) c.d22#c.(f1-f11) c.d23#c.(f1-f12) d6-d8 f1-f11, absorb(canton t)  vce(cl canton)

reg gpp c.d6#c.(f1-f11) c.d7#c.(f1-f11) c.d8#c.(f1-f12) d6-d8 f1-f11 i.t, vce(cl canton)
reghdfe gpp c.d6#c.(f1-f11) c.d7#c.(f1-f11) c.d8#c.(f1-f12) d6-d8 f1-f11, absorb(canton t)  vce(cl canton)



reg gpp c.d6#c.(f2-f12) c.d7#c.(f2-f12) c.d8#c.(f2-f12) d5-d8 if !the_shock_y, vce(cl canton)

xtreg agpp c.d6#c.(f2-f12)#c.the_shock_y c.d7#c.(f2-f12) c.d8#c.(f2-f12) d6-d8 i.year, fe vce(cl canton)

xtreg agpp (c.d6#c.(f2-f12) c.d7#c.(f2-f12) c.d8#c.(f2-f12) d6-d8 i.t)#i.quant_gini_can, fe vce(cl canton)

reghdfe agpp (c.d6#c.(f5-f12) c.d7#c.(f5-f12) c.d8#c.(f5-f12) f1-f12  d1-d12)#i.quant_gini_can, absorb(year quant_gini_can) vce(cl canton)
reghdfe agpp (c.d6#c.(f5-f12) c.d7#c.(f5-f12) c.d8#c.(f5-f12) f1-f12  d1-d12) if quant_gini_can==1, absorb(year) vce(cl canton)
reghdfe agpp (c.d6#c.(f1-f12) c.d7#c.(f1-f12) c.d8#c.(f1-f12) f1-f12  d1-d12), absorb(canton year) vce(cl canton)
reghdfe agpp (c.d6#c.(f1-f12) c.d7#c.(f1-f12) c.d8#c.(f1-f12) f1-f12  d1-d12), absorb(canton year) vce(cl canton)
reghdfe agpp c.w#c.d6#c.(f1-f12) c.d6#c.(f1-f12) w f1-f12  d1-d12, absorb(canton year) vce(cl canton)


reg agpp c.d6#c.(f2-f12) c.d7#c.(f2-f12) c.d8#c.(f2-f12) d6-d8 i.t, vce(cl canton)

reg agpp c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 , vce(cl canton)


xtreg agpp c.w#c.d6#c.(f2-f12) c.w#c.d7#c.(f2-f12)  c.w#c.d8#c.(f2-f12)  ///
c.d6#c.(f2-f12) c.d7#c.(f2-f12) c.d8#c.(f2-f12) d6-d8 i.month i.year, fe vce(cl canton)


* Using all post months
estimates clear
forval q=1/5 {
eststo all`q': xtreg agpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 i.t if quant_gini_can==`q', fe vce(cl canton)
est restore all`q'
margins, expression((_b[c.d5#c.f5] + _b[c.d5#c.f6] + _b[c.d5#c.f7] + _b[c.d5#c.f8] + _b[c.d5#c.f9] + _b[c.d5#c.f10] + _b[c.d5#c.f11] + _b[c.d5#c.f12])/8) post
eststo c1`q'
est restore all`q'
margins, expression((_b[c.d6#c.f6] + _b[c.d6#c.f7] + _b[c.d6#c.f8] + _b[c.d6#c.f9] + _b[c.d6#c.f10] + _b[c.d6#c.f11] + _b[c.d6#c.f12])/7) post
eststo c2`q'
est restore all`q'
margins, expression(( _b[c.d7#c.f7] + _b[c.d7#c.f8] + _b[c.d7#c.f9] + _b[c.d7#c.f10] + _b[c.d7#c.f11] + _b[c.d7#c.f12])/6) post
eststo c3`q'
est restore all`q'
margins, expression((_b[c.d8#c.f8] + _b[c.d8#c.f9] + _b[c.d8#c.f10] + _b[c.d8#c.f11] + _b[c.d8#c.f12])/5) post
eststo c4`q'


	cap matrix drop C`q'
	loc num 4
	matrix C`q'=J(`num',3,.)
	matrix colnames C`q' =   treat ll95 ul95
	matrix rownames C`q' = May June July August


forval i=1/4 {
	est restore c`i'`q'
matrix C`q'[`i',1]= _b[_cons], _b[_cons] - 1.96* _se[_cons],  _b[_cons] + 1.96* _se[_cons]
	mat list C`q'
	}
}

		coefplot (matrix(C1[,1]), ci((C1[,2]	C1[,3])) ) (matrix(C2[,1]), ci((C2[,2]	C2[,3])) ) (matrix(C3[,1]), ci((C3[,2]	C3[,3])) ) (matrix(C4[,1]), ci((C4[,2]	C4[,3])) ) (matrix(C5[,1]), ci((C5[,2]	C5[,3])) ), ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(q_all, replace) ///
scheme(plotplainblind)

		coefplot (matrix(C1[,1]), ci((C1[,2]	C1[,3])) ) (matrix(C2[,1]), ci((C2[,2]	C2[,3])) ) (matrix(C3[,1]), ci((C3[,2]	C3[,3])) ) (matrix(C4[,1]), ci((C4[,2]	C4[,3])) ) (matrix(C5[,1]), ci((C5[,2]	C5[,3])) ), ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(q_all, replace) ///
scheme(plotplainblind)

* May to September 
estimates clear
forval q=1/5 {

eststo all`q': reg lagpp c.d5#c.(f5-f9) c.d6#c.(f6-f9) c.d7#c.(f7-f9) c.d8#c.(f8-f9) d5-d8 i.t if quant_gini_can==`q', vce(cl canton)
est restore all`q'
margins, expression((_b[c.d5#c.f5] + _b[c.d5#c.f6] + _b[c.d5#c.f7] + _b[c.d5#c.f8] + _b[c.d5#c.f9])/5) post
eststo c1`q'
est restore all`q'
margins, expression((_b[c.d6#c.f6] + _b[c.d6#c.f7] + _b[c.d6#c.f8] + _b[c.d6#c.f9])/4) post
eststo c2`q'
est restore all`q'
margins, expression(( _b[c.d7#c.f7] + _b[c.d7#c.f8] + _b[c.d7#c.f9])/3) post
eststo c3`q'
est restore all`q'
margins, expression((_b[c.d8#c.f8] + _b[c.d8#c.f9])/2) post
eststo c4`q'

	cap matrix drop C`q'
	loc num 4
	matrix C`q'=J(`num',3,.)
	matrix colnames C`q' =   treat ll95 ul95
	matrix rownames C`q' = May June July August


forval i=1/4 {
	est restore c`i'`q'
matrix C`q'[`i',1]= _b[_cons], _b[_cons] - 1.96* _se[_cons],  _b[_cons] + 1.96* _se[_cons]
	mat list C`q'
	}
}

	
		coefplot (matrix(C1[,1]), ci((C1[,2]	C1[,3])) ) (matrix(C2[,1]), ci((C2[,2]	C2[,3])) ) (matrix(C3[,1]), ci((C3[,2]	C3[,3])) ) (matrix(C4[,1]), ci((C4[,2]	C4[,3])) ) (matrix(C5[,1]), ci((C5[,2]	C5[,3])) ), ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(q, replace) ///
scheme(plotplainblind)

* May to September only for dinf==0
estimates clear
forval q=1/5 {

eststo all`q': reg lagpp c.d5#c.(f5-f9) c.d6#c.(f6-f9) c.d7#c.(f7-f9) c.d8#c.(f8-f9) d5-d8 i.t if quant_gini_can==`q' & dinf==0, vce(cl canton)
est restore all`q'
margins, expression((_b[c.d5#c.f5] + _b[c.d5#c.f6] + _b[c.d5#c.f7] + _b[c.d5#c.f8] + _b[c.d5#c.f9])/5) post
eststo c1`q'
est restore all`q'
margins, expression((_b[c.d6#c.f6] + _b[c.d6#c.f7] + _b[c.d6#c.f8] + _b[c.d6#c.f9])/4) post
eststo c2`q'
est restore all`q'
margins, expression(( _b[c.d7#c.f7] + _b[c.d7#c.f8] + _b[c.d7#c.f9])/3) post
eststo c3`q'
est restore all`q'
margins, expression((_b[c.d8#c.f8] + _b[c.d8#c.f9])/2) post
eststo c4`q'

	cap matrix drop C`q'
	loc num 4
	matrix C`q'=J(`num',3,.)
	matrix colnames C`q' =   treat ll95 ul95
	matrix rownames C`q' = May June July August


forval i=1/4 {
	est restore c`i'`q'
matrix C`q'[`i',1]= _b[_cons], _b[_cons] - 1.96* _se[_cons],  _b[_cons] + 1.96* _se[_cons]
	mat list C`q'
	}
}

	
		coefplot (matrix(C1[,1]), ci((C1[,2]	C1[,3])) ) (matrix(C2[,1]), ci((C2[,2]	C2[,3])) ) (matrix(C3[,1]), ci((C3[,2]	C3[,3])) ) (matrix(C4[,1]), ci((C4[,2]	C4[,3])) ) (matrix(C5[,1]), ci((C5[,2]	C5[,3])) ), ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(q_dinf, replace) ///
scheme(plotplainblind)


* Callaway and Sant'Anna uses "long" differences.
* Here is the regression adjustment version:

gen lagpp_55circle = lagpp - L.lagpp if f5
gen lagpp_56circle = lagpp - L2.lagpp if f6
gen lagpp_57circle = lagpp - L3.lagpp if f7
gen lagpp_58circle = lagpp - L4.lagpp if f8

gen lagpp_66circle = lagpp - L.lagpp if f6
gen lagpp_67circle = lagpp - L2.lagpp if f7
gen lagpp_68circle = lagpp - L3.lagpp if f8

gen lagpp_77circle = lagpp - L.lagpp if f7
gen lagpp_78circle = lagpp - L2.lagpp if f8

gen lagpp_88circle = lagpp - L.lagpp if f8



* Use all potential controls:
reg lagpp_55circle d5 i.t if f5 , vce(cl canton)
reg lagpp_56circle d5 i.t if f6 & !d6, vce(cl canton)
reg lagpp_57circle d5 i.t if f7 & !d6 & !d7, vce(cl canton)
reg lagpp_58circle d5 i.t if f8 & (d5 | dinf), vce(cl canton)

reg lagpp_66circle d6 i.t if f6 , vce(cl canton)
reg lagpp_67circle d6 i.t if f7 & !d7, vce(cl canton)
reg lagpp_68circle d6 i.t if f8 & (d6 | dinf), vce(cl canton)

reg lagpp_77circle d7 i.t if f7 , vce(cl canton)
reg lagpp_78circle d7 i.t if f8 & (d7 | dinf), vce(cl canton)

reg lagpp_88circle d8 i.t if f8 & (d8 | dinf), vce(cl canton)

* Use only the NT group:

reg lagpp_55circle d5 i.t if f5 & (d5 | dinf), vce(cl canton)
reg lagpp_56circle d5 i.t if f6 & (d5 | dinf), vce(cl canton)
reg lagpp_57circle d5 i.t if f7 & (d5 | dinf), vce(cl canton)
reg lagpp_58circle d5 i.t if f8 & (d5 | dinf), vce(cl canton)

reg lagpp_66circle d6 i.t if f6 & (d6 | dinf), vce(cl canton)
reg lagpp_67circle d6 i.t if f7 & (d6 | dinf), vce(cl canton)
reg lagpp_68circle d6 i.t if f8 & (d6 | dinf), vce(cl canton)

reg lagpp_77circle d7 i.t if f7 & (d7 | dinf), vce(cl canton)
reg lagpp_78circle d7 i.t if f8 & (d7 | dinf), vce(cl canton)

reg lagpp_88circle d8 i.t if f8 & (d8 | dinf), vce(cl canton)


* Exploiting differencing across years
byso canton: egen temp6=max(d6)
byso canton: egen temp7=max(d7)
so canton t 

tab temp7 quant_gini_can
preserve
drop if month>9
gen tr=d6
eststo c6: xtreg agpp c.tr#(c.(f1-f9)) i.month##i.year c.temp##c.temp if (temp6) & !d5 & !d7 & !d8 & !d9, fe vce(cl canton)
*replace tr=d7
*eststo c7: xtreg lagpp c.tr#(c.(f1-f7)) i.month##i.year if (temp7) & !d5 & !d6 & !d8 & !d9, fe vce(cl canton)

forval q=6/6 {
	cap matrix drop C`q'
	loc num 9
	matrix C`q'=J(`num',3,.)
	matrix colnames C`q' =   treat ll95 ul95
	matrix rownames C`q' = Jan Feb Mar Apr May Jun Jul Aug Sep


	est restore c`q'
forval i=1/`num' {
matrix C`q'[`i',1]= _b[c.tr#c.f`i'], _b[c.tr#c.f`i'] - 1.96* _se[c.tr#c.f`i'],  _b[c.tr#c.f`i'] + 1.96* _se[c.tr#c.f`i']
	mat list C`q'
	}


		coefplot (matrix(C`q'[,1]), ci((C`q'[,2]	C`q'[,3])) ), ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) xline(5.5, lcolor(black) lpattern(shortdash)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") /*legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  )*/ name(c`q'trend, replace) ///
scheme(plotplainblind)
}
restore

*By quintile
foreach cohort in 6 7 8 {
preserve
global coefs
*keep if dinf==0
*drop if month>10
*gen tr= d6==1 | d7==1 | d8==1 
forval q=1/5 {
eststo c`q': reghdfe gpp c.d5#c.(f1-f11) c.d6#(c.(f1-f11))  c.d7#c.(f1-f11) c.d8#c.(f1-f11) c.d9#c.(f1-f11)  d5-d9 f1-f11  if quant_gini_can==`q', absorb(canton t) vce(cl canton)
*eststo c`q': reghdfe gpp c.d5#c.(f1-f11) c.d6#(c.(f1-f11))  c.d7#c.(f1-f11) c.d8#c.(f1-f11) c.d9#c.(f1-f11)  d5-d9 f1-f11 L1.c.temper##L1.c.temper if quant_gini_can==`q', absorb(canton t) vce(cl canton)
*eststo c`q': xtreg gpp c.d5#c.(f1-f10) c.d6#(c.(f1-f10))  c.d7#c.(f1-f10) c.d8#c.(f1-f10) c.d9#c.(f1-f10)  d5-d9 f1-f10 L1.c.temper##L1.c.temper i.t if quant_gini_can==`q', fe vce(cl canton)

*eststo c`q': reghdfe gpp c.tr#c.(f1-f4 f6-f12) c.d5#c.(f1-f3 f5-f12)  c.d7#c.(f1-f5 f7-f12) c.d8#c.(f1-f6 f8-f12)  c.d9#c.(f1-f7 f9-f12) d5-d9 f1-f11 if quant_gini_can==`q', absorb(canton t) vce(cl canton)

*eststo c`q': reghdfe gpp c.tr#c.(f1-f11) d5-d9 f1-f11 if quant_gini_can==`q', absorb(canton t) vce(cl canton)

	cap matrix drop C`q'
	loc num 9
	matrix C`q'=J(`num',3,.)
	matrix colnames C`q' =   treat ll95 ul95
	matrix rownames C`q' = Jan Feb Mar Apr May Jun Jul Aug Sep


	est restore c`q'
forval i=1/`num' {
cap matrix C`q'[`i',1]= _b[c.d`cohort'#c.f`i'], _b[c.d`cohort'#c.f`i'] - 1.96* _se[c.d`cohort'#c.f`i'],  _b[c.d`cohort'#c.f`i'] + 1.96* _se[c.d`cohort'#c.f`i']
	mat list C`q'
	}
global coefs "${coefs} (matrix(C`q'[,1]), ci((C`q'[,2]	C`q'[,3]))) "
dis "${coefs}"
}
restore		

loc xline=`cohort'-1
coefplot ${coefs}, ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) xline(`xline'.5, lcolor(black) lpattern(shortdash)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(gini_c`q'trend_d`cohort', replace) ///
scheme(plotplainblind)
graph export "$Dtabs/het_etwfe_d`cohort'.png", replace 
}

eststo c7: xtreg lagpp c.tr#(c.(f1-f12)) c.(f1-f12) i.t if (temp6) & quant_gini_can==5, fe
*drop if month>6
replace tr=d6
eststo c6: reg lagpp c.tr#(c.(f1-f6)) c.(f1-f6) i.t d5-d9 dinf  if (temp6)

suest c7 c6, vce(cl canton)

xtreg lagpp c.d6#(c.(f1-f6)) c.(f1-f6) i.t if (temp6), fe vce(cl canton)


* Heterogeneous linear trends

reg lagpp c.w#c.d5#c.(f5-f9) c.w#c.d6#c.(f6-f9) c.w#c.d7#c.(f7-f9) c.w#c.d8#c.(f8-f9) ///
 c.d5#c.(f2-f4)#i.month c.d6#c.(f2-f5)#i.month  c.d7#c.(f2-f6)#i.month  c.d8#c.(f2-f7)#i.month d5-d8 i.month if year==2015, vce(cl canton)

reg lagpp c.w#c.d5#c.(f5-f9) c.w#c.d6#c.(f6-f9) c.w#c.d7#c.(f7-f9) c.w#c.d8#c.(f8-f9) ///
 c.d5#c.month c.d6#c.month  c.d7#c.month  c.d8#c.month d5-d8 c.month##i.year , vce(cl canton)


lincom (c.d5#c.f5 + c.d5#c.f6 + c.d5#c.f7 + c.d5#c.f8 + c.d5#c.f9)/5


xtreg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) i.t F.w if year==2015, fe vce(cl canton)
reg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 i.t if year==2015 , vce(cl canton)
xtreg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) i.t if year==2015 , fe vce(cl canton)
lincom (c.d5#c.f5 + c.d5#c.f6 + c.d5#c.f7 + c.d5#c.f8 + c.d5#c.f9)/5
lincom (c.d5#c.f6 + c.d5#c.f7 + c.d5#c.f8 + c.d5#c.f9)/5

xtreg lagpp w i.year##i.month, fe vce(cl canton)

lincom (c.d6#c.f6 + c.d6#c.f7 + c.d6#c.f8 + c.d6#c.f9)/4
lincom (c.d5#c.f5 + c.d5#c.f6 + c.d5#c.f7 + c.d5#c.f8 + c.d5#c.f9)/5 - (c.d6#c.f6 + c.d6#c.f7 + c.d6#c.f8 + c.d6#c.f9)/4

preserve
drop if f10==1 | f11==1 | f12==1
reg lagpp c.w#c.d5#c.(f5-f9) c.w#c.d6#c.(f6-f9) c.w#c.d7#c.(f7-f9) c.w#c.d8#c.(f8-f9) ///
 c.d5#c.(f2-f4) c.d6#c.(f2-f5) c.d7#c.(f2-f6) c.d8#c.(f2-f7) d5-d9 i.month##i.year , vce(cl canton)
 xtreg lagpp c.w#c.d5#c.(f5-f9) c.w#c.d6#c.(f6-f9) c.w#c.d7#c.(f7-f9) c.w#c.d8#c.(f8-f9) ///
 c.d5#c.(f2-f4) c.d6#c.(f2-f5) c.d7#c.(f2-f6) c.d8#c.(f2-f7)  i.month##i.year , fe vce(cl canton)
restore
binscatter hot30 year, line(connect) 

reg lagpp i.year##(c.w#c.d5#c.(f5-f12) c.w#c.d6#c.(f6-f12) c.w#c.d7#c.(f7-f12) c.w#c.d8#c.(f8-f12) ///
 c.d5#c.(f2-f4) c.d6#c.(f2-f5) c.d7#c.(f2-f6) c.d8#c.(f2-f7) d5-d8) i.t, vce(cl canton)


xtreg lagpp c.w#c.d5#c.(f5-f12) c.w#c.d6#c.(f6-f12) c.w#c.d7#c.(f7-f12) c.w#c.d8#c.(f8-f12) ///
 c.d5#c.(f2-f4) c.d6#c.(f2-f5) c.d7#c.(f2-f6) c.d8#c.(f2-f7) d5-d8 i.t, fe vce(cl canton)


c.(d5-d8)#c.(f5-f12) d5-d9 i.t if year==2015, vce(cluster canton)



reg agpp c.(d1-d12)#c.(f2-f12) d1-d12 i.t, vce(cluster canton)
xtreg agpp c.(d1-d12)#c.(f1-f12) i.t, fe vce(cluster canton)

reg agpp c.(d5-d9)#c.(f5-f12) d5-d9 i.t if year==2015, vce(cluster canton)


reg agpp (c.(d5-d9)#c.(f1-f11) d5-d9)##b2015.year i.month, vce(cluster canton)
reg agpp c.(d5-d9)#c.(f1-f11) d5-d9 i.t if year==2016, vce(cluster canton)


reg agpp c.w#c.(d5-d9)#c.(f2-f12) c.(d5-d9)#c.(f2-f12) d5-d9 i.t, vce(cluster canton)
xtreg agpp c.w#c.(d5-d9)#c.(f2-f12) c.(d5-d9)#c.(f2-f12) i.t, fe vce(cluster canton)

*/



br canton t ${shock} agpp
* create never treated group
cap drop dinf
cap drop temp
byso canton: egen temp=max(${shock})
gen dinf=(temp-1)*-1
ta dinf

keep year canton t ${shock} agpp dinf quant_gini_can

br canton t ${shock} agpp dinf
tab t, gen(f)


cap drop w
*byso canton year: egen w=max(hot30)
byso canton: gen w=sum(${shock})
replace w=1 if w>0
br year canton t ${shock} agpp dinf w 

forval m=1/84 {
	cap drop d`m'
	cap drop temp`m'
	gen temp`m'=${shock} *f`m'
	if `m'==1{

	 byso canton: egen d`m'=max(temp`m')
	}
	else {
loc mt=`m'-1
 byso canton: egen d`m'=max(temp`m') if d`mt'==0
	}
		drop temp`m'
}

	forval m=1/84 {
replace d`m'=0 if d`m'==.

	}

cap drop test
egen test=rowtotal(d1-d84)
ta test
drop test

reg agpp c.(d5-d84)#c.(f1-f84) d5-d84 i.t, vce(cluster canton)

reg agpp c.(d5-d9)#c.(f5-f9) d5-d9 if year==2015, vce(cluster canton)
reg agpp c.(d6-d9)#c.(f6-f9) d6-d9 i.t if year==2015 & quant_gini_can==5, vce(cluster canton)
reg agpp c.(d6-d9)#c.(f1-f12) d6-d9 i.t if year==2015 & quant_gini_can==1, vce(cluster canton)

reg agpp c.(d6-d9)#c.(f1-f12) d6-d9 i.t if year==2015 & quant_gini_can==5 & dinf==0, vce(cluster canton)

xtreg agpp c.(d5-d9)#c.(f1-f12) i.t if year==2015 & quant_gini_can==5 & dinf==0, fe vce(cluster canton)

reg y c.d4#c.f04 c.d4#c.f05 c.d4#c.f06 ///
	c.d5#c.f05 c.d5#c.f06 ///
	c.d6#c.f06 ///
	d4 d5 d6 i.year, vce(cluster id)


* Compute average effect by cohort:
lincom (c.d4#c.f04 + c.d4#c.f05 + c.d4#c.f06)/3
lincom (c.d5#c.f05 + c.d5#c.f06)/2
	


forval q=1/1 {
qui jwdid  agpp if quant_gini_can==`q', ivar(canton) tvar(t) gvar(first_month)
estat simple
}




* Consistent with clean control method à la de Chaisemartin et D’Haultfœuille?

global shock the_shock
cap drop temp
gen temp=t if ${shock}==1
cap drop first_month
byso canton: egen first_month=min(temp)
replace first_month=0 if first_month==.


so canton t 

* By month
* create never treated group
cap drop dinf
cap drop temp
byso canton year: egen temp=max(${shock})
gen dinf=(temp-1)*-1
ta dinf


br canton t ${shock} agpp dinf
tab month, gen(f)

cap drop w
byso canton year: gen w=sum(${shock})
replace w=1 if w>0

forval q=1/5 {
preserve	
	*keep if quant_gini_can==`q' & year==2019 & month>2 & month<11
	keep if quant_gini_can==`q' 
did_multiplegt agpp canton t w, placebo(3) breps(10) robust_dynamic dynamic(3) cluster(canton)
restore
}




ereturn list


* and heterogeneous treatment effects à la de Chaisemartin et D’Haultfœuille?
twowayfeweights mean_ion_npp canton year treatment if qarea>1,  type(feTR) brepscluster(canton) breps(20)






