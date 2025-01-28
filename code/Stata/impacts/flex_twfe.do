*global ID "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
global ID "~/Dropbox/land_ineq_degradation/"
*global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky/tabs"
global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
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
u data/FR/working_panel_foods.dta, clear

merge m:1 adm_can using data/FR/admin_canton, nogen
foreach var of var name_* {
encode `var', g(n`var')
}


xtset canton t


* Normalizing crop count by size of canton
cap drop count_km2
gen count_km2=dcount/canton_area
sum count_km2,d

gen P_A=farm_perim_est/farm_area_est

egen max_crop=rowmax(crop*)
sum max_crop,d

foreach var in count_km2 gini_can P_A max_crop {
cap drop quant_`var'
cap drop q_`var'*
xtile quant_`var'= `var', nq(5) 
tab quant_`var',gen(q_`var')
tab1 q_`var'*
}


xtset canton t
xtsum gini_can


global shock hot30
cap drop temp
gen temp=t if ${shock}==1
cap drop first_month
byso canton: egen first_month=min(temp)
replace first_month=0 if first_month==.


so canton t 

* By month

br canton t ${shock} agpp
* create never treated group
cap drop dinf
cap drop temp
byso canton year: egen temp=max(${shock})
gen dinf=(temp-1)*-1
ta dinf


br canton t ${shock} agpp dinf
tab month, gen(f)

keep year canton t ${shock} agpp dinf quant_gini_can month f*

cap drop w
*byso canton year: egen w=max(hot30)
byso canton year: gen w=sum(${shock})
replace w=1 if w>0
br year canton t ${shock} agpp dinf w 

forval m=1/12 {
	cap drop d`m'
	cap drop temp`m'
	*gen temp`m'=${shock} *f`m'
	gen temp`m'=w *f`m'
	if `m'==1{

	 byso canton year: egen d`m'=max(temp`m')
	}
	else {
loc mt=`m'-1
 byso canton year: egen d`m'=max(temp`m') if d`mt'==0
	}
		drop temp`m'
}

	forval m=1/12 {
replace d`m'=0 if d`m'==.

	}

cap drop test
egen test=rowtotal(d1-d12)
ta test
drop test
tab1 d1-d12

reg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 i.t, vce(cl canton)
xtreg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 i.t, fe vce(cl canton)
so canton t
gen lagpp=ln(agpp)

* Using all post months
estimates clear
forval q=1/5 {
eststo all`q': reg lagpp c.d5#c.(f5-f12) c.d6#c.(f6-f12) c.d7#c.(f7-f12) c.d8#c.(f8-f12) d5-d8 i.t if quant_gini_can==`q', vce(cl canton)
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
cap drop temp*
byso canton: egen temp6=max(d6)
byso canton: egen temp7=max(d7)
so canton t 
br canton year month t hot30 w d6 temp

tab temp7 quant_gini_can
preserve
drop if month>9
gen tr=d6
eststo c6: xtreg lagpp c.tr#(c.(f1-f9)) i.month##i.year if (temp6) & !d5 & !d7 & !d8 & !d9, fe vce(cl canton)
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
preserve
global coefs
drop if month>9
gen tr=d6
forval q=1/5 {
eststo c`q': xtreg lagpp c.tr#(c.(f1-f9)) i.month##i.year if (temp6) & !d5 & !d7 & !d8 & !d9 & quant_gini_can==`q', fe vce(cl canton)
*replace tr=d7
*eststo c7: xtreg lagpp c.tr#(c.(f1-f7)) i.month##i.year if (temp7) & !d5 & !d6 & !d8 & !d9, fe vce(cl canton)

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
global coefs "${coefs} (matrix(C`q'[,1]), ci((C`q'[,2]	C`q'[,3]))) "
dis "${coefs}"
}
restore		

coefplot ${coefs}, ///
	yline(0, lcolor(black)) vertical  xlabel($lab_vars, labsize(small) angle(45)) xline(5.5, lcolor(black) lpattern(shortdash)) ///
ciopts(/*recast(rline) */lpattern(dash)) ytitle("") legend(row(1) order(2 "q1" 4 "q2" 6 "q3" 8 "q4" 10 "q5") cols(1) ring(0) position(6)  ) name(gini_c`q'trend, replace) ///
scheme(plotplainblind)

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






