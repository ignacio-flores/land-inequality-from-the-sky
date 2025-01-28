global ID "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
*global ID "~/Dropbox/land_ineq_degradation/"
global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land inequality New/tabs"
*global Dtabs "~/Dropbox/Aplicaciones/Overleaf/Land Inequality from the Sky/tabs"
global rep "$ID"
cd "$ID"
u data/FR/multi_panel_foods_pa28, clear
*u data/FR/_with_c28/working_panel_8days_farms_wi28.dta, clear
*/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/data/FR/_with_c28
*u data/FR/_with_c28/working_panel_month_foods
cap drop canton
encode adm_can, g(canton)
so canton t
sum crop28,d



cap drop season
gen season=1 if month>0 & month<=3
replace season=2 if month>3 & month<=6
replace season=3 if month>6 & month<=9
replace season=4 if month>9
ta season

global inter w
*global inter m
* List variables 

global rename "ccount dcount dherf"

foreach var in $rename {
rename `var'_o `var'
}


* Normalize crop count canton-size 
cap drop count_km2
gen count_km2=dcount/(canton_area_${inter})

global het_vars "gini_can dcount count_km2 dherf"
global dep_vars "gpp_${inter}"

egen cm_tag=tag(canton month year)
so canton t
byso canton : gen cgpp_w=sum(gpp_w)
gen tempvar=gpp_m if cm_tag
so canton t
byso canton : gen cgpp_m=sum(tempvar)
* Create baseline het variables, y for gini, y-1 for crops
drop tempvar
gen tempvar=gpp_w if the_shock_y_w
byso canton : gen cgpp_w_shock=sum(tempvar)

drop tempvar
gen tempvar=gpp_w if !the_shock_y_w
byso canton : gen cgpp_w_noshock=sum(tempvar)
drop tempvar

global base_year 2015
loc the_shock the_shock_${inter}
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


	* Create interactions for continuous het and also by quantiles
	loc q=4
	cap drop `the_shock'X`var'
	gen `the_shock'X`var'=`the_shock'*`var'
	cap drop quant_`var'
	xtile quant_`var'= `var', nq(`q') 
	cap drop q_`var'*
	tab quant_`var',gen(q_`var')
	forval x= 1/`q' {
		capture drop t_q_`var'`x'
		gen t_q_`var'`x'=`the_shock'*q_`var'`x'
	}
}

cap n gen the_shock_${inter}=the_shock
cap n gen gpp_${inter}=gpp
cap n gen lgpp_${inter}=ln(gpp_${inter})
cap n gen lcgpp_${inter}=ln(cgpp_${inter})
cap n gen temp_${inter}=temp
cap n gen the_shock_y_${inter}=the_shock_y
ta month the_shock_${inter}

gen tsup_h_q_r=round(tsup_h_q)

cap drop any_the_shock_y_w
byso canton: egen any_the_shock_y_w=max(the_shock_y_w)

binscatter gpp_w tsup_h_q_r, by(quant_gini_can) line(connect) discrete
ta tsup_h_q_r
return list
binsreg gpp_w tsup_h_q, by(quant_gini_can) absorb(canton##month year)
sum tsup_h_q if the_shock_w,d
cap drop p_gpp_w
reghdfe gpp_w c.tsup_h_q##b1.quant_gini_can if tsup_h_q>25 & inrange(p, 11, 33), absorb(canton month) vce(cl canton) resid
predict p_gpp_w, xbd
binscatter p_gpp_w tsup_h_q if tsup_h_q>25 & inrange(p, 11, 33), by(quant_gini_can) line(connect) 


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
(connected gpp temp_w if  quant_gini_can==5, color(gs4) sort legend(order(1 2 3 4 5)  label(1 "q1") label(2 "q2") label(3 "q3") label(4 "q4") label(5 "q5") pos(6) row(2))) ///
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
loc stats "stats(r2 N , label("R-squared" "N") fmt(%9.3f 0))" 
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
(connected gpp temp_w if  quant_gini_can==5, color(gs4) sort legend(order(1 2 3 4 5)  label(1 "q1") label(2 "q2") label(3 "q3") label(4 "q4") label(5 "q5") pos(6) row(2))) ///
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
		xlabel(${lab})
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
		xlabel(${lab}) yline(0, lcolor(gs0)) vertical mcolor(gs5) omitted
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

coefplot med_gini_can*, keep(c.dweek#c.t*#c.med_gini_can) noci vertical xlabel(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(gini,replace) scheme(plotplainblind)


coefplot med_dherf*, keep(c.dweek#c.t*#c.med_dherf) noci vertical xlabel(4.5 "Heat shock") xline(4.5) yline(0) recast(connected)  name(herf,replace) scheme(plotplainblind)

coefplot med_gini_can_p25, keep(c.dweek#c.t*#c.med_gini_can) vertical xlabel(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(gini,replace) scheme(plotplainblind)

coefplot med_dherf_p25, keep(c.dweek#c.t*#c.med_dherf) vertical xlabel(4.5 "Heat shock") xline(4.5) yline(0) recast(connected) name(herf,replace) scheme(plotplainblind)


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




