global DG "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky/tabs"
global rep "$DG"
cd "$DG"


u data/FR/MODIS/satellite_to_canton.dta, clear
keep adm_can name_*
duplicates drop 
duplicates report adm_can
saveold data/FR/admin_canton, replace


*u data/FR/working_panel_canton.dta, clear
*u data/FR/working_panel_farms.dta, clear
u data/FR/working_panel_8days_foods.dta, clear

merge m:1 adm_can using data/FR/admin_canton, nogen
foreach var of var name_* {
encode `var', g(n`var')
}


xtset canton t




* Normalizing crop count by size of canton
cap drop count_km2
gen count_km2=dcount/canton_area
sum count_km2,d

cap drop fcount_km2
gen fcount_km2=dcount/farm_area_est
sum fcount_km2,d

gen P_A=farm_perim_est/farm_area_est

egen max_crop=rowmax(crop*)
sum max_crop,d

foreach var in count_km2 fcount_km2 gini_can P_A max_crop {
cap drop quant_`var'
cap drop q_`var'*
xtile quant_`var'= `var', nq(5) 
tab quant_`var',gen(q_`var')
tab1 q_`var'*
}

binscatter  quant_fcount_km2 quant_count_km2, line(connect) discrete

gen rat=farm_area_est/canton_area
binscatter rat canton_area, by(quant_gini_can) line(connect) name(g2, replace)

binscatter quant_count_km2 quant_fcount_km2, by(quant_gini_can) line(connect) discrete

binscatter quant_count_km2 quant_fcount_km2, by(quant_gini_can) line(connect) discrete


* Relationship dcount and Gini
binscatter2 gini_can count_km2, line(connect) name(corr_gini_bio, replace) 
binscatter2 gini_can count_km2, line(connect) absorb(name_1) name(corr_gini_bio, replace) 
reghdfe gini_can count_km2, noabsorb vce(cl canton)

reghdfe gini_can count_km2, absorb(name_1) vce(cl canton)
reghdfe gini_can count_km2, absorb(name_2) vce(cl canton)
reghdfe gini_can count_km2, absorb(name_3) vce(cl canton)
// Strongly negatively correlated
reghdfe gini_can count_km2, absorb(canton t) vce(cl canton) // Turns positive
xtreg gini_can, fe // 98% of variation is between cantons!! Can't use canton fixed effects when examing relationship with gini and other variables




cap drop lagpp
gen lagpp=ln(agpp)

* Reproduce main effects using biodiversity measure or gini
capture drop t_q_*
foreach var of var q_* {
gen t_`var'=hot30*`var'
}

ta year

xtset canton t
xtsum gini_can

forval q=1/5 {
eststo paraq`q': reghdfe lagpp hot30 if quant_gini_can==`q', absorb(canton t) vce(cl canton)
}

cap drop temp
gen temp=t if hot30==1
cap drop first_month
byso canton: egen first_month=min(temp)
replace first_month=0 if first_month==.

forval q=1/5 {
qui jwdid  lagpp if quant_gini_can==`q', ivar(canton) tvar(t) gvar(first_month)
estat simple
}

forval q=1/5 {
qui jwdid  lagpp if quant_count_km2==`q', ivar(canton) tvar(t) gvar(first_month)
estat simple
}

forval q=5/5 {
qui csdid lagpp if quant_gini_can==`q', ivar(canton) time(t) gvar(first_month)
estat simple
}


reghdfe agpp i.quant_gini_can, absorb(name_2 t) vce(cl canton)
binscatter2 count_km2 farm_area_est, line(connect) name(count_time, replace) absorb(name_2 t)



binscatter2 agpp count_km2, line(connect) name(count_time, replace) absorb(name_2 t)

binscatter2 agpp gini_can, line(connect) name(count_time, replace) 
binscatter2 count_km2 year, line(connect) name(count_time, replace) 
binscatter2 agpp year, line(connect) name(count_time, replace) 
binscatter2 agpp farm_area_est, line(connect) name(count_time, replace) 


binscatter2 gini_can count_km2, line(connect) name(corr_gini_bio, replace) 
binscatter2 gini_can count_km2, line(connect) absorb(name_1) name(corr_gini_bio, replace) 
reghdfe gini_can count_km2, noabsorb vce(cl canton)

reghdfe gini_can count_km2, absorb(name_1 t) vce(cl canton)
reghdfe gini_can count_km2, absorb(name_2 t) vce(cl canton)
reghdfe gini_can count_km2, absorb(name_3 t) vce(cl canton)
// Strongly negatively correlated
reghdfe gini_can count_km2, absorb(canton t) vce(cl canton) // Turns positive
xtreg gini_can, fe // 98% of variation is between cantons!! Can't use canton fixed effects when examing relationship with gini and other variables

binscatter2 gini_can quant_count_km2, line(connect) name(quin_gini_bio, replace)

cap drop lagpp
gen lagpp=ln(agpp)

* Reproduce main effects using biodiversity measure or gini
capture drop t_q_*
foreach var of var q_* {
gen t_`var'=hot30*`var'
}

ta year

xtset canton t
xtsum gini_can

foreach dep in fcount_km2 gini_can {
foreach var in hot30 {
global for "3"
global lag "3"
global shock=${for}+1
global xlabel
loc i=1
forval q=1/5 {
eststo paraq`q': reghdfe lagpp f(${for}/0).`var' l(1/${lag}).`var' if q_`dep'`q'==1, absorb(canton t) vce(cl canton)
}

loc end= ${lag}+ ${for}+1
mylabels -${for}(1)${lag}, myscale(@+${for}+1) local(labels) prefix(t)
#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* `var' L*) 
xlabel(`labels') yline(0, lcolor(gs0)) xline(${shock}) vertical recast(line) offset(0) ylabel(.1(.01)-.1)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on Log AGPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") cols(1) ring(0) position(6)  ) name(`dep', replace)
scheme(plotplainblind);
#delimit cr
}
}


* What happens when you impose biodiversity?
foreach dep in gini_can {
foreach var in hot30 {
global for "3"
global lag "3"
global shock=${for}+1
global xlabel
loc i=1
forval q=1/5 {
eststo paraq`q': reghdfe lagpp f(${for}/0).`var' l(1/${lag}).`var' if q_`dep'`q'==1 & quant_count_km2==5, absorb(canton t) vce(cl canton)
}

loc end= ${lag}+ ${for}+1
mylabels -${for}(1)${lag}, myscale(@+${for}+1) local(labels) prefix(t)
#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* `var' L*) 
xlabel(`labels') yline(0, lcolor(gs0)) xline(${shock}) vertical recast(line) offset(0) ylabel(.1(.1)-.5)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on GPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") cols(1) ring(0) position(6)  ) name(`dep'_counter, replace)
scheme(plotplainblind);
#delimit cr
}
}

estimates clear
* All
cap drop lagpp
gen lagpp=ln(agpp)
eststo hot1: reghdfe lagpp t_q_gini* q_gini*, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal
sca p_quintiles1=round(r(p), .0001)

eststo hot2: reghdfe lagpp t_q_gini* q_gini* if quant_count_km2==5, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal
sca p_quintiles2=round(r(p), .0001)

eststo hot3: reghdfe lagpp t_q_gini* q_gini* if quant_P_A==5, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal
sca p_quintiles3=round(r(p), .0001)




binscatter P_A quant_count_km2, line(connect) discrete

#delimit
coefplot (hot1, offset(0)) (hot2, offset(.1)),  keep(t_q_*)  yline(0, lcolor(gs0)) vertical 
ciopts(/*recast(rline) */lpattern(dash)) ytitle("Impact on log GPP") connect(line)  legend( order(2 4 6) rows(3) label(2 "Baseline") label(4 "5th quintile biodiversity") label(6 "5th quintile Perim./Area")) 
xlabel(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5") xtitle("Quantiles of Gini")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
#delimit cr

eststo mono1: reghdfe lagpp t_q_gini* q_gini* if quant_max_crop==1, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal
sca p_quintiles3=round(r(p), .0001)

eststo mono5: reghdfe lagpp t_q_gini* q_gini* if quant_max_crop==5, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal
sca p_quintiles3=round(r(p), .0001)

#delimit
coefplot (hot1, offset(0)) (mono1, offset(.1)) (mono5, offset(-.1)),  keep(t_q_*)  yline(0, lcolor(gs0)) vertical 
ciopts(/*recast(rline) */lpattern(dash)) ytitle("Impact on log GPP") connect(line)  legend( order(2 4 6) rows(3) label(2 "Baseline") label(4 "<12% monoculture (1st quintile crop proportion)") label(6 ">80% monoculture (5th quintile of crop proportion)")) 
xlabel(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5") xtitle("Quantiles of Gini")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
#delimit cr

#delimit
coefplot hot*, keep(t_q_*)  yline(0, lcolor(gs0)) vertical 
ciopts(/*recast(rline) */lpattern(dash)) ytitle("Impact on log GPP") connect(line)
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr


#delimit
coefplot hot*, keep(t_q_*)  yline(0, lcolor(gs0)) vertical mcolor(gs5) offset(0)
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on log GPP")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr

eststo hot: reghdfe agpp t_q_count_km2* q_count_km2*, absorb(canton i.t) vce(cl canton)
testparm t_q_gini*, equal

gen P_A=farm_perim_est/farm_area_est
sum P_A,d
eststo hot: reghdfe agpp c.P_A##(t_q_gini* q_gini*), absorb(canton i.t) vce(cl canton)
eststo hot: reghdfe agpp c.P_A##(t_q_cou* q_cou*), absorb(canton i.t) vce(cl canton)

count if farm_perim_est<farm_area_est

ta year

binscatter gini_can t, line(connect) discrete
reghdfe gini_can t, noabsorb vce(cl canton)

binscatter count_km2 t, line(connect) discrete
binscatter dcount t, line(connect) discrete
reghdfe count_km2 t, absorb(canton) vce(cl canton)
binscatter gini_can t, line(connect)

byso quant_count_km2: sum agpp if hot30_y
byso quant_count_km2: sum agpp if !hot30_y

binscatter count_km2 t, line(connect) discrete
binscatter gini_can year, line(connect) discrete



ta ccount
reghdfe q_gini_can5 crop*, absorb(nname_1 t) vce(cl canton)
reghdfe q_gini_can5 dcount, absorb(nname_1 t) vce(cl canton)


byso quant_max_crop: sum max_crop


binscatter gini_can max_crop , line(connect)

binscatter gini_can herfindahl, line(connect)

gen herfindahl_km2=herfindahl/canton_area


binscatter dcount herfindahl_km2, line(connect)


binscatter count_km2 max_crop , line(connect)

logit q_gini_can5 crop2-crop28 i.t i.nname_1,  vce(cl canton)



* There is no within canton variation in dcount by year
* HH index should be scaled by area as well??



