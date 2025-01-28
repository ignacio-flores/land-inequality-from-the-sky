
global DG "/Users/dylanglover/Dropbox (Personal)/land_ineq_degradation/"
global Dtabs "/Users/dylanglover/Dropbox (Personal)/Apps/Overleaf/Land Inequality from the Sky/tabs"
global rep "$DG"
cd "$DG"

***************************
* Agricultural census data *
****************************

* Cleaning and mergin census years

*https://agreste.agriculture.gouv.fr/agreste-web/disaron/G_2007/detail/
foreach base in 1988 2000 2010 {
import delimited using "${DG}/data/FR/agri_census/FDS_G_2007_`base'.txt", delim(";") varn(1) clear
keep if frdom=="METRO" & com!="............"
keep if g_2007_lib_dim2=="Ensemble des exploitations"

drop france frdom g_2007_dim1 g_2007_dim2 g_2007_lib_dim2 g_2007_dim3

tostring g_2007_mod_dim1, replace
tostring g_2007_mod_dim3, replace
cap drop size
gen size_type=g_2007_mod_dim1+g_2007_mod_dim3
ta size_type

drop g_2007_mod_dim1 g_2007_lib_dim1 g_2007_mod_dim2 g_2007_mod_dim3 g_2007_lib_dim3 qualite

reshape wide valeur, i(com) j(size_type) s
sa "${DG}/data/FR/agri_census/temp`base'", replace
}

************************************************
* Looking at evolution of agri land inequality *
************************************************
u "${DG}/data/FR/agri_census/temp1988", clear
append using "${DG}/data/FR/agri_census/temp2000"
append using "${DG}/data/FR/agri_census/temp2010"

* Adding 2020
collapse (sum) valeur11 valeur12, by(annref)

set obs `=_N+1'
replace valeur11=416054 if _n==_N
replace valeur12=26864337 if _n==_N
replace annref=2020 if _n==_N

* https://agreste.agriculture.gouv.fr/agreste-saiku/?plugin=true&query=query/open/RA2020_001#query/open/RA2020_001

* Converting agricultural hectars into km square
gen areakm2=valeur12*.01

* Number of farms 
gen farms=valeur11

* Area per farm ratio
cap drop r_area_farm
gen r_area_farm=areakm2/farms
sum r_area_farm

replace areakm2=log(areakm2)
replace farms=log(farms)

*graph bar (mean) r_area_farm, over(annref)

* Illustration of the trends
eststo y1988a: mean areakm2 if annref==1988
eststo y2000a: mean areakm2 if annref==2000
eststo y2010a: mean areakm2 if annref==2010
eststo y2020a: mean areakm2 if annref==2020

eststo y1988b: mean farms if annref==1988
eststo y2000b: mean farms if annref==2000
eststo y2010b: mean farms if annref==2010
eststo y2020b: mean farms if annref==2020

coefplot (y1988b y1988a , color(gs9) ) ///
(y2000b y2000a , color(gs6) ) ///
(y2010b y2010a , color(gs3) ) ///
(y2020b y2020a , color(gs0) ), ///
noci vertical xlabel(1 "log(Farms)" 2 "log(Area km2)") ytitle(Log points)  ///
legend(row(1) order(1 "1988" 2 "2000" 3 "2010" 4 "2020") cols(1) ring(0) position(12) ) ///
recast(bar)  barwidth(0.2) fcolor(*.5) scheme(plotplainblind) ylabel(12(.5)14)
graph export "$Dtabs/farms_and_area.pdf", replace 

* Illustration of rise in inequality
eststo y1988: mean r_area_farm if annref==1988
eststo y2000: mean r_area_farm if annref==2000
eststo y2010: mean r_area_farm if annref==2010
eststo y2020: mean r_area_farm if annref==2020

coefplot (y1988 , color(gs9) ) ///
(y2000 , color(gs6) ) ///
(y2010, color(gs3) ) ///
(y2020, color(gs0) ), ///
noci vertical xlabel(1 "Area/Farms") ytitle("Area (km2 per farm)")  ///
legend(row(1) order(1 "1988" 2 "2000" 3 "2010" 4 "2020") cols(1) ring(0) position(12) ) ///
recast(bar)  barwidth(0.2) fcolor(*.5) scheme(plotplainblind) ylabel(.2(.1).7)
graph export "$Dtabs/area_per_farm.pdf", replace 

* % change in Big farms

dis (10539741 - 8751751)/8751751

u data/FR/working_panel, clear


* Understand relationship between gpp and npp
cap drop canton
encode adm_can, g(canton)

cap drop year_gpp
byso canton year: egen year_gpp=mean(month_gpp)


binscatter mean_nf_npp year_gpp, line(connect)

reg mean_nf_npp year_gpp,  vce(cl canton)


cap drop year_gini_can
byso canton year: egen year_gini_can=mean(gini_can)

reghdfe mean_nf_npp, absorb(canton year) vce(cl canton)  res
cap drop r_mean_nf_npp
predict r_mean_nf_npp, r


binscatter2 mean_ion_npp year_gini_can, line(connect) absorb(canton year) n(10)

binscatter2 mean_nf_npp year_gpp, line(connect) absorb(canton year) n(10)



binscatter r_mean_nf_npp year_gini_can if area_agri>50, line(connect)

reg mean_nf_npp year_gpp,  vce(cl canton)


/*
ssc install ftools
ssc install reghdfe
ssc install gtools
net install binscatter2, from("https://raw.githubusercontent.com/mdroste/stata-binscatter2/master/")
*/


*xi i.adm

set matsize 5000
cap drop bins*
* Agri and gini

//unconditional
*binsreg gini_can area_agri

*******************************************************
* Inequality evolution over time using cadastral data *
*******************************************************

sum area_agri,d
* Problems with this calculation: some cantons have >100%. Top code for now.
replace area_agri=r(p99) if area_agri>r(p99)
kdensity area_agri
cap drop qarea
* Generating quintiles of agri area in 2010 for heterogeneity analysis
cap drop area2010
gen area2010=area_agri if year==2010
cap drop ref_area
byso canton: egen ref_area=max(area2010)
sum ref_area,d
global quants=5
xtile qarea=ref_area, nq($quants)

cap drop q_*
tab qarea,gen(q_)
tab1 q_*

binscatter gini_nat year, line(connect) scheme(plotplainblind) discrete xtitle("Year") ytitle("Land Gini")
graph export "$Dtabs/bins_gini_can_year.pdf", replace 


so canton year month
byso canton: gen t=_n
xtset canton t
xtreg month_gpp, fe



sum month_gpp, d
gen lmonth_gpp=ln(month_gpp)

gen lmonth_gpp_farms=ln(month_gpp_farms)

reg month_gpp gini_can, vce(cl canton)
reghdfe month_gpp gini_can, absorb(month) vce(cl canton)
reghdfe month_gpp gini_can, absorb(canton month) vce(cl canton)
* No correlation with gini
reghdfe month_gpp gini_can if qarea>1, absorb(canton t) vce(cl canton)
* Still nothing. At odds with NPP!

binscatter lmonth_gpp gini_can, line(connect) scheme(plotplainblind) xtitle("Land Gini") ytitle("Log GPP") n(10)
graph export "$Dtabs/bins_lgpp_gini_can.pdf", replace 

reghdfe lmonth_gpp, absorb(canton month) vce(cl canton)  res
cap drop r_lmonth_gpp
predict r_lmonth_gpp, r

binscatter r_lmonth_gpp gini_can, line(connect) scheme(plotplainblind) xtitle("Land Gini") ytitle("Resid. Log GPP") n(10)
graph export "$Dtabs/res_bins_lgpp_gini_can.pdf", replace 


*********************



****************************
**** Temperature and GPP ***
****************************
cap drop hot
gen hot=mean_temp>25
ta hot



binscatter r_lmonth_gpp mean_temp, line(connect) scheme(plotplainblind) xtitle("Temperature") ytitle("Resid. Log GPP") n(10)
graph export "$Dtabs/bins_lgpp_temp.pdf", replace 


binscatter r_lmonth_gpp mean_temp if hot, line(connect) scheme(plotplainblind) xtitle("Temperature") ytitle("Resid. Log GPP") n(10)
graph export "$Dtabs/bins_lgpp_temp_hot.pdf", replace 


reghdfe hot q_*, absorb(canton t) vce(cl canton)



****************************
**** Temperature and Gini ***
****************************

reg mean_temp gini_can, vce(cl canton)

reghdfe mean_temp gini_can, absorb(canton t) vce(cl canton)  res
cap drop r_hot
predict r_hot, r
* Now no correlation

reg hot  , vce(cl canton) 


byso month: sum mean_temp,d

cap drop m_temp
byso month canton: egen m_temp=mean(mean_temp)
cap drop sd_temp
byso month canton: egen sd_temp=sd(mean_temp)

cap drop hot2 
gen hot2=(mean_temp>m_temp+2*sd_temp)


*replace extreme=(mean_temp<m_temp-1*sd_temp)
*ta extreme
so canton t
br canton t month year mean_temp m_temp sd_temp hot hot2


binscatter2 hot year, line(connect) scheme(plotplainblind) xtitle("Temperature") ytitle(Pr(Hot)) n(10)

binscatter r_mean_temp gini_can, line(lfit) scheme(plotplainblind) xtitle("Mean Temp") ytitle("Resid. Log GPP") 
graph export "$Dtabs/bins_lgpp_temp_hot.pdf", replace 


binscatter gini_nat year, line(connect) // still weird

binscatter gini_can year if area_agri>50, line(connect) // still weird

binscatter gini_can year if area_agri>50, line(connect) // still weird


binscatter r_lmonth_gpp mean_temp if hot



binscatter month_gpp mean_temp if hot


sum mean_temp, d

sum gini_can,d
sum month_gpp, d

cap drop lgini_can
gen lgini_can=ln(gini_can)



reghdfe lmonth_gpp hot if year>2009 & area_agri>50, absorb(canton t) vce(cl canton)

did_multiplegt lmonth_gpp canton t hot if year>2009 & q_5, breps(20) cluster(canton)

did_multiplegt lmonth_gpp canton t hot if year>2009 & q_5, breps(20) placebo(4) cluster(canton)



binscatter lmonth_gpp year, line(connect) scheme(plotplainblind) xtitle("Temperature") ytitle("Resid. Log GPP") n(10)

*********************
* Inequality and GPP*
*********************
cap drop st_gini_can
sum gini_can
gen st_gini_can=gini_can/r(sd)
reg lmonth_gpp st_gini_can if area_agri>80,  vce(cl canton)
reghdfe lmonth_gpp st_gini_can if area_agri>50, absorb(canton t)  vce(cl canton)


reg herfindahl gini_can ,  vce(cl canton)
reghdfe herfindahl gini_can , absorb(canton t)  vce(cl canton)


reg avg_farm_size gini_can ,  vce(cl canton)
reghdfe avg_farm_size gini_can if area_agri>.50, absorb(canton t)  vce(cl canton)


reg herfindahl gini_can ,  vce(cl canton)
reghdfe herfindahl gini_can , absorb(canton t)  vce(cl canton)
reghdfe herfindahl gini_can if area_agri>.50, absorb(canton t)  vce(cl canton)


ereturn list
    
cap drop quant_gini
cap drop q_*
xtile quant_gini= gini_can, nq(5) 
tab quant_gini,gen(q_)
tab1 q_*

* Effect by quintile
capture drop t_q_*
foreach var of var q_* {

gen t_`var'=hot*`var'
}

ta year


bysor quant_gini: sum area_agri ,d

bysor quant_gini: sum  herfindahl, d
* All
eststo hot: reghdfe lmonth_gpp t_q_* q_* if area_agri>.50, absorb(canton i.t) vce(cl canton)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .0001)


#delimit
coefplot hot, keep(t_q_*)  yline(0, lcolor(gs0)) vertical mcolor(gs5) offset(0)
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on log GPP")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr

xtset canton t
xtreg gini_can, fe 
xtsum gini_can


global for "3"
global lag "3"
global xlabel
loc i=1
forval q=1/5 {
eststo paraq`q': reghdfe lmonth_gpp f(${for}/0).hot l(1/${lag}).hot if q_`q'==1 & area_agri>.20, absorb(canton i.year##i.month) vce(cl canton)
}

loc end= ${lag}+ ${for}+1
mylabels -${for}(1)${lag}, myscale(@+${for}+1) local(labels) prefix(t)
#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* hot L*) 
xlabel(`labels') yline(0, lcolor(gs0)) xline(${for}) vertical recast(line) offset(0)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on log GPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") cols(1) ring(0) position(6)  ) 
scheme(plotplainblind);
#delimit cr


global for "3"
global lag "3"
global xlabel
loc i=1
forval q=1/5 {
eststo paraq`q': reghdfe lmonth_gpp f(${for}/0).hot l(1/${lag}).hot if q_`q'==1 & herfindahl>.2xc erv, absorb(canton i.year##i.month) vce(cl canton)
}

loc end= ${lag}+ ${for}+1
mylabels -${for}(1)${lag}, myscale(@+${for}+1) local(labels) prefix(t)
#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* hot L*) 
xlabel(`labels') yline(0, lcolor(gs0)) xline(${for}) vertical recast(line) offset(0)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on log GPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") cols(1) ring(0) position(6)  ) 
scheme(plotplainblind);
#delimit cr



* For farms
eststo hot: reghdfe lmonth_gpp_farms t_q_1 t_q_2 t_q_3 t_q_4 t_q_5 q_1 q_2 q_3 q_4 , absorb(canton i.year##i.month) vce(cl canton)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .0001)


#delimit
coefplot hot, keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5)  yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) offset(0)
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on log GPP")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr

cap drop time
so canton year month
byso canton: gen time=_n
gen date=mdy(month,1,year)
format date %td


xtset canton time
preserve
keep if area_agri>60
eststo paraq5: reghdfe lmonth_gpp f(6/0).hot l(1/6).hot if q_5==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq4:reghdfe lmonth_gpp f(6/0).hot l(1/6).hot if q_4==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq3:reghdfe lmonth_gpp f(6/0).hot l(1/6).hot if q_3==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq2:reghdfe lmonth_gpp f(6/0).hot l(1/6).hot if q_2==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq1:reghdfe lmonth_gpp f(6/0).hot l(1/6).hot if q_1==1, absorb(canton i.year##i.month) vce(cl canton)

#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* hot L*) 
xlabel(1 "t-6" 2 "t-5" 3 "t-4" 4 "t-3" 5 "t-2" 6 "t-1" 7 "t" 8 "t+1" 9 "t+2" 10 "t+3" 11 "t+4" 12 "t+5" 13"t+6"   ) yline(0, lcolor(gs0)) xline(7) vertical recast(line) offset(0)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on log GPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") ) 
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
#delimit cr
restore

* Farms
eststo paraq5: reghdfe lmonth_gpp_farms f(6/0).hot l(1/6).hot if q_5==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq4:reghdfe lmonth_gpp_farms f(6/0).hot l(1/6).hot if q_4==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq3:reghdfe lmonth_gpp_farms f(6/0).hot l(1/6).hot if q_3==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq2:reghdfe lmonth_gpp_farms f(6/0).hot l(1/6).hot if q_2==1, absorb(canton i.year##i.month) vce(cl canton)
eststo paraq1:reghdfe lmonth_gpp_farms f(6/0).hot l(1/6).hot if q_1==1, absorb(canton i.year##i.month) vce(cl canton)

#delimit
coefplot paraq5 paraq4 paraq3 paraq2 paraq1, keep(F* hot L*) 
xlabel(1 "t-6" 2 "t-5" 3 "t-4" 4 "t-3" 5 "t-2" 6 "t-1" 7 "t" 8 "t+1" 9 "t+2" 10 "t+3" 11 "t+4" 12 "t+5" 13"t+6"   ) yline(0, lcolor(gs0)) xline(7) vertical recast(line) offset(0)
ciopts(/*recast(rline) */lpattern(solid)) ytitle("Impact on log GPP") legend(row(1) order(2 "q5" 4 "q4" 6 "q3" 8 "q2" 10 "q1") ) 
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
#delimit cr

encode ecobiome, g(n_ecob)
tab n_ecob, g(dn_ecob)

gen P_A=avg_hold_peri/avg_farm_size 

reghdfe P_A gini_can, absorb(canton i.t) vce(cl canton)
lab var P_A "Perimeter-Area ratio"

global xvars "mean_temp_farms mean_temp month_gpp_farms month_gpp herfindahl n_holdings P_A avg_farm_size avg_hold_peri area_agri area_1_pct-area_99_pct area_10_pct dn_ecob*"
eststo clear
estpost sum ${xvars} if q_5==1
matrix mean_1 = e(mean)
matrix obs_1 = e(count)

estpost sum ${xvars}  if q_5==0
matrix mean_0 = e(mean)
matrix obs_0 = e(count)

estpost ttest ${xvars}, by(q_5) unequal
estadd matrix mean_0
estadd matrix mean_1
estadd matrix obs_0
estadd matrix obs_1

esttab , replace noobs ///
cells("mean_1(fmt(2)) mean_0(fmt(2)) b(star fmt(2)) obs_1(fmt(0)) obs_0(fmt(0))") ///
 label nonumbers ///
collabels("Mean q5" "Mean q1-q4" "Diff." "q5 Obs." "q1-q4 Obs.") 


esttab using "$Dtabs/mean_diff_q5.tex", replace noobs ///
cells("mean_0(fmt(2)) mean_1(fmt(2)) b(star fmt(2)) obs_1(fmt(0)) obs_0(fmt(0))") ///
 label nonumbers ///
collabels("Mean q5" "Mean q1-q4" "Diff." "q5 Obs." "q1-q4 Obs.") 




bysor canton year: egen yr_gpp=total(month_gpp)
gen lyr_gpp=ln(yr_gpp)

cap drop yr_hot
bysor canton year: egen yr_hot=total(hot)
ta yr_hot

eststo hot: reghdfe lyr_gpp t_q_1 t_q_2 t_q_3 t_q_4 t_q_5 q_1 q_2 q_3 q_4 , absorb(canton i.year##i.month) vce(cl canton)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .0001)

#delimit
coefplot hot, keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5)  yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) offset(0)
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on log GPP")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr



#delimit
coefplot hot, keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5)  yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) offset(0)
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on log GPP")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
#delimit cr

eststo hot: reghdfe lmonth_gpp hot##c.herfindahl if q_5==1, absorb(canton i.year##i.month) vce(cl canton)

reghdfe lmonth_gpp hot if q_5==1, absorb(canton i.year##i.month) vce(cl canton)
reghdfe lmonth_gpp hot##c.herfindahl if q_5==1, absorb(canton i.year##i.month) vce(cl canton)

reghdfe lmonth_gpp hot if q_5!=1 , absorb(canton i.year##i.month) vce(cl canton)
reghdfe lmonth_gpp hot##c.herfindahl if q_5!=1, absorb(canton i.year##i.month) vce(cl canton)

reghdfe lmonth_gpp hot##c.herfindahl , absorb(canton i.year##i.month) vce(cl canton)


test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .0001)

reghdfe lmonth_gpp gini_can, absorb(time canton) vce(cl canton)
reghdfe lmonth_gpp gini_can if area_agri>20, absorb(time canton) vce(cl canton)
reghdfe lmonth_gpp gini_can if area_agri>20, absorb(year canton) vce(cl canton)

reghdfe month_gpp gini_can if area_agri>50, absorb(year canton) vce(cl canton)

tab1 year if e(sample)

cap drop qa_*
tab qarea,gen(qa_)
tab1 qa_*

binscatter mean_temp year
binscatter gini_nat year
binscatter hot year
binscatter hot year if year>2010




count if gini_can==. & year==2014
count if gini_can==. & year==2015
count if gini_can==. & year==2016


/*

* Effect by quintile
foreach var of var q_1-q_10 {
capture drop t_`var'
gen t_`var'=hot*`var'
}

eststo hot: areg month_gpp t_q_1-t_q_10 q_1-q_10 i.year i.month, absorb(canton) vce(cl canton)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5=t_q_6=t_q_7=t_q_8=t_q_9=t_q_10
test t_q_8=t_q_10

cap drop time
so canton year month
byso canton: gen time=_n
gen date=mdy(month,1,year)
format date %td


 did_multiplegt month_gpp canton time t_q_10, placebo(3) breps(2)  cluster(canton)


*/


cap drop time
so canton year month
byso canton: gen time=_n
gen date=mdy(month,1,year)
format date %td

byso canton: gen 

binscatter gini_can date, line(connect) xlabel(, format(%td)) discrete
binscatter month_gpp date, by(gini_q) line(connect) xlabel(, format(%td)) 


cap drop gini_q
sum gini_can,d
gen gini_q=(gini_can>r(p50))
reghdfe month_gpp hot##c.gini_can,  absorb(canton year month) vce(cl canton)

reg month_gpp hot##gini_q, vce(cl canton)
reghdfe month_gpp hot##gini_q,  absorb(canton i.year##i.month) vce(cl canton)

* Dirty controls
reghdfe month_gpp hot##gini_q area_1_pct-area_99_pct area_unexpl_pct,  absorb(canton i.year##i.month) vce(cl canton)


cap drop month_gpp_r
reghdfe month_gpp, absorb(canton  i.year##i.month) resi(month_gpp_r) 


binscatter month_gpp_r mean_temp, by(gini_q) line(connect)
binscatter month_gpp_r time, by(gini_q) line(connect)

binscatter month_gpp_r mean_temp if herfindahl<.22, by(gini_q) line(connect)
binscatter month_gpp_r mean_temp if herfindahl>.22, by(gini_q) line(connect)


binscatter month_gpp_r mean_temp if mean_temp>30, by(gini_q) line(connect)

.2185993 

 did_multiplegt month_gpp canton time gini_q if hot==1, placebo(3) breps(2)  cluster(canton)


areg mean_ion_npp `xvar',  absorb(canton) vce(cl canton)


#delimit
binscatter2 gini_can area_agri, line(connect) 
ytitle("Gini") xtitle("% agricultural land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
*graph export "$Dtabs/CEF_gini_area.pdf", replace ;
#delimit cr

*partialing out canton and time effects
#delimit
binscatter2 gini_can area_agri, absorb(year canton) altcontrols line(connect) 
ytitle("Gini") xtitle("% agricultural land") lcolors(red)
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/CEF_gini_area_cond.pdf", replace ;
#delimit cr

* Agri and NPP
#delimit
binscatter2 mean_ion_npp area_agri,  line(connect) 
ytitle("NPP") xtitle("% agricultural land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/CEF_NPP_area.pdf", replace ;
#delimit cr

#delimit
binscatter2 mean_ion_npp area_agri, absorb(year canton)  altcontrols line(connect) 
ytitle("NPP") xtitle("% agricultural land") lcolors(red)
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/CEF_NPP_area_cond.pdf", replace ;
#delimit cr


*Is inequality associated with types of agriculture?
areg gini_can i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton)
coefplot, keep(area*)  horizontal xline(0) sort xtitle("Correlation with Gini") ///
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white))
graph export "$Dtabs/Corr_crop_gini.pdf", replace


sum area_agri,d
* Problems with this calculation: some cantons have >100%. Top code for now.
replace area_agri=r(p99) if area_agri>r(p99)
kdensity area_agri
cap drop qarea
* Generating quintiles of agri area in 2010 for heterogeneity analysis
cap drop area2010
gen area2010=area_agri if year==2010
cap drop ref_area
byso canton: egen ref_area=max(area2010)
sum ref_area,d
global quants=5
xtile qarea=ref_area, nq($quants)

cap drop q_*
tab qarea,gen(q_)
tab1 q_*

xtset canton year
* Why is it unbalanced?

* Table of aggregate effect
foreach xvar in gini_can {
eststo A0: reg mean_ion_npp `xvar',   vce(cl canton)

qui xtreg mean_ion_npp `xvar', fe vce(cl canton)
ereturn list
loc within_r2=e(r2_w)

eststo A: areg mean_ion_npp `xvar',  absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg mean_ion_npp `xvar' i.year, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo B: areg mean_ion_npp `xvar' i.year, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg mean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo C: areg mean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

loc stats "stats(ar2 within_r2, label("\\ Adj R2." "Within canton R2" ) fmt(%9.2f %9.2f))"

#delimit
esttab A0 A B C using "$Dtabs/all_`xvar'.tex",  f nonum nomti noli 
collabels(none) keep(`xvar') `stats' style(tex) noobs cells(b(fmt(4) star) 
se(par fmt(4))) star(* .1 ** .05 *** .01)  replace;
#delimit cr
}




* Heterogeneity by agricultural land
cap drop dbagri_p50
sum area_agri,d
gen dbagri_p50=area_agri>r(p50)
*kdensity area_agri

foreach xvar in gini_can {

* Effect by quintile
foreach var in q_1 q_2 q_3 q_4 q_5 {
capture drop t_`var'
gen t_`var'=`xvar'*`var'
sum area_agri if `var'==1,d
global m`var'=round(r(mean), 1)
}

eststo `xvar'q: areg mean_ion_npp t_q_1 t_q_2 t_q_3 t_q_4 t_q_5 i.year, absorb(canton) vce(cl canton)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .01)


loc stats "stats(pval_id ar2, label("\\ p-val same effect" "Adj. R2" ) fmt(%9.2f %9.2f))"

#delimit
esttab `xvar'q using "$Dtabs/het_agri_impact_`xvar'.tex",  f nonum nomti noli 
collabels(none) keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5) varlabels(t_q_1 "1st quintile" t_q_2 "2nd quintile" t_q_3 "3rd quintile" t_q_4 "4th quintile" t_q_5 "5th quintile")
 `stats' style(tex) noobs cells(b(fmt(3) star) 
se(par fmt(3))) star(* .1 ** .05 *** .01)  replace;
#delimit cr


#delimit
coefplot `xvar'q, keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5) xlabel(1 "q1=$mq_1" 2 "q1=$mq_2" 
3 "q3=$mq_3"
4 "q4=$mq_4" 5 "q5=$mq_5") yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) 
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on mean NPP") xtitle("Proportion of agricultural land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
graph export "$Dtabs/npp_`xvar'_het_agriland.pdf", replace ;
#delimit cr
}


* Main effect excluding bottom quintile agri %
foreach xvar in gini_can {
eststo A0: reg mean_ion_npp `xvar',   vce(cl canton)

qui xtreg mean_ion_npp `xvar' if qarea>1, fe vce(cl canton)
ereturn list
loc within_r2=e(r2_w)

eststo A: areg mean_ion_npp `xvar' if qarea>1,  absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg mean_ion_npp `xvar' i.year if qarea>1, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo B: areg mean_ion_npp `xvar' i.year if qarea>1, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg mean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo C: areg mean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

loc stats "stats(ar2 within_r2, label("\\ Adj R2." "Within canton R2" ) fmt(%9.2f %9.2f))"

#delimit
esttab A0 A B C using "$Dtabs/all_`xvar'_exclq1.tex",  f nonum nomti noli 
collabels(none) keep(`xvar') `stats' style(tex) noobs cells(b(fmt(4) star) 
se(par fmt(4))) star(* .1 ** .05 *** .01)  replace;
#delimit cr
}

binscatter2 mean_ion_npp gini_can if qarea>1, absorb(year canton)  altcontrols ///
ytitle("NPP") xtitle("Gini") lcolors(red) ///
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white))
graph export "$Dtabs/CEF_NPP_gini_cond_exclq1.pdf", replace 


* Trying to generate a binary treatment that indicates a large change in Gini
* Then test parallel trends
cap drop within_gini_sd
byso canton: egen within_gini_sd=sd(gini_can)
so canton year
cap drop treatment
cap drop gini2010
cap drop ref_gini
gen gini2010=gini_can if year==2010
byso canton: egen ref_gini=max(gini2010)
sum within_gini_sd,d
* Set effect size here:
gen treatment=(gini_can>ref_gini+1.5*r(mean))
*gen treatment=(gini_can>ref_gini+r(p75))
*byso canton: replace treatment=1 if treatment[_n-1]==1
ta treatment

* Generating leads and lag as parallel trends: 
sort canton year
forval x=1/4 {
cap drop treatlag`x'
by canton: gen treatlag`x' = treatment[_n-`x'] if year==year[_n-`x']+`x'
cap drop treatlead`x'
by canton: gen treatlead`x' = treatment[_n+`x'] if year==year[_n+`x']-`x'
}

br canton year treat*

reghdfe mean_ion_npp treatlead3 treatlead2 treatlead1  treatment treatlag1 treatlag2 i.year if qarea>1, absorb(canton) vce(cl canton)

#delimit
coefplot, keep(treatlead*  treatment treatlag*) xlabel(1 "t-3" 2 "t-2" 
3 "t-1"
4 "t" 5 "t+1" 6 "t+2") yline(0, lcolor(gs0)) xline(3.5) vertical mcolor(gs5) 
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on mean NPP") xtitle("Leads and lags of treatment")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/npp_gini_trend.pdf", replace ;
#delimit cr

/*
ssc install did_multiplegt
ssc install twowayfeweights
*/
* Consistent with Fuzzy difference in differences à la de Chaisemartin et D’Haultfœuille?
 did_mùmkhklpmù:;,,:








































tiplegt mean_ion_npp canton year treatment if qarea>1, placebo(3) breps(10) cluster(canton) dynamic(2) 
ereturn list


* and heterogeneous treatment effects à la de Chaisemartin et D’Haultfœuille?
twowayfeweights mean_ion_npp canton year treatment if qarea>1,  type(feTR) brepscluster(canton) breps(20)


* Still sensitive to changes in definition of treatment 


* Monoculture
xtreg mean_ion_npp c.gini_can##c.herfin_norm i.year if qarea>1, fe vce(cl canton)

* Monoculture
xtreg mean_ion_npp c.gini_can##c.herfin_norm i.year if qarea>1, fe vce(cl canton)

replace status="0" if status==""
destring status, replace force
xtreg mean_ion_npp c.gini_can##i.status i.year if qarea>1, fe vce(cl canton)

binscatter2 mean_ion_npp herfin_norm if qarea>1, absorb(year canton) altcontrols line(connect) 
binscatter2 gini_can herfin_norm if qarea>1, absorb(year canton) altcontrols line(connect) 





*Looking at ratio of Perimeter to Area 
cap drop PtoA
gen PtoA= (avg_hold_peri) /avg_farm_size*10000
cap drop lPtoA
gen lPtoA=ln(PtoA)
cap drop lmean_ion_npp
gen lmean_ion_npp=ln(mean_ion_npp) 

kdensity lPtoA

* Main effect excluding bottom quintile agri %
foreach xvar in lPtoA {
eststo A0: reg lmean_ion_npp `xvar',   vce(cl canton)

qui xtreg lmean_ion_npp `xvar' if qarea>1, fe vce(cl canton)
loc within_r2=e(r2_w)

eststo A: areg lmean_ion_npp `xvar' if qarea>1,  absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg lmean_ion_npp `xvar' i.year if qarea>1, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo B: areg lmean_ion_npp `xvar' i.year if qarea>1, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

qui xtreg lmean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, fe vce(cl canton)
loc within_r2=e(r2_w)
eststo C: areg  lmean_ion_npp `xvar' i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton)
estadd sca within_r2=`within_r2'

}


cap drop lgini_can
gen lgini_can=ln(gini_can)


#delimit
binscatter2 lmean_ion_npp lPtoA if qarea>1, absorb(year canton) altcontrols line(connect) 
ytitle("Log NPP") xtitle("Log(P/A)") lcolors(red)
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/CEF_lNPP_lPA_cond.pdf", replace ;
#delimit cr

#delimit
binscatter2 lgini_can lPtoA if qarea>1, absorb(year canton) altcontrols line(connect) 
ytitle("Log Gini") xtitle("Log(P/A)") lcolors(red)
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white));
graph export "$Dtabs/CEF_lGINI_lPA_cond.pdf", replace ;
#delimit cr


xtreg lgini_can lPtoA i.year if qarea>1, fe vce(cl canton)


binscatter2 gini_can PtoA if qarea>1, absorb(year canton) altcontrols line(connect) 










***********************************************
***** Old stuff mostly using endogenous agri land *
***********************************************

* Heterogeneous effect by agricultural area
xtreg mean_ion_npp c.gini_can##i.qarea i.year, fe vce(cl canton)
margins, dydx(gini_can) at(qarea=(1(1)5)) post
mat list e(b)
marginsplot

sum gini_can, d
cap drop temp
gen temp=gini_can/100
		*kdensity gini if  area_agri
		cap drop bingini
				/// Bins at intervals of 2 percentile points
				loc start=.5
				loc end=1-`start'
		gen bingini=1 if temp<=`start'
		local plus=2
		forval x=0(.2)`end' {

		dis `x'
		replace bingini=`plus' if temp>`start'+`x'
		local ++plus
		}
		ta bingini
		
		byso bingini: sum gini_can

cap drop can_w
byso bingini: egen can_w=count(canton)
ta can_w

pause on
forval q=0/$quants {
*forval q=0/0 {
if `q'==0{
*reg mean_ion_npp area_agri, vce(cl canton)
qui reghdfe mean_ion_npp area_agri i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton) resid
}
else {
*reg mean_ion_npp  if qarea==`q', vce(cl canton)
qui reghdfe mean_ion_npp  i.year area_1_pct- area_99_pct area_unexpl_pct if qarea==`q', absorb(canton)  vce(cl canton) resid
}
cap drop resid
predict resid, r


preserve
collapse (mean) resid gini_can [aw=can_w], by(bingini)
graph tw (scatter resid gini_can, sort) (connect resid gini_can, sort)
pause
restore

}


binscatter mean_ion_npp area_agri, by(treatment) line(connect)

replace status="0" if status==""
binscatter mean_ion_npp year, by(status) line(connect)
binscatter mean_ion_npp year, by(status) line(connect)

*Is inequality associated with mono agriculture
foreach var of var area_1_pct- area_99_pct area_unexpl_pct {
sum `var',d
replace `var'=`var'*100
}

areg gini_can i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton)

coefplot, keep(area*)  horizontal xline(0) sort

#delimit
coefplot, keep(area*)  yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) xlabel(, angle(90)) vertical
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Correlation with Gini") xtitle("Type of land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) /*note("p-val Same effect by quintile: `=p_quintiles'")*/;
*graph export "figures/descriptives//npp_`xvar'_het_agriland.pdf", replace ;
#delimit cr


reg gini_can area_1_pct- area_99_pct area_unexpl_pct

br canton year gini_can treatment
reghdfe mean_ion_npp treatment i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton) resid

xtset canton year
xtreg mean_ion_npp gini_can, fe vce(cl canton)
xtreg mean_ion_npp gini_can i.year, fe vce(cl canton)
xtreg mean_ion_npp gini_can i.year area_1_pct- area_99_pct area_unexpl_pct, fe vce(cl canton)

xtreg mean_ion_npp c.gini_can##i.qarea, fe vce(cl canton)
xtreg mean_ion_npp c.gini_can##i.qarea i.year, fe vce(cl canton)
xtreg mean_ion_npp c.gini_can##i.qarea i.year area_1_pct- area_99_pct area_unexpl_pct, fe vce(cl canton)
areg mean_ion_npp c.gini_can##i.qarea, absorb(canton) vce(cl canton)
areg mean_ion_npp c.gini_can##i.qarea i.year , absorb(canton) vce(cl canton)

areg mean_ion_npp c.gini_can##i.qarea i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton)
kdensity area_agri
areg mean_ion_npp c.gini_can i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(canton) vce(cl canton)

areg mean_ion_npp c.gini_can i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton)

areg mean_ion_npp c.gini_can i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton)

areg mean_ion_npp i.year if qarea>1, absorb(canton) vce(cl canton)
cap drop y_dt
predict y_dt, resid
xtreg y_dt gini_can if qarea>1, fe vce(cl canton)


reghdfe mean_ion_npp c.gini_can##i.qarea area_1_pct- area_99_pct area_unexpl_pct, absorb(i.year##i.dep i.canton) vce(cl canton) resid

areg mean_ion_npp i.year, absorb(canton) vce(cl canton)
cap drop y_dt
predict y_dt, resid
binscatter y_dt gini_can if ref_gini>20, by(qarea) line(lfit)
binscatter y_dt gini_can if ref_gini>20 & qarea==4, line(lfit)


areg mean_ion_npp i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton)
cap drop y_dt
predict y_dt, resid
binscatter y_dt year  if qarea>1, by(treatment) line(connect) discrete


test 
margins, dydx(gini_can) at(qarea=(1(1)5)) post
mat list e(b)
marginsplot

#delimit
coefplot,  xlabel(1 "q1" 2 "q2" 
3 "q3"
4 "q4" 5 "q5") yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) 
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on mean NPP") xtitle("Quantile of agricultural land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) /*note("p-val Same effect by quintile: `=p_quintiles'")*/;
*graph export "figures/descriptives//npp_`xvar'_het_agriland.pdf", replace ;
#delimit cr


sum area_agri
xtreg mean_ion_npp c.gini_can##(c.area_agri) i.year area_1_pct- area_99_pct area_unexpl_pct, fe vce(cl canton)
margins, dydx(gini_can) at(area_agri=(0(10)100))
marginsplot

reghdfe mean_ion_npp l(0/3).treatment f(1/3).treatment area_1_pct- area_99_pct area_unexpl_pct i.year, absorb(canton) vce(cl canton) resid



reghdfe mean_ion_npp treatment##i.qarea i.year, absorb(canton) vce(cl canton) resid
lincom 1.treatment + 1.treatment#2.qarea
lincom 1.treatment + 1.treatment#3.qarea
lincom 1.treatment + 1.treatment#4.qarea
lincom 1.treatment + 1.treatment#5.qarea


kdensity mean_ion_npp
kdensity gini_can

reghdfe mean_ion_npp i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton) resid

reghdfe mean_ion_npp gini_can i.year area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(canton) vce(cl canton) resid
encode name_2, g(dep)
encode name_1, g(reg)
reghdfe mean_ion_npp gini_can area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(i.year##i.dep canton) vce(cl canton) resid
reghdfe mean_ion_npp gini_can if qarea>1, absorb(i.year##i.dep canton) vce(cl canton) resid

reghdfe mean_ion_npp gini_can area_1_pct- area_99_pct area_unexpl_pct if qarea>1, absorb(i.year##i.reg canton) vce(cl canton) resid
reghdfe mean_ion_npp gini_can if qarea>1, absorb(i.year##i.reg canton) vce(cl canton) resid


areg mean_ion_npp (c.gini_can##i.qarea) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)
reghdfe mean_ion_npp (c.gini_can##i.qarea) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)

reghdfe mean_ion_npp, absorb(canton) vce(cl canton) resid
cap drop resid
predict resid, r

binsreg resid gini_can if qarea>1, vce(cl canton)  

twoway (scatter resid gini_can if qarea>1) || (lfit resid gini_can if qarea>1)
binscatter resid gini_can if qarea>1

reg mean_ion_npp gini_can area_agri, vce(cl canton)

areg mean_ion_npp gini_can i.year, absorb(adm_can) vce(cl adm_can)

areg mean_ion_npp (c.gini_can##i.qarea) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct  if area_agri>20 , absorb(adm_can) vce(cl adm_can)

areg mean_ion_npp (c.gini_can) i.year if area_agri>20 , absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct  if area_agri>20 , absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) (i.year##c.(area_1_pct- area_99_pct area_unexpl_pct))  if area_agri>20 , absorb(adm_can) vce(cl adm_can)


		*kdensity gini if  area_agri
		cap drop bingini
				/// Bins at intervals of 2 percentile points
		gen bingini=1 if gini<=.80
		local plus=2
		forval x=0(.02).2 {
		replace bingini=`plus' if gini>.80+`x' & gini<=.82+`x'
		local ++plus
		}
		ta bingini
		
**************************************
*Lasso to select pertinent covariates
********************************
* Potential covariates
global cov  tot_area_ilot n_holdings area_agri avg_plot_ha protected ecobiome_*

rlasso npp_var_2015_2000 $cov if area_agri >= 50, robust maxupsiter(100) 
global x_sel `e(selected)'

reg npp_var_2015_2000 gini if area_agri >= 50, r
reg npp_var_2015_2000 gini $x_sel if area_agri >= 50, r

********************************
* Creating binscatters by hand *
********************************

* No controls
reg npp_var_2015_2000 gini if area_agri >= 50, r
cap drop resid
predict resid, r

preserve
keep if area_agri >= 50
collapse (mean) resid gini, by(bingini)
graph tw (scatter resid gini) (connect resid gini)
restore

* With controls
reg npp_var_2015_2000 gini $x_sel if area_agri >= 50, r
cap drop resid
predict resid, r

preserve
keep if area_agri >= 50
collapse (mean) resid gini, by(bingini)
graph tw (scatter resid gini) (connect resid gini)
restore

* With a quadratic fit?
cap drop gini2
gen gini2=gini^2

reg npp_var_2015_2000 gini gini2 if area_agri >= 50, r
reg npp_var_2015_2000 gini gini2 $x_sel if area_agri >= 50, r
test gini gini2

* Creating binscatter "by hand"
reg npp_var_2015_2000 $x_sel if area_agri >= 50, r
cap drop resid
predict resid, r

keep if area_agri >= 50
collapse (mean) resid gini, by(bingini)
graph tw (scatter resid gini) (connect resid gini)
restore

***************************************************************************************************
***************************************************************************************************
***************************************************************************************************
***************************************************************************************************

***************************
* Agricultural census data *
****************************
/*
* Cleaning and mergin census years

*https://agreste.agriculture.gouv.fr/agreste-web/disaron/G_2007/detail/
foreach base in 1988 2000 2010 {
import delimited using FDS_G_2007_`base'.txt, delim(";") varn(1) clear
keep if frdom=="METRO" & com!="............"
keep if g_2007_lib_dim2=="Ensemble des exploitations"

drop france frdom g_2007_dim1 g_2007_dim2 g_2007_lib_dim2 g_2007_dim3

tostring g_2007_mod_dim1, replace
tostring g_2007_mod_dim3, replace
cap drop size
gen size_type=g_2007_mod_dim1+g_2007_mod_dim3
ta size_type

drop g_2007_mod_dim1 g_2007_lib_dim1 g_2007_mod_dim2 g_2007_mod_dim3 g_2007_lib_dim3 qualite

reshape wide valeur, i(com) j(size_type) s
sa temp`base', replace
}
*/

************************************************
* Looking at evolution of agri land inequality *
************************************************
u temp1988, clear
append using temp2000
append using temp2010

* Converting commune level agricultural hectars into km square
gen areakm2=valeur12*.01

* Number of farms per commune
gen farms=valeur11

* Area per farm ratio
cap drop r_area_farm
gen r_area_farm=areakm2/farms
sum r_area_farm

replace areakm2=log(areakm2)
replace farms=log(farms)

*graph bar (mean) r_area_farm, over(annref)

* Illustration of the trends
eststo y1988a: mean areakm2 if annref==1988
eststo y2000a: mean areakm2 if annref==2000
eststo y2010a: mean areakm2 if annref==2010

eststo y1988b: mean farms if annref==1988
eststo y2000b: mean farms if annref==2000
eststo y2010b: mean farms if annref==2010


coefplot (y1988b y1988a , citop color(gs1) ciopts(recast(rcap)lcolor(gs1))) ///
(y2000b y2000a , citop color(gs6) ciopts(recast(rcap)lcolor(gs1))) ///
(y2010b y2010a , citop color(gs10) ciopts(recast(rcap)lcolor(gs1))), ///
vertical xlabel(1 "log(Farms)" 2 "log(Area km2)") ytitle(Log points)  ///
legend(row(1) order(1 "1988" 3 "2000" 5 "2010") ) ///
recast(bar)  barwidth(0.2) fcolor(*.5) ciopts(recast(rcap)) graphregion(fcolor(white))
quietly graph export ///
				"figures/descriptives//farms_and_area.pdf", ///
				replace 

* Illustration of rise in inequality
eststo y1988: mean r_area_farm if annref==1988
eststo y2000: mean r_area_farm if annref==2000
eststo y2010: mean r_area_farm if annref==2010

coefplot (y1988 , citop color(gs1) ciopts(recast(rcap)lcolor(gs1))) ///
(y2000, citop color(gs6) ciopts(recast(rcap)lcolor(gs1))) ///
(y2010, citop color(gs10) ciopts(recast(rcap)lcolor(gs1))), ///
vertical xlabel(1 "Area/Farms") ytitle("Area (km2 per farm")  ///
legend(row(1) order(1 "1988" 3 "2000" 5 "2010") ) ///
recast(bar)  barwidth(0.2) fcolor(*.5) ciopts(recast(rcap)) graphregion(fcolor(white))
quietly graph export ///
				"figures/descriptives//area_per_farm.pdf", ///
				replace 
/*
* Other way to do it rather than mean
reg  r_area_farm i.annref
eststo y1988: lincomest _cons
reg  r_area_farm i.annref
eststo y2000: lincomest _cons+2000.annref
reg  r_area_farm i.annref
eststo y2010: lincomest _cons+2010.annref
*/

************************************************************************************
* Departement or Region level changes in inequality: Can we find a control group? **
************************************************************************************

preserve
*loc geo dep
loc geo region
collapse (mean) r_area_farm (sd) sdr_area_farm=r_area_farm (count) n=r_area_farm [w=_n], by(`geo' annref)

/*
encode `geo', gen(depn)
reg r_area_farm i.depn##i.annref
predict y_variation, xb
levelsof depn, l(dep)
foreach d in `dep'{
replace y_variation=y_variation-_b[_cons]-_b[2000.annref]- _b[2010.annref] - _b[`d'.depn] if depn==`d'
}
byso `geo': egen t_y_variation=total(y_variation)
so t_y_variation
list 
xtline t_y_variation if annref>1988 , i(`geo') t(annref)  overlay xlabel(2000 2010)
quietly graph export ///
				"figures/descriptives//var_area_per_farm_`geo'.pdf", ///
				replace 
*/

gen temp1=r_area_farm if annref==2000
gen temp2=r_area_farm if annref==2010
byso `geo': egen var2000=max(temp1)
byso `geo': egen var2010=max(temp2)
gen ineq_var=var2010-var2000
so ineq_var
list 

* Region: Occitanie has lowest with reasonable number of communes represented
* Dep 30: Gard has little change with reasonable number of communes
/*
Negative values
     +--------------------------------------------------------------------------------------------------+
     | annref   dep   r_area~m   sdr_ar~m     n      temp1      temp2    var2000    var2010    ineq_var |
     |--------------------------------------------------------------------------------------------------|
  1. |   1988    94   .1048848   .1565676    17          .          .   .1808388   .0706601   -.1101787 |
  2. |   2000    94   .1808388    .343504     6   .1808388          .   .1808388   .0706601   -.1101787 |
Val de mar  3. |   2010    94   .0706601   .0510564     3          .   .0706601   .1808388   .0706601   -.1101787 |
  4. |   2010    06   .4616385    .839537    99          .   .4616385   .5201596   .4616385   -.0585212 |
  5. |   2000    06   .5201596    .910025   111   .5201596          .   .5201596   .4616385   -.0585212 |
     |--------------------------------------------------------------------------------------------------|
 Alpes Mari 6. |   1988    06   .2134094   .4023833   136          .          .   .5201596   .4616385   -.0585212 |
  7. |   2010    83   .1933057   .3055116   131          .   .1933057   .2075218   .1933057   -.0142162 |
  8. |   1988    83   .1674497   .5982537   145          .          .   .2075218   .1933057   -.0142162 |
 VAR  9. |   2000    83   .2075218   .5472772   140   .2075218          .   .2075218   .1933057   -.0142162 |
*/
restore



* Simple test: Use CVdL as "treated" and then create a synthetic control Following Abadie et al. 2010
u fr_2015_ineq_merged_saveold_collapsed, clear

local n = 4 // defines adm level to take into account
local p = .8 //defines the top share (1 - p) % we use as ineq stat
local min_agri = 50 //minimum % of agricultural land area to be included


//Display how many areas we will use
quietly keep if ftile_`n' == `p'
count if area_agri >= `min_agri' & missing(status)
di as result round(r(N)/ _N * 100, 0.1) "% of administrative areas used"

//cosmetics
quietly format %9.0f area_* _20* 
local top = (1 - `p') * 100
quietly rename L_surf_`n'  top`top'
label var top`top' "Share of top `top'% in land distribution "
cap quietly gen avg_plot_ha = avg_plot_size / 10000
quietly label var avg_plot_ha "Average plot size in ha."


//regional npp progression WITH ADDITIONAL CONTROLS for weighting matrix in synth command
loc g 1
*loc g 2
tab ecobiome, g(ecob_)
	quietly collapse (mean) top20 gini area_adm_4 n_holdings ln_avg_plot_size area_agri ecob_*	_20*mean*  [w=area_adm_4] , by(name_`g')
	quietly rename _20*mean* mean_20*
	reshape long mean_, i(name_`g') j(year)

	//cosmetics  
	quietly rename mean_ mean_npp
	quietly replace mean_npp = mean_npp / 1000
	quietly label var mean_npp "Yearly average NPP (C.ton/m{sup:2})"
	/*
	//index base 100
	cap drop index
	sort name_1 year 
	bysort name_1: gen index = mean_npp / mean_npp[1] * 100 
	
	//rank 
	quietly gen last_value = . 
	quietly levelsof name_1, local(regnames) 
	foreach reg in `regnames' {
		quietly sum index if name_1 == "`reg'" & year == 2015
		quietly replace last_value = r(max) if name_1 == "`reg'"
	}
	sort last_value year 
	*/
	
	encode name_`g' , gen(g)
	order name_`g' g, first

tsset g year

* Region
if `g'==1 {
global treat=10
}

*Departement
if `g'==2 {
global treat=32
}


/*
net from "https://web.stanford.edu/~jhain/Synth"
net install synth, all replace force
*/


* Occitanie=10 as treated region with lowest change in land inequality from 2000-2010 according to Agri Censi
synth mean_npp mean_npp(2000(1)2005) /*area_adm_4(2000(1)2005)*/ /// 
ln_avg_plot_size(2000(1)2005) area_agri(2000(1)2005) ///
ecob_1 ecob_2 ecob_3 ecob_4 ecob_5 ecob_6 ecob_7 ecob_8 ecob_9 ///
,trunit($treat) trperiod(2005) figure
quietly graph export ///
				"figures/descriptives//synth_treated.pdf", ///
				replace 
ereturn list	
mat def y_synth=e(Y_synthetic)		
mat def y_treated=e(Y_treated)		
svmat y_synth, names(synth)
svmat y_treated, names(treat)
gen effect=treat1-synth1

twoway line effect year, lcolor(gs0) ///
legend(off) xline(2005, lpattern(dash)) yline(0, lpattern(dash)) ylabel(-1.5(.5)1.5)

ttest synth1=treat1

********************************************
**** Canton Panel exploration **********************
*********************************************

capture cd "~/Desktop/Temp/"
use "fr_panel.dta", clear

so name_4 year
ta year

/*
drop if name_4==""
duplicates tag name_4 year, g(d)
br if d>0
ta name_4 if d>0
drop if d>0
*/

encode gid_4 , gen(can)

xtset can year

gen temp=area_agri if year==2010
byso can: egen barea_agri=max(temp)

xi i.adm
xtile qarea=area_agri, nq(10)

areg mean_ion_npp gini_can i.year, absorb(adm_can) vce(cl adm_can)

areg mean_ion_npp (c.gini_can##i.qarea) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct, absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct  if area_agri>20 , absorb(adm_can) vce(cl adm_can)

areg mean_ion_npp (c.gini_can) i.year if area_agri>20 , absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) i.year area_1_pct- area_99_pct area_unexpl_pct  if area_agri>20 , absorb(adm_can) vce(cl adm_can)
areg mean_ion_npp (c.gini_can) (i.year##c.(area_1_pct- area_99_pct area_unexpl_pct))  if area_agri>20 , absorb(adm_can) vce(cl adm_can)


reg mean_ion_npp gini_can i.year if area_agri>20, vce(cl adm_can)


foreach xvar in gini_4 top20 {

eststo A: reg mean_npp `xvar',  vce(cl can)
sum `e(depvar)' if e(sample)
estadd sca mean_dep=r(mean)
sum `xvar' if e(sample)
estadd sca mean_indep=r(mean)
eststo B: reg mean_npp `xvar' i.year, vce(cl can)
eststo C: xtreg mean_npp `xvar' i.year, fe vce(cl can)
eststo D: reghdfe mean_npp `xvar', absorb(i.dep##i.year i.can) vce(cl can)


loc stats "stats(mean_dep mean_indep, label("\\ Mean Dep. Var." "Mean Indep. Var." ) fmt(%9.1f %9.1f))"

#delimit
esttab A B C D using "figures/descriptives//corr_`xvar'.tex",  f nonum nomti noli 
collabels(none) keep(`xvar') `stats' style(tex) noobs cells(b(fmt(4) star) 
se(par fmt(4))) star(* .1 ** .05 *** .01)  replace;
#delimit cr
}

* Heterogeneity by agricultural land
cap drop dbagri_p50
sum barea_agri,d
gen dbagri_p50=barea_agri>r(p50)

foreach xvar in gini_4 top20 {

* Effect by agri land median and quintile
capture drop `xvar'_dbagri_p50
gen `xvar'_dbagri_p50=`xvar'*dbagri_p50

capture drop `xvar'_no_dbagri_p50
gen `xvar'_no_dbagri_p50=`xvar'*(1-dbagri_p50)


* Effect by agri land quintile
cap drop quin
xtile quin = barea_agri, nq(5)
cap drop q_*
tab quin,gen(q_)
tab1 q_*

foreach var in q_1 q_2 q_3 q_4 q_5 {
capture drop t_`var'
gen t_`var'=`xvar'*`var'
sum barea_agri if `var'==1,d
global m`var'=round(r(mean), 1)
}

eststo `xvar': xtreg mean_npp `xvar'_no_dbagri_p50 `xvar'_dbagri_p50 i.year, fe vce(cl can)
test `xvar'_no_dbagri_p50=`xvar'_dbagri_p50
estadd sca pval_id=r(p)
eststo `xvar'q: xtreg mean_npp t_q_1 t_q_2 t_q_3 t_q_4 t_q_5 i.year, fe vce(cl can)
test t_q_1=t_q_2=t_q_3=t_q_4=t_q_5
sca p_quintiles=round(r(p), .01)

loc stats "stats(pval_id, label("\\ p-val same effect" ) fmt(%9.3f))"

#delimit
esttab `xvar' using "figures/descriptives//het_impact_`xvar'.tex",  f nonum nomti noli 
collabels(none) keep(`xvar'_no_dbagri_p50 `xvar'_dbagri_p50) varlabels(`xvar'_no_dbagri_p50  "Below median Agr. Land" `xvar'_dbagri_p50 "Above median Agr. Land") `stats' style(tex) noobs cells(b(fmt(3) star) 
se(par fmt(3))) star(* .1 ** .05 *** .01)  replace;
#delimit cr

#delimit
coefplot `xvar'q, keep(t_q_1 t_q_2 t_q_3 t_q_4 t_q_5) xlabel(1 "q1=$mq_1" 2 "q1=$mq_2" 
3 "q3=$mq_3"
4 "q4=$mq_4" 5 "q5=$mq_5") yline(0, lcolor(gs0)) xline(9) vertical mcolor(gs5) 
ciopts(/*recast(rline) */lpattern(dash) lcolor(gs0)) ytitle("Impact on mean NPP") xtitle("Proportion of agricultural land")
plotregion(style(none)) plotregion(style(none)) graphregion(fcolor(white)) note("p-val Same effect by quintile: `=p_quintiles'");
graph export "figures/descriptives//npp_`xvar'_het_agriland.pdf", replace ;
#delimit cr
}

* Robustness with departement level time trends
encode name_2, g(dep)
xtreg mean_npp gini_4 i.year, fe vce(cl can)
reghdfe mean_npp gini_4, absorb(i.dep##i.year i.can) vce(cl can)
xtreg mean_npp top20 i.year, fe vce(cl can)
reghdfe mean_npp top20, absorb(i.dep##i.year i.can) vce(cl can)

* By proportion of agriculture
foreach xvar in gini_4 top20 {
xtreg mean_npp `xvar'_no_dbagri_p50 `xvar'_dbagri_p50 i.year, fe vce(cl can)
test `xvar'_no_dbagri_p50=`xvar'_dbagri_p50
reghdfe mean_npp `xvar'_no_dbagri_p50 `xvar'_dbagri_p50, absorb(i.dep##i.year i.can) vce(cl can)
test `xvar'_no_dbagri_p50=`xvar'_dbagri_p50
}

* Non linear effects?
xtreg mean_npp c.gini_4##c.gini_4 i.year, fe vce(cl can)
test gini_4 c.gini_4#c.gini_4 
margins, dydx(gini_4) at(gini_4=(10(10)100))
marginsplot

* log-level
cap drop lmean_npp
gen lmean_npp=log(mean_npp)
xtreg lmean_npp gini_4 i.year, fe vce(cl can)


* Within Canton Variation
byso can: egen var_gini=sd(gini_4)
so var_gini

* Conditional binscatter 
qui reghdfe mean_npp i.year, absorb(can) resid
cap drop resid
predict resid, r

cap drop gini
gen gini=gini_4/100
cap drop bingini
loc start=0
loc end=1-`start'
				/// Bins at intervals of 2 percentile points
		gen bingini=1 if gini<=`start'
		local plus=2
		forval x=0(.02)`end' {
		replace bingini=`plus' if gini>`start'+`x' & gini<=(`start'+.02)+`x'
		local ++plus
		}
		ta bingini

preserve
collapse (mean) resid gini, by(bingini)
graph tw (scatter resid gini if gini<.3) (lfit resid gini if gini<.3)  || (scatter resid gini if gini>=.3) (lfit resid gini if gini>=.3) 
restore

* Event study?
so can year
cap drop d_gini
by can: gen d_gini=gini_4-gini_4[_n-1]
br can year gini_4 d_gini
gen treatment=d_gini>0 if d_gini!=.

xtreg mean_npp treatment i.year, fe vce(cl can)
