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
byso canton year: egen max_p=max(p)

ta p month

binscatter lgpp_${inter} p, line(connect)
global shock the_shock_s
	byso canton year: gen w = sum(${shock})
	qui replace w = 1 if w > 0

cap drop min_p
gen min_p=p if w==1
byso canton year: ereplace min_p=min(min_p)
replace min_p=99 if min_p==.

ta min_p
preserve
keep if inrange(p, 18, 24)
replace the_shock_s_y=min_p<23
collapse (mean) lgpp_${inter} the_shock_s_y quant_gini_can, by(canton year)
xtset canton year
did_multiplegt_dyn lgpp_${inter} canton year the_shock_s_y, by(quant_gini_can) placebo(2) effects(2)
eststo base: reghdfe cgpp_${inter} (F(2/1).the_shock_s_y the_shock_s_y L(1/1).the_shock_s_y)##i.quant_gini_can, a(canton year) vce(cl canton) 



reghdfe lgpp_${inter} the_shock_s_y##i.quant_gini_can if inrange(p, 18, 26), absorb(canton year) vce(cl canton)


byso the_shock_s: sum tsup_h_q 
preserve
keep canton ?gpp_${inter} gini_can the_shock* year t month date p the_shock_${inter}Xgini_can-t_q_dherf4  temp_w  temp_m crop*

gen shock30=temp_w>30
global shock the_shock_s
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
	qui keep year canton t ${shock} lgpp_${inter} dinf quant_* p month temp_w  temp_m month crop*
	cap drop w
	byso canton year: gen w = sum(${shock})
	qui replace w = 1 if w > 0

*global time_unit month
global time_unit p

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

ta year ${shock} 

cap drop can_year
egen can_year=group(canton year)


reghdfe lgpp_${inter} c.d22#c.(f18-f26)##i.quant_gini_can if inrange(p, 18, 26), absorb(canton) vce(cl canton)

cap drop min_p
gen min_p=p if w==1
byso canton year: ereplace min_p=min(min_p)
ta min_p
replace min_p=99 if min_p==.
reghdfe lgpp_${inter} (c.d22)##c.(f18-f24)##i.quant_gini_can if (d20==0 | min_p>24) & inrange(p, 18, 30), absorb(canton#p year) vce(cl canton)
br if e(sample)

reghdfe lgpp_${inter} (c.d27)##c.(f23-f29)##i.quant_gini_can if (d26==0 | min_p>29) & inrange(p, 23, 31) & inrange(year, 2019, 2020), absorb(canton#p) vce(cl canton)
br if e(sample)


reghdfe lgpp_${inter} (c.d25)##c.(f22-f27)##i.quant_gini_can if (d24==0) & inrange(p, 22, 28), absorb(canton p) vce(cl canton)


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

xtset canton t

twowayfeweights lgpp_${inter} canton t the_shock_s, type(feTR) summary_measures
twowayfeweights lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33), type(feS) summary_measures

cap drop abs_the_shock_s
gen abs_the_shock_s=0
so canton t
byso canton year: replace abs_the_shock_s=sum(the_shock_s)
replace  abs_the_shock_s=3 if  abs_the_shock_s>3
ta abs_the_shock_s
br canton t the_shock_s abs_the_shock_s
byso canton year: egen abs_the_shock_s=max(the_shock_s)

cap drop can_year
egen can_year=group(canton year)
so canton t
byso canton year: gen tx=_n
br canton can_year t p tx year
encode dept, gen(ndept)
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 11, 26), by(quant_gini_can) placebo(3) effects(4) trends_nonparam(canton)

did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 11, 26), predict_het(quant_gini_can,4) placebo(3) effects(4) trends_nonparam(canton)


cap drop samp0
gen samp0=0
cap drop can_year
egen can_year=group(canton year)


xtset can_year p
eststo base: reghdfe lgpp_${inter} (F(3/1).the_shock_s the_shock_s L(1/3).the_shock_s)##i.quant_gini_can if inrange(p, 11, 26), a(can_year year p) vce(cl canton) 

ereturn list

loc g=4
forval q=1/`g' {
	cap drop shock_q`q'
	gen shock_q`q'=the_shock_s*gini_`q'
}
did_multiplegt_dyn lgpp_${inter} can_year p shock_q4 if inrange(p, 11, 26),  placebo(2) effects(2) trends_nonparam(canton)
}
estimates clear
loc g=4
forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 11, 26) & quant_gini_can==`q', placebo(2) effects(2) trends_nonparam(canton)
eststo rob_quant_gini_`q'
}



loc g=4
forval q=2/`g' {
	cap drop shock
	gen shock=the_shock_s*gini_`q'
	reghdfe lgpp_${inter} shock the_shock_s if inrange(p, 11, 26) & (quant_gini_can==`q' | quant_gini_can==1), a(can_year p) vce(cl canton)
	reghdfe lgpp_${inter} the_shock_s##gini_`q' if inrange(p, 11, 26) & (quant_gini_can==`q' | quant_gini_can==1), a(can_year p) vce(cl canton)

}

cap drop samp0
gen samp0=0
loc g=4
local nume 3
loc nump 3
loc num=`nume'+`nump'+1


forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26) & quant_gini_can==`q', placebo(`nump') effects(`nume') trends_nonparam(canton) 
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



loc g=2
local nume 1
loc nump 0
loc num=`nume'+`nump'+1
did_multiplegt_dyn dhat_lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26) & quant_gini_can==`g', placebo(`nump') effects(`nume') trends_nonparam(canton) 


forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26) & quant_gini_can==`q', placebo(`nump') effects(`nume') trends_nonparam(canton) 
}



reghdfe lgpp_${inter} the_shock_s if quant_gini_can==1, a(canton##p) resid

cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=.

levelsof canton if quant_gini_can==1, l(can)
levelsof t if quant_gini_can==1, l(time)
foreach c in `can' {
foreach t in `time' {
sum lgpp_${inter} if canton==`c'	& t==`t'
replace dhat_lgpp_${inter}=lgpp_${inter}-r(mean) if t==`t'
}	
}


cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=.
levelsof t, l(time)
foreach t in `time' {
sum lgpp_${inter} if quant_gini_can==1	& t==`t'
replace dhat_lgpp_${inter}=lgpp_${inter}-r(mean) if t==`t'
}	

cap drop can_year
egen can_year=group(canton year)

reg lgpp_${inter} i.can_year if quant_gini_can==1
cap drop hat_lgpp_${inter}
predict hat_lgpp_${inter}, xb


replace dhat_lgpp_${inter}=lgpp_${inter}-hat_lgpp_${inter}

reg lgpp_${inter} p##year##quant_gini_can
cap drop hat0_lgpp_${inter}
predict hat0_lgpp_${inter}, xb

sum lgpp_${inter} if quant_gini_can==1 & p==18
reg hat_lgpp_${inter} canton##p
cap drop hat_lgpp_${inter}
predict hat_lgpp_${inter}, xb

reghdfe lgpp_${inter} p##quant_gini_can##year, a(canton) resid
cap drop hat_lgpp_${inter}
predict hat_lgpp_${inter}, xb
cap drop hatFE_lgpp_${inter}
predict hatFE_lgpp_${inter}, xbd


reg hat_lgpp_${inter} p##year if quant_gini_can==1
cap drop hat1_lgpp_${inter}
predict hat1_lgpp_${inter}, xb

cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=hat0_lgpp_${inter}-hat1_lgpp_${inter}


sum hat_lgpp_${inter} if quant_gini_can==1 & p==18 & e(sample)
sum hat_lgpp_${inter} if quant_gini_can==4 & p==18 & e(sample)

reghdfe lgpp_${inter} p##quant_gini_can, a(can_year) resid
cap drop hat_lgpp_${inter}
predict hat_lgpp_${inter}, xb

reg hat_lgpp_${inter} i.t if quant_gini_can==1
cap drop hat1_lgpp_${inter}
predict hat1_lgpp_${inter}, xb

cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=hat_lgpp_${inter}-hat1_lgpp_${inter}

sum hat_lgpp_${inter} if quant_gini_can==1 & p==18 & e(sample)
sum hat_lgpp_${inter} if quant_gini_can==4 & p==18 & e(sample)

sum lgpp_${inter} if quant_gini_can==1 & p==18 & e(sample)
sum lgpp_${inter} if quant_gini_can==4 & p==18 & e(sample)

gen dhat_lgpp_${inter}=lgpp_${inter}-hat_lgpp_${inter}

reg hat_lgpp_${inter} p##year if quant_gini_can==1
cap drop hat1_lgpp_${inter}
predict hat1_lgpp_${inter}, xb
cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=lgpp_${inter}-hat1_lgpp_${inter}
so canton t
br canton can_year year p lgpp_${inter} hat_lgpp_${inter} hat1_lgpp_${inter} dhat_lgpp_${inter} quant_gini_can if p==28

ta quant_gini_can if inrange(p, 18, 26)

byso quant_gini_can: sum dhat_lgpp_${inter}
cap drop dhat_lgpp_${inter}
gen dhat_lgpp_${inter}=lgpp_${inter}-hat_lgpp_${inter}
br canton quant_gini_can


byso year: sum lgpp_${inter} if quant_gini_can==1 & p==22
cap drop hat_lgpp_${inter}
byso canton quant_gini_can year p: egen hat_lgpp_${inter}=mean(lgpp_${inter})
byso year: sum hat_lgpp_${inter} if quant_gini_can==1 & p==22
replace hat_lgpp_${inter}=. if quant_gini_can>1
byso year p: ereplace hat_lgpp_${inter}=max(hat_lgpp_${inter})
replace hat_lgpp_${inter

cap drop samp0
gen samp0=0
matrix drop _all
loc g=4
local nume 1
loc nump 0
did_multiplegt_dyn dhat_lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26), placebo(`nump') effects(`nume') by(quant_gini_can) trends_nonparam(canton)
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26), placebo(`nump') effects(`nume') by(quant_gini_can) trends_nonparam(canton)

loc num=`nume'+`nump'+1
forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 18, 26) & quant_gini_can==`q', effects(`nume') placebo(`nump') 
}

}

ta p the_shock_s

matrix drop _all
loc g=4
local nume 2
loc nump 2
loc num=`nume'+`nump'+1
did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 11, 26), placebo(`nump') effects(`nume') by(quant_gini_can)   trends_nonparam(canton) only_never_switchers


di -.1654452 - -.1434372 
di -.1332689 - -.0886957

pause on 
preserve
gen w2=w
so canton t
byso canton year: replace w2=1 if w[_n+1]==1
so canton t
br canton year t w w2
pause

matrix drop _all
loc g=4
local nume 2
loc nump 2
loc num=`nume'+`nump'+1
forval q=1/`g' {
did_multiplegt_dyn lgpp_${inter} can_year p w if inrange(p, 18, 26) & quant_gini_can==`q', effects(`nume') placebo(`nump')  trends_nonparam(canton)


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




loc j 1
foreach x in $treat_period  {
local lab_vars "`lab_vars' `j' "`x'""
loc ++j
}

mac list _lab_vars
dis `lab_vars'




xlabel(`lab_vars',  labsize(small) angle(45))  ytitle(Impact) ;
graph export "${tex_fig2}/cumul_impact_`dep'.png", replace;

matlist e(b)
esttab rob_quant_gini*
estimates restore rob_quant_gini1
loc var rob_quant_gini
		#delimit
	coefplot
	(`var'_1, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_2, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_3, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3)))
		(`var'_4, m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(gs0))), keep(Av_tot_eff)
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) name(dCH, replace) ;
			*graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr


loc var rob_quant_gini
		#delimit
	coefplot
	(`var'_1, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_2, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_3, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3)))
		(`var'_4, m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(gs0))), keep(Pla* Eff*) order(Placebo_2 Placebo_1 Effect_1 Effect_2)
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) ;
			*graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr

xtset can_year p
ta quant_gini_can, g(gini_)
forval q=1/4 {
gen shock_gini_`q'=the_shock_s*gini_`q'	
}

cap drop samp0
eststo base: reghdfe lgpp_${inter} shock_gini_*  if inrange(p, 11, 26), a(can_year p) vce(cl canton) 


		#delimit
	coefplot base, keep(shock_gini_*)
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) ;
			*graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr

forval q=1/4 {
eststo m_`q': reghdfe lgpp_${inter} the_shock_s##b`q'.quant_gini_can if inrange(p, 11, 26), a(can_year p) vce(cl canton) 
}
loc var m
		#delimit
	coefplot
	(`var'_1, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_2, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_3, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3)))
		(`var'_4, m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(gs0))), keep(1.the_shock_s)
				yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) ;
			*graph export "$figs/het_impact_`var'_`shock'.png", replace ;
		#delimit cr

estout base, keep(shock_gini_1)
eststo m1
		#delimit
coefplot m1, yline(0, lcolor(gs0)) vertical 
				ciopts(/*recast(rline) */lpattern(dash)) 
				ytitle("") xtitle("") scheme(plotplainblind) ;

testparm shock_gini_*, equal

eststo base: reghdfe lgpp_${inter} shock_gini_1 gini_? if inrange(p, 11, 25), a(can_year p) vce(cl canton) 
testparm shock_gini_*, equal


	coefplot(`var'_2, m(o) mc(gs9)  ciopts(lpattern(shortdash) lcolor(gs9)))
		(`var'_3, m(o) mc(gs6)  ciopts(lpattern(shortdash) lcolor(gs6)))
		(`var'_4, m(o) mc(gs3)  ciopts(lpattern(shortdash) lcolor(gs3)))
		(`var'_5, m(o) mc(gs0) ciopts(lpattern(shortdash) lcolor(gs0)))

eststo base: reghdfe lgpp_${inter} (F(3/1).the_shock_s the_shock_s L(1/3).the_shock_s)##i.quant_gini_can if inrange(p, 18, 25) & samp0, a(can_year p) vce(cl canton) 


eststo base: reghdfe lgpp_${inter} the_shock_s##i.quant_gini_can if inrange(p, 11, 25), a(can_year p) vce(cl canton) 

ta p the_shock_s
ta month p

xtset can_year p
cap drop ?1the_shock_s
gen F1the_shock_s=F1.the_shock_s
gen L1the_shock_s=L1.the_shock_s
gen L2the_shock_s=L2.the_shock_s
br canton year t p ??the_shock_s the_shock_s shock_p
eststo base: reghdfe lgpp_${inter} (F(3/1).the_shock_s the_shock_s L(1/3).the_shock_s)##i.quant_gini_can if inrange(p, 18, 25), a(can_year p) vce(cl canton) 

cap drop shock_p
gen shock_p=p if the_shock_s
byso canton year: ereplace shock_p=min(shock_p)

cap drop time_to_shock
gen time_to_shock=p-shock_p
ta time_to_shock
br canton year t p ??the_shock_s the_shock_s shock_p time_to_shock

xtset can_year p
 eventdd lgpp_${inter} the_shock_s i.p if inrange(p, 11, 33), timevar(time_to_shock) method(fe, cluster(canton)) graph_op(ytitle("shock to gpp") xlabel(-20(5)25))
didregress (lgpp_${inter}) (the_shock_s) if inrange(p, 11, 33), group(can_year) time(p)
estat granger
estat grangerplot
estat ptrends  
cap drop treated
byso canton year: egen treated=max(the_shock_s)

reghdfe lgpp_${inter} the_shock_s##i.quant_gini_can if inrange(p, 11, 33), a(can_year p) vce(cl canton) 
eststo base: reghdfe lgpp_${inter} the_shock_s##i.quant_gini_can if inrange(p, 11, 33) & wgini_med_seminat, a(canton#year p) vce(cl canton) 
eststo base: reghdfe lgpp_${inter} the_shock_s##i.quant_gini_can if inrange(p, 11, 33) & !wgini_med_seminat, a(canton#year p) vce(cl canton) 


did_multiplegt_dyn lgpp_${inter} can_year p the_shock_s if inrange(p, 11, 33), by(quant_gini_can) effects(1) trends_nonparam(canton) normalized 

did_multiplegt_dyn lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33),  placebo(3) effects(3) trends_nonparam(quant_gini_can)


did_multiplegt_dyn lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33) & quant_gini_can==4,  effects(1) 
matlist e(b)
did_multiplegt_dyn lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33) & quant_gini_can==3,  placebo(4) effects(4) 
did_multiplegt_dyn lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33) & quant_gini_can==2,  placebo(4) effects(4) 
did_multiplegt_dyn lgpp_${inter} canton t the_shock_s if inrange(p, 11, 33) & quant_gini_can==1,  placebo(3) effects(3) 

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


ssc install rdrobust, replace
ssc install rddensity, replace

h rdrobust

cap drop cut
sum tsup_h_q if the_shock_s,d
global cut=r(min)

** Mimicking variance RD plot with quantile-spaced bins


forval q=1(1)4 {
	sum tsup_h_q if the_shock_s & quant_gini_can==`q',d
loc cut=r(min)
	rdrobust lgpp_${inter} tsup_h_q if quant_gini_can==`q' & tsup_h_q>25, c(`cut') p(1) kernel(triangular) bwselect(mserd) vce(nncluster canton) 
local bandwidth = e(h_l)
*rdbwselect Y X, kernel(triangular) p(1) bwselect(mserd) covs($covariates) vce(nncluster prov_num) 
rdplot lgpp_${inter} tsup_h_q if quant_gini_can==`q' & tsup_h_q >= `bandwidth', c(`cut') h(`bandwidth') (binselect(qsmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Temp) ytitle(Outcome)) name(rd_`q', replace)
}

rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local bandwidth = e(h_l)
*rdplot Y X if abs(X) <= `bandwidth', p(1) h(`bandwidth') kernel(triangular)
rdplot Y X if abs(X) <= `bandwidth', p(1) h(`bandwidth') kernel(triangular) ci(95)



* Productivity and temperature
cap drop cut
sum tsup_h_q if the_shock_s,d
global cut=r(min)
gen int cut=round(${cut})
binsreg lgpp_${inter} tsup_h_q, by(quant_gini_can) absorb(canton##year p) samebinsby line(1 1)  usegtools(on) ///
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
binsreg lgpp_${inter} tsup_h_q if tsup_h_q>=`cut', by(quant_gini_can) absorb(canton##year p) samebinsby polyreg(1) polyregcigrid(1) usegtools(on) ///
savedata(data/FR/aux_files/bin_data_zoom`cut') replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 4 "q2" 7 "q3" 10 "q4" )  title(Gini quantiles, size(small))   pos(0) bplace(ne)) 
graph export "$figs/gpp_temp_by_gini_zoom`cut'.png", replace 

binsreg gpp_${inter} tsup_h_q, by(quant_fsize) absorb(canton##year p) samebinsby line(1 1)  usegtools(on) ///
savedata(data/FR/aux_files/bin_data1_fsize) replace xtitle(Temperature °C) ytitle(GPP in C.kg/m{superscript:2} (weekly)) ///
legend(order(1 "q1" 3 "q2" 5 "q3" 7 "q4" )  title(Farm size quantiles, size(small))   pos(0) bplace(nw)) 
graph export "$figs/gpp_temp_by_fsize.png", replace 

sum cut
loc cut=r(mean)
binsreg gpp_${inter} tsup_h_q if tsup_h_q>=`cut', by(quant_fsize p) absorb(canton##year) samebinsby polyreg(1) polyregcigrid(1) usegtools(on) ///
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



