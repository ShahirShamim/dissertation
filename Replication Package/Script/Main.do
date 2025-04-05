clear all
macro drop _all
cd "C:\Users\shahi\OneDrive - University of Warwick\Dissertation\Replication Package"


//// MICS-6 Dataset


// Merging datasets

use "Datasets/Pakistan Sindh MICS6 SPSS Datasets\wm.dta" 

merge 1:1 HH1 HH2 LN using "Datasets\Pakistan Sindh MICS6 SPSS Datasets\hhplushl.dta"

drop _merge

// Creating running variable

replace WB3M = 0 if WB3M == 98

drop if WB3Y == 9999
drop if WB3Y == 9998

gen birth_years = WM6Y - WB3Y
gen birth_months = WM6M - WB3M

gen age_months = (birth_years * 12) + birth_months

gen age_in_oct13 = age_months - (( WM6Y - 2013)*12 + ( WM6M - 10))


drop if age_in_oct13 <  (18*12) - (3*12)
drop if age_in_oct13 >  (18*12) + (3*12)

gen running_var = age_in_oct13 - 216

gen treated = 1 if age_in_oct13 < 216
replace treated = 0 if treated==.

// Creating outcome variables

replace MA8M = 0 if MA8M == 97
replace MA8M = 0 if MA8M == 98
replace MA8M = 0 if MA8M == 99

drop if MA8Y == 9997
drop if MA8Y == 9998

gen marriage_years = MA8Y - WB3Y
gen marriage_months = MA8M - WB3M

gen marriage_age_months = (marriage_years * 12) + marriage_months

gen empow = DV1A if DV1A==1
replace empow = 0 if empow==.
replace empow = empow + DV1B if DV1B==1
replace empow = empow + DV1C if DV1C==1
replace empow = empow + DV1D if DV1D==1
replace empow = empow + DV1E if DV1E==1
replace empow =. if DV1A > 2 & DV1B > 2 & DV1C > 2 & DV1D > 2 & DV1E > 2 

gen year_first_birth = 1900 + floor((WDOBFC - 1)/12)
gen month_first_birth = WDOBFC - 12*(year_first_birth - 1900)

gen first_birth_years = year_first_birth - WB3Y
gen first_birth_months = month_first_birth - WB3M

gen first_birth_age_months = (first_birth_years * 12) + first_birth_months

replace DB2 =. if DB2 == 9

gen child_survival = CSURV/(CDEAD + CSURV)

gen schooling = 0 if WB5 == 2
replace schooling = WB6A + 1 if schooling ==.
lab def school 0"None" 1"ECE" 2"PRIMARY" 3"MIDDLE" 4"SECONDARY" 5"HIGHER"
lab val schooling school

// Improving readability

rename marriage_age_months age_married
rename CEB num_children
rename first_birth_age_months first_birth
rename MN32 size_child
rename DB2 wanted_child


// Creating discriptive stats

dtable age_in_oct13 age_married wscore i.welevel i.HH6 i.DV1A i.WM14 i.WB5 CM11 i.CM8 i.HC12 i.HC14 i.HC15 , sample(,  place(seplabels)) sformat("(N=%s)" frequency) nformat(%7.2f mean sd) note(The table contains discriptive statistics for the sample.)  column(by(hide)) nformat(%7.2f mean sd) title(Descriptive Statistics) export("Tables\disc_stata.tex", replace)

dtable age_in_oct13 age_married wscore i.welevel i.HH6 i.DV1A i.WM14 i.WB5 CM11 i.CM8 i.HC12 i.HC14 i.HC15 ,by(treated, tests) sample(,  place(seplabels)) sformat("(N=%s)" frequency) nformat(%7.2f mean sd) note(The table contains discriptive statistics for observations around the cutoff of 216 months of age. Row 0 indicates women who were less than 18 when the law was passed (Control), while row 1 indicates women who were older (Treated). It also contains tests.)  column(by(hide)) nformat(%7.2f mean sd) title(Descriptive Statistics by cutoff) export("Tables\desc_stats_by_cutoff.tex", replace)


// Selecting optimal bandwidth

rdbwdensity running_var ,c(0) kernel(uniform) vce(jackknife)
rdbwdensity running_var ,c(0) kernel(triangular) vce(jackknife)
rdbwdensity running_var ,c(0) kernel(epanechnikov) vce(jackknife)

********************************************************************************
*********************** Cattaneo, Jansson, and Ma (2020) ***********************
********************************************************************************

//Cattaneo, Jansson, and Ma (2020) test for manipulation
//See https://cran.r-project.org/web/packages/rdrobust/rdrobust.pdf
//Note that it doesn't matter if treatment is on the right or left side of the
//cutoff it's set at 0.5.



rddensity running_var, c(0)  kernel(uniform) p(2) plot  graph_opt(xtitle("Running Variable") ytitle("Density") title("RDD Density Plot") legend(off) scheme(s2mono))  plotl_estype(line)  plotl_citype(region) all h(23 22)

graph export "Figures\rddensity_uni.png", as(png)  replace

rddensity running_var, c(0) kernel(triangular) p(2) plot  graph_opt(xtitle("Running Variable") ytitle("Density") title("RDD Density Plot") legend(off) scheme(s2mono))  plotl_estype(line)  plotl_citype(region) all h(23)

graph export "Figures\rddensity_tri.png", as(png)  replace


rddensity running_var, c(0) kernel(epanechnikov) p(2) plot  graph_opt(xtitle("Running Variable") ytitle("Density") title("RDD Density Plot") legend(off) scheme(s2mono))  plotl_estype(line)  plotl_citype(region) all h(23)

graph export "Figures\rddensity_epn.png", as(png)  replace



********************************************************************************
******************************** McCrary (2008) ********************************
********************************************************************************

DCdensity running_var, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
di "T-statistic: " r(theta)/r(se)

graph export "Figures\DCdensity.png", as(png)  replace

drop Xj Yj r0 fhat se_fhat	
	
// Outcome variables 


// age_married 
// CEB // Children ever born
// child_survival // Surviving childen
// empow 
// first_birth_age_months
// MN32 // Size of child at birth
// DB2 // Wanted last child



// Regression Tables

/// Unifrom Kernel
//// P(1)

rdrobust  age_married running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(1) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P1.tex", title("RD Estimates of P(1) using Uniform Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 1, uniform kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs


//// P(2) ---- Used for results

rdrobust  age_married running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P2.tex", title("RD Estimates of P(2) using Uniform Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 2, uniform kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs

//// P(3)

rdrobust  age_married running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(3) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P3.tex", title("RD Estimates of P(3) using Uniform Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 3, uniform kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs


//// P(4)

rdrobust  age_married running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(4) all h(23 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P4.tex", title("RD Estimates of P(4) using Uniform Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 4, uniform kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs



// List of outcome variables
local outcomes age_married num_children child_survival empow first_birth size_child wanted_child schooling

global x running_var
global c 0
su $x
global x_min = r(min)
global x_max = r(max)

foreach y in `outcomes' {
    forval p = 1/4 {
        // Generate RD plot variables for the current polynomial order
        rdplot `y' $x, p(`p') all h(23 22) kernel(uniform) level(95) masspoints(adjust) genvars hide
        
        // Create the twoway plot for the current polynomial order
        twoway (scatter rdplot_mean_y rdplot_mean_bin, sort msize(small)  mcolor(gs10)) ///
        (function `e(eq_l)', range($x_min $c) lcolor(black) sort lwidth(medthin) lpattern(solid)) ///
        (function `e(eq_r)', range($c $x_max) lcolor(black) sort lwidth(medthin) lpattern(solid)), ///
        xline($c, lcolor(black) lwidth(medthin)) xscale(r($x_min $x_max)) /// 
        legend(pos(6) cols(3) order(2 "Polynomial fit of order `p'" 1 "`y'" 4 "Uniform Kernel")) ///
        title("Regression function fit for `y' (P(`p'))", color(gs0)) ///
        name(graph_`y'_p`p', replace)   // Name the graph for later use or export
        
        // Export the graph as a file
        graph save  "Figures\RDplot_uniform_`y'_P`p'.gph", replace
		graph export "Figures\RDplot_uniform_`y'_P`p'.png", as(png) replace
        
        // Drop generated variables
        drop rdplot_ci_l rdplot_ci_r rdplot_hat_y rdplot_id rdplot_max_bin rdplot_mean_bin rdplot_mean_x rdplot_mean_y rdplot_min_bin rdplot_N rdplot_se_y
    }
}



foreach y in `outcomes' {
    graph combine "Figures\RDplot_uniform_`y'_P1.gph" "Figures\RDplot_uniform_`y'_P2.gph" "Figures\RDplot_uniform_`y'_P3.gph" "Figures\RDplot_uniform_`y'_P4.gph", cols(2)
	graph save "Figures\RDplot_uniform_`y'_Combined.gph", replace
	graph export "Figures\RDplot_uniform_`y'_Combined.png", as(png) replace
}


// Drop all global variables at the end
macro drop _all






/// Triangular Kernel
//// P(1)

rdrobust  age_married running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(1) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P1_triangular.tex", title("RD Estimates of P(1) using Triangular Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 1, triangular kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs



//// P(2)

rdrobust  age_married running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(2) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P2_triangular.tex", title("RD Estimates of P(2) using Triangular Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 2, triangular kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs
//// P(3)

rdrobust  age_married running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(3) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P3_triangular.tex", title("RD Estimates of P(3) using Triangular Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 3, triangular kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs

//// P(4)

rdrobust  age_married running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(4) all h(23 22) kernel(triangular) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P4_triangular.tex", title("RD Estimates of P(4) using Triangular Kernel") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 4, triangular kernel and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs


// List of outcome variables
local outcomes age_married num_children child_survival empow first_birth size_child wanted_child schooling

global x running_var
global c 0
su $x
global x_min = r(min)
global x_max = r(max)

foreach y in `outcomes' {
    forval p = 1/4 {
        // Generate RD plot variables for the current polynomial order
        rdplot `y' $x, p(`p') all h(23 22) kernel(triangular) level(95) masspoints(adjust) genvars hide
        
        // Create the twoway plot for the current polynomial order
        twoway (scatter rdplot_mean_y rdplot_mean_bin, sort msize(small)  mcolor(gs10)) ///
        (function `e(eq_l)', range($x_min $c) lcolor(black) sort lwidth(medthin) lpattern(solid)) ///
        (function `e(eq_r)', range($c $x_max) lcolor(black) sort lwidth(medthin) lpattern(solid)), ///
        xline($c, lcolor(black) lwidth(medthin)) xscale(r($x_min $x_max)) /// 
        legend(pos(6) cols(3) order(2 "Polynomial fit of order `p'" 1 "`y'" 4 "triangular Kernel")) ///
        title("Regression function fit for `y' (P(`p'))", color(gs0)) ///
        name(graph_`y'_p`p', replace)  // Name the graph for later use or export
        
        // Export the graph as a file
        graph save "Figures\RDplot_triangular_`y'_P`p'.gph", replace
		graph export "Figures\RDplot_triangular_`y'_P`p'.png", as(png) replace
        
        // Drop generated variables
        drop rdplot_ci_l rdplot_ci_r rdplot_hat_y rdplot_id rdplot_max_bin rdplot_mean_bin rdplot_mean_x rdplot_mean_y rdplot_min_bin rdplot_N rdplot_se_y
    }
}




foreach y in `outcomes' {
    graph combine "Figures\RDplot_triangular_`y'_P1.gph" "Figures\RDplot_triangular_`y'_P2.gph" "Figures\RDplot_triangular_`y'_P3.gph" "Figures\RDplot_triangular_`y'_P4.gph", cols(2)
	graph save "Figures\RDplot_triangular_`y'_Combined.gph", replace
	graph export "Figures\RDplot_triangular_`y'_Combined.png", as(png) replace
}



// Robustness: Changing bandwidts

rdrobust  age_married running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(2) all h(13 12) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P2_Small.tex", title("RD Estimates of P(2) with Smaller Bandwidth") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 2, uniform kernel and bandwidths of 13 months on the left of the cutoff and 12 months on the right of the cutoff.") replace booktabs

rdrobust  age_married running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model1
rdrobust  num_children running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model2
rdrobust  child_survival running_var ,p(2) all h(13 22) kernel(uniform) level(95) masspoints(adjust)
estimates store model3
rdrobust  empow running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model4
rdrobust  first_birth running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model5
rdrobust  size_child running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model6
rdrobust  wanted_child running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model7
rdrobust  schooling running_var ,p(2) all h(33 32) kernel(uniform) level(95) masspoints(adjust)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P2_Large.tex", title("RD Estimates of P(2) with Larger Bandwidth") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 2, uniform kernel and bandwidths of 33 months on the left of the cutoff and 32 months on the right of the cutoff.") replace booktabs




// Robustness: Placebo

rdrobust  age_married running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model1
rdrobust  num_children running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model2
rdrobust  child_survival running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model3
rdrobust  empow running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model4
rdrobust  first_birth running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model5
rdrobust  size_child running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model6
rdrobust  wanted_child running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model7
rdrobust  schooling running_var ,p(2) all h(23 22) kernel(uniform) level(95) masspoints(adjust) c(-8)
estimates store model8
esttab model1 model2 model3 model4 model5 model6 model7 model8 using "Tables\RD_P2_Placebo.tex", title("RD Estimates of P(2) with Placebo Cutoff") note("The table is generated by estimating the effect of the SCMRA on the 8 outcome variables present in each column. The reported values are the estimated coefficients and the standard error in the brackets. Three separate estimates are reported for Conventional, Bias-corrected and Robust estimators using the STATA rdrobust package with polynomials of degree 2, uniform kernel, modified cutoff of -8 and bandwidths of 23 months on the left of the cutoff and 22 months on the right of the cutoff.") replace booktabs

