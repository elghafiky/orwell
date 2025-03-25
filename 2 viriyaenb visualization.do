* ==== PRELIMINARIES ==== *
* GOAL: VISUALIZE REGRESSION RESULTS FOR VIRIYAENB REPORT

* clear environment
capture log close
clear all 
set more off

* ==== DK ==== *
** linear model
use "$temp\DK_wyoung_linear_allmodel.dta", clear

** plot 
set scheme plotplainblind
qui levelsof treatnm
foreach k of numlist `r(levels)' {
	qui levelsof outnum if treatnm==`k'
	foreach i of numlist `r(levels)' {
		loc fignm "vnb_DK`i'_treat`k'_wyoung_linear"
		preserve
		keep if outnum==`i' & treatnm==`k'
		foreach y of varlist lci uci {
			qui sum `y'
			loc max`y'=abs(`r(max)')
		}
		loc intval=max(`maxlci',`maxuci')
		loc steps=`intval'/4
		loc textx= .25
		loc texty=`intval'/2.5 
		twoway 	(rcap uci lci equation, vertical lcolor(gray)) ///
				(bar coef equation if equation==1, vertical) ///
				(bar coef equation if equation==2, vertical) ///
				(bar coef equation if equation==3, vertical), ///
				ytitle("Probability relative to control", size(small)) yline(0, lpattern(dash) lcolor(red)) yscale(range(-`intval' `intval')) ylab(-`intval'(`steps')`intval',format(%12.2fc))  /// 
				legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				xscale(range(0 4) reverse noline) xtitle("") xlabel(none) ///
				note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
				text(-`texty' `textx' "Less likely to support",size(vsmall)) text(`texty' `textx' "More likely to support",size(vsmall)) ///
				subtitle("Linear model", size(small)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}

** ordinal logit model
foreach x in ologit  {
	use "$temp\DK_wyoung_`x'_allmodel.dta", clear 
	set scheme plotplainblind
	loc texty .25
	loc textx .4
	loc intval 1
	loc steps .25
	if "`x'"=="oprobit" {
		loc axistitle "z-score/probit index"
		loc subtitle "Ordered probit"
	}
	else {
		loc axistitle "Odds ratio relative to control"
		loc subtitle "Ordered logit"
	}
	qui levelsof treatnm
	foreach k of numlist `r(levels)' {
		qui levelsof outnum if treatnm==`k'
		foreach i of numlist `r(levels)' {
			loc fignm "vnb_DK`i'_treat`k'_wyoung_`x'"
			preserve
			keep if outnum==`i' & treatnm==`k'
			twoway 	(rcap uci lci equation, horizontal lcolor(gray)) ///
					(dot coef equation if equation==1, horizontal) ///
					(dot coef equation if equation==2, horizontal) ///
					(dot coef equation if equation==3, horizontal), ///
					xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
					legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
					yscale(range(0 4) reverse noline) ytitle("") ylabel(none) ///
					note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
					text(`texty' -`textx' "More inclined to oppose",size(vsmall)) text(`texty' `textx' "More inclined to support",size(vsmall)) ///
					subtitle("`subtitle'", size(small)) saving("$fig\\`fignm'.gph", replace )
			gr export "$fig\\`fignm'.png", replace 		
			restore 
		}
	}
}

*** COMBINE WYOUNG GRAPHS
qui levelsof treatnm
foreach k of numlist `r(levels)' {
	qui levelsof outnum if treatnm==`k'
	foreach i of numlist `r(levels)' {
		grc1leg2  "$fig\vnb_DK`i'_treat`k'_wyoung_ologit.gph" "$fig\vnb_DK`i'_treat`k'_wyoung_linear.gph", row(1) pos(12) ///
		notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 use PDS Lasso.", size(tiny)) ///
		plotr(margin(zero))
		gr export "$fig\vnb_DK`i'_treat`k'_wyoung_combined.png", replace
	}
}
