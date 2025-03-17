* ==== PRELIMINARIES ==== *
* GOAL: CONDUCT BALANCE TEST

* clear environment
capture log close
clear all 
set more off

* ==== PROGRAMS ==== *
// net describe ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
// net install ritest 

pro setupdata 
	* load data
	use "$ipt\raw_$date.dta", clear 
	
	* region
	clonevar region=ID01
	recode region (1/16 = 1) (20/21=1) (17/19=2) (22/30=2) (31/38=3)
	qui sum region
	forval i = 1/`r(max)' {
		g region`i'=region==`i'
	}
	la def rgnlab 1 "West" 2 "Central" 3 "East"
	la val region rgnlab
	
	* province
	clonevar prov=ID01

	* urban
	g urban=ID02==1

	* male
	clonevar sex=ID03
	g male=ID03==1
	
	* age
	clonevar age=ID04

	* unmarried
	g unmarried=ID05==1

	* education
	clonevar edlvl=ID06
	qui sum ID06
	forval i=1/`r(max)' {
		g edu`i'=ID06==`i'
	}

	* gender household head
	clonevar hhhead_gender=RT01
	g hhhead_female=RT01==2

	* social assistance
	g nosocast=RT02==3

	* household size 
	egen hhsize=rowtotal(RT03*)
	
	* generate overall treatment status
	g treat=inrange(lfCB,1,5)

	la def polsuplab 1 "Against" 2 "Undecided" 3 "Support"
	forval i = 1/13 {
		* generate binary policy support/oppose/undecided indicator
		g support`i'=inrange(QDKr`i',4,5)
		g resist`i'=inrange(QDKr`i',1,2)
		g undecided`i'=QDKr`i'==3
		
		* recode policy support likert
		clonevar QDKr_recoded`i'=QDKr`i'
		recode QDKr_recoded`i' (2=1) (3=2) (4/5=3)
		la val QDKr_recoded`i' polsuplab
	}
	
	* correct interpretation of message
	foreach x in A B C D E {
		if "`x'"=="D" {
			g correctCB01`x'=inrange(CB01`x',2,3)
		}
		else {
			g correctCB01`x'=CB01`x'==1
		}
		replace correctCB01`x'=1 if lfCB==6
	}
	egen crt_intrpt_msg=rowmax(correctCB01*)
	
	* length of reading stimulus
	egen read_stim_time=rowmean(pagetimestim*)
	
	* social desirability bias index
	foreach i of numlist 5 7 9 10 13 {
		recode SBr`i' (1=2) (2=1)
	}
	egen sdbi=rowtotal(SBr*)
	
	* clean outlier in hhsize
	replace hhsize=. if hhsize>12
	
	* matching outcome with treatment
	foreach x in QDKr_recoded support resist undecided {
		replace `x'1=. if inrange(lfCB,3,4)
		replace `x'2=. if inlist(lfCB,1,4,5)
		replace `x'3=. if inlist(lfCB,1,4,5)
		replace `x'4=. if inlist(lfCB,5)
		replace `x'5=. if inrange(lfCB,4,5)
		replace `x'6=. if inrange(lfCB,2,4)
		replace `x'7=. if inrange(lfCB,2,4)
		replace `x'8=. if inrange(lfCB,3,5)
		replace `x'9=. if inlist(lfCB,1,4,5)
		replace `x'10=. if inrange(lfCB,2,4)
		replace `x'11=. if inrange(lfCB,3,5)
		replace `x'12=. if inlist(lfCB,1,4,5)
		replace `x'13=. if inlist(lfCB,1,4,5)
	}

	* set scheme and covariates
	set scheme plotplainblind
	gl covariates i.region urban male age i.edlvl hhhead_female nosocast hhsize
	gl cognitivecontrols pagetimeQDK read_stim_time sdbi
	
// 	* randomly drop jakarta oversampling 
// 	count 
// 	loc excess=`r(N)'-4000
// 	count if ID01==11
// 	loc jktsampsi=`r(N)'-`excess'
// 	preserve 
// 	drop if ID01==11
// 	tempfile nojkt
// 	save `nojkt', replace
// 	restore
// 	keep if ID01==11 
// 	set seed 861288800
// 	sample `jktsampsi', count
// 	append using `nojkt'
end

pro savepval
	regsave, tstat pval
end

* ==== POLICY SUPPORT ==== *
** OLS and PD LASSO
setupdata
est clear 
forval i = 1/13 {
	foreach x in support resist undecided {
		foreach y in ib6.lfCB {
			if "`y'"=="treat" {
				loc treattype "ovte"
				loc coef treat
				loc coefnm "treat"
				loc yscale yscale(off)
			}
			else {
				loc treattype "mlte"
				loc coef *.lfCB
				loc coefnm "lfCB"
				loc yscale 
			}
			foreach z in crt all {
				loc baseeq `x'`i' `y'
				if "`z'"=="crt" {
					loc equation `baseeq' if crt_intrpt_msg==1
				}
				else {
					loc equation `baseeq'
				}
			
				reg `equation', r
				eststo modela`treattype'`x'`i'`z'
				preserve
				savepval
				keep if regexm(var,"`coefnm'")
				g model="a"
				g outcome="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resa`treattype'`x'`i'`z'
				save `resa`treattype'`x'`i'`z'', replace 
				restore
				
				preserve
				keep if `x'`i'<.
				dsregress `equation', controls(() $covariates)
				eststo modelb`treattype'`x'`i'`z'
				savepval
				keep if regexm(var,"`coefnm'")
				g model="b"
				g outcome="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resb`treattype'`x'`i'`z'
				save `resb`treattype'`x'`i'`z'', replace 
				restore
				
				preserve
				keep if `x'`i'<.
				dsregress `equation', controls(($cognitivecontrols) $covariates)
				eststo modelc`treattype'`x'`i'`z'
				savepval
				keep if regexm(var,"`coefnm'")
				g model="c"
				g outcome="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resc`treattype'`x'`i'`z'
				save `resc`treattype'`x'`i'`z'', replace 
				restore
				
				loc texty .5
				loc textx .1
				loc intval .3
				loc steps .1
				
				if "`x'"=="undecided" {
					loc stance "be `x'"
				}
				else {
					loc stance "`x'"
				}
				
				loc fignm "DK`i'_`treattype'_`x'_`z'_unadjusted"
				coefplot modela`treattype'`x'`i'`z' modelb`treattype'`x'`i'`z' modelc`treattype'`x'`i'`z', yscale(noline) ylab(,notick) ///
				text(`texty' -`textx' "Less likely to `stance'",size(vsmall)) text(`texty' `textx' "More likely to `stance'",size(vsmall)) ///
				keep(`coef') xline(0, lpattern(dash) lcolor(red)) xtitle("Probability to `stance' relative to control", size(small)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval') ///
				legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 4 "Model 2: Model 1 + covariates" 6 "Model 3: Model 2 + cognitive controls") size(vsmall)) `yscale' ///
				subtitle("Linear estimator", size(small)) note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) saving("$fig\\`fignm'.gph", replace) 
				gr export "$fig\\`fignm'.png", replace 
			}
		}
	}
}

* storing pvalues
foreach mod in a b c {
	foreach x in support resist undecided {
		foreach y in mlte {
			foreach z in crt all {
				use `res`mod'`y'`x'1`z'', clear 
				forval i=2/13 {
					append using `res`mod'`y'`x'`i'`z''
				}
				save "$temp\DK_`y'_`x'_model`mod'_`z'.dta", replace 
			}
		}
	}
}

** ORDERED PROBIT & LOGIT
setupdata
est clear 
foreach y in ib6.lfCB {
	if "`y'"=="treat" {
		loc treattype "ovte"
		loc coef treat
		loc coefnm "treat"
		loc yscale yscale(off)
	}
	else {
		loc treattype "mlte"
		loc coef *.lfCB
		loc coefnm "lfCB"
		loc yscale 
	}
	foreach x in oprobit ologit {
		if "`x'"=="oprobit" {
			loc axistitle "z-score/probit index"
			loc regress_options r 
			loc subtitle "Ordered probit"
		}
		else {
			loc axistitle "Odds ratio relative to control"
			loc regress_options r or
			loc subtitle "Ordered logit"
		}
		forval i=1/13 {
			foreach z in crt all {
				loc baseeq QDKr_recoded`i' `y'
				if "`z'"=="crt" {
					loc cond if crt_intrpt_msg==1
				}
				else {
					loc cond
				}
				
				`x' `baseeq' `cond', `regress_options'
				eststo modela`treattype'`x'`i'`z'
				preserve
				savepval
				keep if regexm(var,"`coefnm'")
				g model="a"
				g estimator="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resa`treattype'`x'`i'`z'
				save `resa`treattype'`x'`i'`z'', replace 
				restore
				
				qui dsregress `baseeq' `cond', controls(() $covariates)
				loc selected_controls `e(controls_sel)'
				`x' `baseeq' `selected_controls' `cond', `regress_options'
				eststo modelb`treattype'`x'`i'`z'
				preserve
				savepval
				keep if regexm(var,"`coefnm'")
				g model="b"
				g estimator="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resb`treattype'`x'`i'`z'
				save `resb`treattype'`x'`i'`z'', replace 
				restore
			
				qui dsregress `baseeq' `cond', controls(($cognitivecontrols) $covariates)
				loc selected_controls `e(controls_sel)'
				`x' `baseeq' `selected_controls' $cognitivecontrols `cond', `regress_options'
				eststo modelc`treattype'`x'`i'`z'
				preserve
				savepval
				keep if regexm(var,"`coefnm'")
				g model="c"
				g estimator="`x'"
				g policy=`i'
				g sample="`z'"
				tempfile resc`treattype'`x'`i'`z'
				save `resc`treattype'`x'`i'`z'', replace 
				restore
				
				loc texty .5
				loc textx .4
				loc intval 1
				loc steps .25
				
				loc fignm "DK`i'_`treattype'_`x'_`z'_unadjusted"
				coefplot modela`treattype'`x'`i'`z' modelb`treattype'`x'`i'`z' modelc`treattype'`x'`i'`z', yscale(noline) ylab(,notick) ///
				text(`texty' -`textx' "More inclined to oppose",size(vsmall)) text(`texty' `textx' "More inclined to support",size(vsmall)) ///
				keep(`coef') xline(0, lpattern(dash) lcolor(red)) xtitle(`axistitle', size(small)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  ///
				legend(pos(12) row(1) order(2 "Model 1: Base model" 4 "Model 2: Model 1 + covariates" 6 "Model 3: Model 2 + cognitive controls") size(vsmall)) `yscale' ///
				subtitle("`subtitle'", size(small)) note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) saving("$fig\\`fignm'.gph", replace)
				gr export "$fig\\`fignm'.png", replace 
			}
		}
	}
}

* storing pvalues
foreach mod in a b c {
	foreach x in oprobit ologit {
		foreach y in mlte {
			foreach z in crt all {
				use `res`mod'`y'`x'1`z'', clear 
				forval i=2/13 {
					append using `res`mod'`y'`x'`i'`z''
				}
				save "$temp\DK_`y'_`x'_model`mod'_`z'.dta", replace 
			}
		}
	}
}

* COMBINE GRAPHS
foreach y in mlte {
	foreach z in crt all {
		forval i = 1/13 {
			grc1leg2  "$fig\DK`i'_`y'_ologit_`z'_unadjusted.gph" "$fig\DK`i'_`y'_support_`z'_unadjusted.gph", row(1) pos(12) ///
			notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 uses PDS Lasso.", size(tiny)) ///
			plotr(margin(zero))
			gr export "$fig\DK`i'_`y'_combined_`z'_unadjusted.png", replace
		}
	}
}

* ==== BALANCE TEST ==== *
** prep variables 
setupdata

* balance test
loc basechar i.region urban male age unmarried edu* hhhead_female nosocast hhsize
ritest lfCB e(F),  reps(500)  : reg lfCB `basechar'
