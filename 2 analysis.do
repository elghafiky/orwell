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
		clonevar QDKr`i'_recoded=QDKr`i'
		recode QDKr`i'_recoded (2=1) (3=2) (4/5=3)
		la val QDKr`i'_recoded polsuplab
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
	
	* set scheme and covariates
	set scheme plotplainblind
	gl covariates i.region urban male age i.edlvl hhhead_female nosocast hhsize
	gl cognitivecontrols crt_intrpt_msg pagetimeQDK read_stim_time sdbi
	
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
	keep if regexm(var,"lfCB")
end

* ==== POLICY SUPPORT ==== *
** OLS and PD LASSO
setupdata
est clear 
forval i = 1/13 {
	foreach x in support resist undecided {
		reg `x'`i' ib6.lfCB, r
		eststo modela`x'`i'
		
		preserve
		savepval
		g model="a"
		g outcome="`x'"
		g policy=`i'
		tempfile resa`x'`i'
		save `resa`x'`i'', replace 
		restore
		
		dsregress `x'`i' ib6.lfCB, controls(() $covariates)
		eststo modelb`x'`i'
		preserve
		savepval
		g model="b"
		g outcome="`x'"
		g policy=`i'
		tempfile resb`x'`i'
		save `resb`x'`i'', replace 
		restore
		
		dsregress `x'`i' ib6.lfCB, controls(($cognitivecontrols) $covariates)
		eststo modelc`x'`i'
		preserve
		savepval
		g model="c"
		g outcome="`x'"
		g policy=`i'
		tempfile resc`x'`i'
		save `resc`x'`i'', replace 
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
		
		coefplot modela`x'`i' modelb`x'`i' modelc`x'`i', text(`texty' -`textx' "Less likely to `stance'",size(vsmall)) text(`texty' `textx' "More likely to `stance'",size(vsmall)) ///
		keep(*.lfCB) xline(0, lpattern(dash) lcolor(red)) xtitle("Probability to `stance' relative to control", size(small)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval') ///
		legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 4 "Model 2: Model 1 + covariates" 6 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
		subtitle("Linear estimator", size(small)) note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) saving("$fig\DK`i'_`x'_unadjusted.gph", replace)
		gr export "$fig\DK`i'_`x'_unadjusted.png", replace 
	}
}

* storing pvalues
foreach y in a b c {
	foreach x in support resist undecided {
		use `res`y'`x'1', clear 
		forval i=2/13 {
			append using `res`y'`x'`i''
		}
		save "$temp\polsupport_`x'_indicator_model`y'.dta", replace 
	}
}

** ORDERED PROBIT & LOGIT
setupdata
est clear 
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
		`x' QDKr`i'_recoded ib6.lfCB, `regress_options'
		eststo modela`x'`i'
		preserve
		savepval
		g model="a"
		g estimator="`x'"
		g policy=`i'
		tempfile resa`x'`i'
		save `resa`x'`i'', replace 
		restore
		
		qui dsregress QDKr`i' ib6.lfCB, controls(() $covariates)
		loc selected_controls `e(controls_sel)'
		`x' QDKr`i'_recoded ib6.lfCB `selected_controls', `regress_options'
		eststo modelb`x'`i'
		preserve
		savepval
		g model="b"
		g estimator="`x'"
		g policy=`i'
		tempfile resb`x'`i'
		save `resb`x'`i'', replace 
		restore
	
		qui dsregress QDKr`i' ib6.lfCB, controls(($cognitivecontrols) $covariates)
		loc selected_controls `e(controls_sel)'
		`x' QDKr`i'_recoded ib6.lfCB `selected_controls' $cognitivecontrols, `regress_options'
		eststo modelc`x'`i'
		preserve
		savepval
		g model="c"
		g estimator="`x'"
		g policy=`i'
		tempfile resc`x'`i'
		save `resc`x'`i'', replace 
		restore
		
		loc texty .5
		loc textx .2
		loc intval .5
		loc steps .1
		
		coefplot modela`x'`i' modelb`x'`i' modelc`x'`i', text(`texty' -`textx' "More inclined to oppose",size(vsmall)) text(`texty' `textx' "More inclined to support",size(vsmall)) ///
		keep(*.lfCB) xline(0, lpattern(dash) lcolor(red)) xtitle(`axistitle', size(small)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  ///
		legend(pos(12) row(1) order(2 "Model 1: Base model" 4 "Model 2: Model 1 + covariates" 6 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
		subtitle("`subtitle'", size(small)) note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) saving("$fig\DK`i'_`x'_unadjusted.gph", replace)
		gr export "$fig\DK`i'_`x'_unadjusted.png", replace 
	}
}

* storing pvalues
foreach y in a b c {
	foreach x in oprobit ologit {
		use `res`y'`x'1', clear 
		forval i=2/13 {
			append using `res`y'`x'`i''
		}
		save "$temp\polsupport_`x'_likert_model`y'.dta", replace 
	}
}

* COMBINE GRAPHS
forval i = 1/13 {
	grc1leg2  "$fig\DK`i'_ologit_unadjusted.gph" "$fig\DK`i'_support_unadjusted.gph", row(1) pos(12) ///
	notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 uses PDS Lasso.", size(tiny)) ///
	plotr(margin(zero))
	gr export "$fig\DK`i'_combined_unadjusted.png", replace
}

* ==== BALANCE TEST ==== *
** prep variables 
setupdata

* balance test
loc basechar i.region urban male age unmarried edu* hhhead_female nosocast hhsize
ritest lfCB e(F),  reps(500)  : reg lfCB `basechar'
