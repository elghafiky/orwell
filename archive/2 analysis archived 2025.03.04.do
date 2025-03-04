* ==== PRELIMINARIES ==== *
* GOAL: CONDUCT BALANCE TEST

* clear environment
capture log close
clear all 
set more off

* directory
if "`c(username)'"=="user" {
	gl basedir "H:"
}

* globals
cd "$basedir\Shared drives\Projects\2025\Orwell\Breadcrumbs\10 Quantitative Narrative Testing\9 Main survey"
gl ipt "2a input"
gl temp "2b temp"
gl opt "2c output"
gl lg "3 log"
gl fig "4 figures"
gl tbl "5 tables"

* set data date
gl date "20250304"

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

* ==== BALANCE TEST ==== *
** prep variables 
setupdata

* balance test
loc basechar i.region urban male age unmarried edu* hhhead_female nosocast hhsize
ritest lfCB e(F),  reps(500)  : reg lfCB `basechar'

* ==== POLICY SUPPORT ==== *
** prep variables 
setupdata

* set scheme and covariates
set scheme plotplainblind
gl covariates i.region urban male age i.edlvl hhhead_female nosocast hhsize
gl cognitivecontrols crt_intrpt_msg pagetimeQDK read_stim_time sdbi

* OLS and PD LASSO
est clear 
foreach x in resist undecided {
	forval i = 1/13 {
		reg `x'`i' ib6.lfCB, r
		eststo modela`x'`i'
		
		dsregress `x'`i' ib6.lfCB, controls(() $covariates)
		eststo modelb`x'`i'
		
		dsregress `x'`i' ib6.lfCB, controls(($cognitivecontrols) $covariates)
		eststo modelc`x'`i'
	}
	loc legend_info - "OLS:" 1 "Model 1: Base model" - "PDS Lasso:" 2 "Model 2: Model 1 + covariates" 3 "Model 3: Model 2 + cognitive controls"
	loc axistitle "Probability opposing relative to control"
	loc graph_options keep(*.lfCB) xline(0, lpattern(dash) lcolor(red)) subtitle(,size(small)) byopts(legend(pos(12))) ///
	legend(row(2) holes(2) order(`legend_info') size(vsmall)) xtitle(`axistitle', size(small))

	coefplot modela`x'1 modelb`x'1 modelc`x'1, bylabel("Utilities price hike and" "cash transfer for all") ///
	|| modela`x'2 modelb`x'2 modelc`x'2, bylabel("Greater gov. budget for" "mass transport") ///
	|| modela`x'3 modelb`x'3 modelc`x'3, bylabel("Greater gov. budget for" "env. friendly tech. and renewables") ///
	|| modela`x'4 modelb`x'4 modelc`x'4, bylabel("Displaced people obtain benefit" "from infra. utilization") /// 
	|| , `graph_options'
	gr export "$fig\pol`x'_indicator_1.png", replace

	coefplot modela`x'5 modelb`x'5 modelc`x'5, bylabel("Subsidy for energy-efficient" "houses and buildings") ///
	|| modela`x'6 modelb`x'6 modelc`x'6, bylabel("Greater amount of" "social assistance") ///
	|| modela`x'7 modelb`x'7 modelc`x'7, bylabel("Wider coverage of" "social assistance") ///
	|| modela`x'8 modelb`x'8 modelc`x'8, bylabel("Higher tax for" "motorized vehicle") /// 
	|| , `graph_options'
	gr export "$fig\pol`x'_indicator_2.png", replace

	coefplot modela`x'9 modelb`x'9 modelc`x'9, bylabel("Subsidy for industries to switch" "to env. friendly tech. and energy") ///
	|| modela`x'10 modelb`x'10 modelc`x'10, bylabel("Unemployment" "insurance") ///
	|| modela`x'11 modelb`x'11 modelc`x'11, bylabel("Forest clearance for" "agriculture") ///
	|| modela`x'12 modelb`x'12 modelc`x'12, bylabel("Forest clearance for" "settlement development") /// 
	|| modela`x'13 modelb`x'13 modelc`x'13, bylabel("Forest clearance for" "infrastructure development") /// 
	|| , `graph_options'
	gr export "$fig\pol`x'_indicator_3.png", replace
}

* ORDERED PROBIT & LOGIT
est clear 
foreach x in oprobit ologit {
	if "`x'"=="oprobit" {
		loc axistitle "z-score"
		loc regress_options r 
	}
	else {
		loc axistitle "Odds ratio of higher support"
		loc regress_options r or
	}
	loc legend_info 1 "Model 1: Base model" 2 "Model 2: Model 1 + covariates" 3 "Model 3: Model 2 + cognitive controls"
	loc graph_options keep(*.lfCB) xline(0, lpattern(dash) lcolor(red)) subtitle(,size(small)) byopts(legend(pos(12))) ///
	legend(row(2) order(`legend_info') size(vsmall)) xtitle(`axistitle', size(small))
	forval i=1/13 {
		`x' QDKr`i'_recoded ib6.lfCB, `regress_options'
		eststo modela`x'`i'
		
		qui dsregress QDKr`i' ib6.lfCB, controls(() $covariates)
		loc selected_controls `e(controls_sel)'
		`x' QDKr`i'_recoded ib6.lfCB `selected_controls', `regress_options'
		eststo modelb`x'`i'
	
		qui dsregress QDKr`i' ib6.lfCB, controls(($cognitivecontrols) $covariates)
		loc selected_controls `e(controls_sel)'
		`x' QDKr`i'_recoded ib6.lfCB `selected_controls' $cognitivecontrols, `regress_options'
		eststo modelc`x'`i'
	}
	coefplot modela`x'1 modelb`x'1 modelc`x'1, bylabel("Utilities price hike and" "cash transfer for all") ///
	|| modela`x'2 modelb`x'2 modelc`x'2, bylabel("Greater gov. budget for" "mass transport") ///
	|| modela`x'3 modelb`x'3 modelc`x'3, bylabel("Greater gov. budget for" "env. friendly tech. and renewables") ///
	|| modela`x'4 modelb`x'4 modelc`x'4, bylabel("Displaced people obtain benefit" "from infra. utilization") /// 
	|| , `graph_options'
	gr export "$fig\polsupport_likert_`x'_1.png", replace

	coefplot modela`x'5 modelb`x'5 modelc`x'5, bylabel("Subsidy for energy-efficient" "houses and buildings") ///
	|| modela`x'6 modelb`x'6 modelc`x'6, bylabel("Greater amount of" "social assistance") ///
	|| modela`x'7 modelb`x'7 modelc`x'7, bylabel("Wider coverage of" "social assistance") ///
	|| modela`x'8 modelb`x'8 modelc`x'8, bylabel("Higher tax for" "motorized vehicle") /// 
	|| , `graph_options'
	gr export "$fig\polsupport_likert_`x'_2.png", replace

	coefplot modela`x'9 modelb`x'9 modelc`x'9, bylabel("Subsidy for industries to switch" "to env. friendly tech. and energy") ///
	|| modela`x'10 modelb`x'10 modelc`x'10, bylabel("Unemployment" "insurance") ///
	|| modela`x'11 modelb`x'11 modelc`x'11, bylabel("Forest clearance for" "agriculture") ///
	|| modela`x'12 modelb`x'12 modelc`x'12, bylabel("Forest clearance for" "settlement development") /// 
	|| modela`x'13 modelb`x'13 modelc`x'13, bylabel("Forest clearance for" "infrastructure development") /// 
	|| , `graph_options' 
	gr export "$fig\polsupport_likert_`x'_3.png", replace
}
