* ==== PRELIMINARIES ==== *
* GOAL: ANALYZE DATA

* clear environment
capture log close
clear all 
set more off

* ==== PROGRAMS ==== *
// net describe ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
// net install ritest 

*** GENERAL DATA SETUP
pro setupdatagen 
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
	
	* separate treatment group
	forval i = 1/6 {
		g treat`i'=lfCB==`i'
	}
	
	* agreement with stimulus
	g agreestim=(lfCB==6)|(inrange(CB04,4,6) & (inrange(lfCB,1,3) | lfCB==5))|(inrange(CB04,1,3) & lfCB==4)

	* set scheme
	set scheme plotplainblind
	
	* globals for estimations
	gl seednum 859687378
	gl bstraprep 1000
	gl sampresc if crt_intrpt_msg==1
	gl seest robust
	
	gl covariates i.region urban male age i.edlvl hhhead_female nosocast hhsize
	gl basecogctrl read_stim_time sdbi agreestim
	gl covarset1 controls(() $covariates)
	gl covarset2 controls(($cognitivecontrols) $covariates)
	
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

*** SETUP DATA FOR DK ANALYSIS
pro setupdataDK
	setupdatagen
	la def polsuplab 1 "Against" 2 "Undecided" 3 "Support"
	loc varnm QDK 
	loc varprefix `varnm'r
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* generate binary policy support/oppose/undecided indicator
		g support`i'=inrange(`varprefix'`i',4,5)
		g resist`i'=inrange(`varprefix'`i',1,2)
		g undecided`i'=`varprefix'`i'==3
		
		* recode policy support likert
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
		clonevar `varprefix'_recoded`i'=`varprefix'`i'
		recode `varprefix'_recoded`i' (2=1) (3=2) (4/5=3)
		la val `varprefix'_recoded`i' polsuplab
	}
	
	* matching outcome with treatment
	foreach x in `varprefix'_recoded `varprefix'_cloned support resist undecided {
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
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	gl treatcomb1 treat1 treat2 treat5
	gl treatcomb2 treat2 treat3 
	gl treatcomb3 treat1 treat2 treat3 treat4 
	gl treatcomb4 treat1 treat2 treat3  
	gl treatcomb5 treat1 treat5 
	gl treatcomb6 treat1 treat2  

	gl linoutcome support
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		gl `x'eq1 ${`x'outcome}1 $treatcomb1
		gl `x'eq2 ${`x'outcome}2 $treatcomb2
		gl `x'eq3 ${`x'outcome}3 $treatcomb2
		gl `x'eq4 ${`x'outcome}4 $treatcomb3
		gl `x'eq5 ${`x'outcome}5 $treatcomb4
		gl `x'eq6 ${`x'outcome}6 $treatcomb5
		gl `x'eq7 ${`x'outcome}7 $treatcomb5
		gl `x'eq8 ${`x'outcome}8 $treatcomb6
		gl `x'eq9 ${`x'outcome}9 $treatcomb2
		gl `x'eq10 ${`x'outcome}10 $treatcomb5
		gl `x'eq11 ${`x'outcome}11 $treatcomb6
		gl `x'eq12 ${`x'outcome}12 $treatcomb2
		gl `x'eq13 ${`x'outcome}13 $treatcomb2
	}
end

*** SETUP DATA FOR CB05 ANALYSIS
pro setupdataCB05
	setupdatagen
	loc varnm CB05 
	loc varprefix `varnm'r
	qui ds `varprefix'*
	gl varnumlist 3 5 6 7 8 9 10 11 12 
	foreach i of numlist $varnumlist {
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
	}
	
	* matching outcome with treatment
	foreach x in `varprefix'_cloned {
		replace `x'3=. if inrange(lfCB,2,4) // c
		replace `x'5=. if inlist(lfCB,1,2,4) // e
		replace `x'6=. if inlist(lfCB,1,3,5) // f
		replace `x'7=. if inlist(lfCB,2,4) // g
		replace `x'8=. if inlist(lfCB,2,4) // h
		replace `x'9=. if inlist(lfCB,2,4) // i
		replace `x'10=. if inlist(lfCB,1,2,4) // j
		replace `x'11=. if inlist(lfCB,1,3) // k
		replace `x'12=. if inlist(lfCB,1,4) // l
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	gl treatcomb1 treat1 treat5
	gl treatcomb2 treat3 treat5 
	gl treatcomb3 treat2 treat4 
	gl treatcomb4 treat1 treat3 treat5  
	gl treatcomb5 treat3 treat5 
	gl treatcomb6 treat2 treat4 treat5 
	gl treatcomb7 treat2 treat3 treat5 
	
	gl linoutcome `varprefix'_cloned
	foreach x in lin {
		gl `x'eq3 ${`x'outcome}3 $treatcomb1 // c 
		gl `x'eq5 ${`x'outcome}5 $treatcomb2 // e
		gl `x'eq6 ${`x'outcome}6 $treatcomb3 // f
		gl `x'eq7 ${`x'outcome}7 $treatcomb4 // g
		gl `x'eq8 ${`x'outcome}8 $treatcomb4 // h
		gl `x'eq9 ${`x'outcome}9 $treatcomb4 // i
		gl `x'eq10 ${`x'outcome}10 $treatcomb5 // j
		gl `x'eq11 ${`x'outcome}11 $treatcomb6 // k
		gl `x'eq12 ${`x'outcome}12 $treatcomb7 // l
	}
end

*** SETUP DATA FOR CB07 ANALYSIS
pro setupdataCB07
	setupdatagen
	loc varnm CB07 
	loc varprefix `varnm'r
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* generate binary agree/disagree indicator
		g agree`i'=inrange(`varprefix'`i',4,6)
		g disagree`i'=inrange(`varprefix'`i',1,3)
		
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
	}
	
	* matching outcome with treatment
	foreach x in `varprefix'_cloned agree disagree {
		replace `x'1=. if inlist(lfCB,3,4)
		replace `x'2=. if inlist(lfCB,1,5)
		replace `x'3=. if inlist(lfCB,1,3,5)
		replace `x'4=. if inrange(lfCB,2,4)
		replace `x'5=. if inrange(lfCB,2,3)
		replace `x'6=. if inrange(lfCB,2,3)
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	gl treatcomb1 treat1 treat2 treat5
	gl treatcomb2 treat2 treat3 treat4
	gl treatcomb3 treat2 treat4 
	gl treatcomb4 treat1 treat5 
	gl treatcomb5 treat1 treat4 treat5  

	gl linoutcome agree
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		gl `x'eq1 ${`x'outcome}1 $treatcomb1
		gl `x'eq2 ${`x'outcome}2 $treatcomb2
		gl `x'eq3 ${`x'outcome}3 $treatcomb3
		gl `x'eq4 ${`x'outcome}4 $treatcomb4
		gl `x'eq5 ${`x'outcome}5 $treatcomb5
		gl `x'eq6 ${`x'outcome}6 $treatcomb5
	}
end

*** SETUP DATA FOR CB08 ANALYSIS
pro setupdataCB08
	setupdatagen
	loc varnm CB08 
	loc varprefix `varnm'r
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* generate binary agree/disagree indicator
		g agree`i'=inrange(`varprefix'`i',4,6)
		g disagree`i'=inrange(`varprefix'`i',1,3)
		
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
		
		* matching outcome with treatment
		foreach x in `varprefix'_cloned agree disagree {
			replace `x'`i'=. if !inlist(lfCB,4,6)
		}
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	gl treatcomb1 treat4

	gl linoutcome agree
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		forval i = 1/$numvar {	
			gl `x'eq`i' ${`x'outcome}`i' $treatcomb1 
		}
	}
end

*** SETUP DATA FOR TD ANALYSIS
pro setupdataTD
	setupdatagen
	loc varnm TD 
	loc varprefix `varnm'0
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
		
		* matching outcome with treatment
		foreach x in `varprefix'_cloned {
			replace `x'`i'=. if !inlist(lfCB,4,6)
		}
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetimeinfo`varnm' $basecogctrl
	gl treatcomb1 treat4

	gl proboutcome `varprefix'_cloned
	foreach x in prob {
		forval i = 1/$numvar {	
			gl `x'eq`i' ${`x'outcome}`i' $treatcomb1 
		}
	}
end

*** SAVE PVALUE UNADJUSTED
pro savepval
	regsave, tstat pval
end

*** CALCULATE WYOUNG CI
pro calcci
	replace pwyoung = pwyoung-.01 if pwyoung==1
	replace pwyoung = p if pwyoung==0 & p<0.01
	g tstat_wyoung = invt(df,(pwyoung/2))
	g se_wyoung = abs(coef)/abs(tstat_wyoung)
	g lci=coef-1.96*se_wyoung
	g uci=coef+1.96*se_wyoung
	foreach x of varlist lci uci {
		replace `x'=.99 if `x'>1
		replace `x'=-.99 if `x'<-1
	}
end

*** DATA PREP FOR PLOTTING
pro plotprep 	
	loc strtyp: type outcome
	loc strlen=substr("`strtyp'",4,2)
	g outnum=substr(outcome,-1,.) 
	replace outnum=substr(outcome,-2,.) if strlen(outcome)==`strlen'
	destring outnum, replace ignore("`c(alpha)' `c(ALPHA)'")
	encode familyp, g(treatnm)
	g treat=. 
	forval i = 1/13 {
		qui levelsof familyp if outnum==`i'
		loc treatnum: word count `r(levels)'
		forval j = 1/`treatnum' {
			loc treatarm: word `j' of `r(levels)'
			replace treat=`j' if outnum==`i' & familyp=="`treatarm'"
		}
	}
	g treat_model = .
	replace treat_model = equation if treat==1
	loc j=4
	forval i=2/5 {
		replace treat_model = equation + `j' if treat==`i'
		loc j=`j'+4
	}
end 

* ==== POLICY SUPPORT: WESTFALL YOUNG ==== *
*** OLS AND PD LASSO

** model 1
* open data
setupdataDK
est clear

* conduct simple linear regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'} $sampresc, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("reg $lineq1 $sampresc, $seest" ///
"reg $lineq1 $sampresc, $seest" ///
"reg $lineq1 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq7 $sampresc, $seest" ///
"reg $lineq7 $sampresc, $seest" ///
"reg $lineq8 $sampresc, $seest" ///
"reg $lineq8 $sampresc, $seest" ///
"reg $lineq9 $sampresc, $seest" ///
"reg $lineq9 $sampresc, $seest" ///
"reg $lineq10 $sampresc, $seest" ///
"reg $lineq10 $sampresc, $seest" ///
"reg $lineq11 $sampresc, $seest" ///
"reg $lineq11 $sampresc, $seest" ///
"reg $lineq12 $sampresc, $seest" ///
"reg $lineq12 $sampresc, $seest" ///
"reg $lineq13 $sampresc, $seest" ///
"reg $lineq13 $sampresc, $seest") ///
familyp($treatcomb1 /// pol1
$treatcomb2 /// pol2 
$treatcomb2 /// pol3
$treatcomb3 /// pol4
$treatcomb4 /// pol5
$treatcomb5 /// pol6
$treatcomb5 /// pol7
$treatcomb6 /// pol8
$treatcomb2 /// pol9
$treatcomb5 /// pol10
$treatcomb6 /// pol11
$treatcomb2 /// pol12
$treatcomb2) /// pol13
bootstraps($bstraprep) seed($seednum) replace 

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\DK_wyoung_linear_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataDK
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq7 ${covarset`j'_select7} $sampresc, $seest" ///
	"reg $lineq7 ${covarset`j'_select7} $sampresc, $seest" ///
	"reg $lineq8 ${covarset`j'_select8} $sampresc, $seest" ///
	"reg $lineq8 ${covarset`j'_select8} $sampresc, $seest" ///
	"reg $lineq9 ${covarset`j'_select9} $sampresc, $seest" ///
	"reg $lineq9 ${covarset`j'_select9} $sampresc, $seest" ///
	"reg $lineq10 ${covarset`j'_select10} $sampresc, $seest" ///
	"reg $lineq10 ${covarset`j'_select10} $sampresc, $seest" ///
	"reg $lineq11 ${covarset`j'_select11} $sampresc, $seest" ///
	"reg $lineq11 ${covarset`j'_select11} $sampresc, $seest" ///
	"reg $lineq12 ${covarset`j'_select12} $sampresc, $seest" ///
	"reg $lineq12 ${covarset`j'_select12} $sampresc, $seest" ///
	"reg $lineq13 ${covarset`j'_select13} $sampresc, $seest" ///
	"reg $lineq13 ${covarset`j'_select13} $sampresc, $seest") ///
	familyp($treatcomb1 /// pol1
	$treatcomb2 /// pol2 
	$treatcomb2 /// pol3
	$treatcomb3 /// pol4
	$treatcomb4 /// pol5
	$treatcomb5 /// pol6
	$treatcomb5 /// pol7
	$treatcomb6 /// pol8
	$treatcomb2 /// pol9
	$treatcomb5 /// pol10
	$treatcomb6 /// pol11
	$treatcomb2 /// pol12
	$treatcomb2) /// pol13
	bootstraps($bstraprep) seed($seednum) replace 
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\DK_wyoung_linear_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\DK_wyoung_linear_model1.dta", clear
forval i = 2/3 {
	append using "$temp\DK_wyoung_linear_model`i'.dta"
}
calcci
plotprep 
save "$temp\DK_wyoung_linear_allmodel.dta", replace

** plot 
set scheme plotplainblind
loc texty .25
loc textx .4
loc intval 1
loc steps .25
forval i=1/$numvar {
	loc fignm "DK`i'_wyoung_linear"
	preserve
	keep if outnum==`i'
	forval j=1/5 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof treatnm
	foreach h of numlist `r(levels)' {
		loc val`h'=0
		qui sum treat_model if treatnm==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h' "Treatment `h'"
		}
	}
	twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
			(dot coef treat_model if equation==1, horizontal) ///
			(dot coef treat_model if equation==2, horizontal) ///
			(dot coef treat_model if equation==3, horizontal), ///
			xtitle("Probability relative to control", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
			legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
			yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick)  ///
			note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to support",size(vsmall)) text(`texty' `textx' "More likely to support",size(vsmall)) ///
			subtitle("Linear model", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

*** ORDERED PROBIT AND LOGIT
foreach x in ologit  {
	** model 1
	* open data
	setupdataDK
	est clear

	* conduct unadjusted regressions to obtain df per outcome
	if "`x'"=="ologit" {
		loc regress_options $seest or
	}
	else {
		loc regress_options $seest 
	}

	forval i = 1/$numvar {
		qui `x' ${probeq`i'} $sampresc, `regress_options'
		loc df`i'=`e(N)'
	}

	* wyoung procedure 
	wyoung, cmd ("`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq2 $sampresc, `regress_options'" ///
	"`x' $probeq2 $sampresc, `regress_options'" ///
	"`x' $probeq3 $sampresc, `regress_options'" ///
	"`x' $probeq3 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq6 $sampresc, `regress_options'" ///
	"`x' $probeq6 $sampresc, `regress_options'" ///
	"`x' $probeq7 $sampresc, `regress_options'" ///
	"`x' $probeq7 $sampresc, `regress_options'" ///
	"`x' $probeq8 $sampresc, `regress_options'" ///
	"`x' $probeq8 $sampresc, `regress_options'" ///
	"`x' $probeq9 $sampresc, `regress_options'" ///
	"`x' $probeq9 $sampresc, `regress_options'" ///
	"`x' $probeq10 $sampresc, `regress_options'" ///
	"`x' $probeq10 $sampresc, `regress_options'" ///
	"`x' $probeq11 $sampresc, `regress_options'" ///
	"`x' $probeq11 $sampresc, `regress_options'" ///
	"`x' $probeq12 $sampresc, `regress_options'" ///
	"`x' $probeq12 $sampresc, `regress_options'" ///
	"`x' $probeq13 $sampresc, `regress_options'" ///
	"`x' $probeq13 $sampresc, `regress_options'") ///
	familyp($treatcomb1 /// pol1
	$treatcomb2 /// pol2 
	$treatcomb2 /// pol3
	$treatcomb3 /// pol4
	$treatcomb4 /// pol5
	$treatcomb5 /// pol6
	$treatcomb5 /// pol7
	$treatcomb6 /// pol8
	$treatcomb2 /// pol9
	$treatcomb5 /// pol10
	$treatcomb6 /// pol11
	$treatcomb2 /// pol12
	$treatcomb2) /// pol13
	bootstraps($bstraprep) seed($seednum) replace

	* tidy wyoung result
	g equation=1
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$proboutcome`i'"
	}
	save "$temp\DK_wyoung_`x'_model1.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataDK
		forval i = 1/$numvar {
			qui dsregress $proboutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq2 ${covarset`j'_select2} $sampresc, `regress_options'" ///
		"`x' $probeq2 ${covarset`j'_select2} $sampresc, `regress_options'" ///
		"`x' $probeq3 ${covarset`j'_select3} $sampresc, `regress_options'" ///
		"`x' $probeq3 ${covarset`j'_select3} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq6 ${covarset`j'_select6} $sampresc, `regress_options'" ///
		"`x' $probeq6 ${covarset`j'_select6} $sampresc, `regress_options'" ///
		"`x' $probeq7 ${covarset`j'_select7} $sampresc, `regress_options'" ///
		"`x' $probeq7 ${covarset`j'_select7} $sampresc, `regress_options'" ///
		"`x' $probeq8 ${covarset`j'_select8} $sampresc, `regress_options'" ///
		"`x' $probeq8 ${covarset`j'_select8} $sampresc, `regress_options'" ///
		"`x' $probeq9 ${covarset`j'_select9} $sampresc, `regress_options'" ///
		"`x' $probeq9 ${covarset`j'_select9} $sampresc, `regress_options'" ///
		"`x' $probeq10 ${covarset`j'_select10} $sampresc, `regress_options'" ///
		"`x' $probeq10 ${covarset`j'_select10} $sampresc, `regress_options'" ///
		"`x' $probeq11 ${covarset`j'_select11} $sampresc, `regress_options'" ///
		"`x' $probeq11 ${covarset`j'_select11} $sampresc, `regress_options'" ///
		"`x' $probeq12 ${covarset`j'_select12} $sampresc, `regress_options'" ///
		"`x' $probeq12 ${covarset`j'_select12} $sampresc, `regress_options'" ///
		"`x' $probeq13 ${covarset`j'_select13} $sampresc, `regress_options'" ///
		"`x' $probeq13 ${covarset`j'_select13} $sampresc, `regress_options'") ///
		familyp($treatcomb1 /// pol1
		$treatcomb2 /// pol2 
		$treatcomb2 /// pol3
		$treatcomb3 /// pol4
		$treatcomb4 /// pol5
		$treatcomb5 /// pol6
		$treatcomb5 /// pol7
		$treatcomb6 /// pol8
		$treatcomb2 /// pol9
		$treatcomb5 /// pol10
		$treatcomb6 /// pol11
		$treatcomb2 /// pol12
		$treatcomb2) /// pol13
		bootstraps($bstraprep) seed($seednum) replace 
		loc modelnum=`j'+1
		g equation=`modelnum'
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$proboutcome`i'"
		}
		save "$temp\DK_wyoung_`x'_model`modelnum'.dta", replace
	}

	** tidy all wyoung result for plotting
	use "$temp\DK_wyoung_`x'_model1.dta", clear
	forval i = 2/3 {
		append using "$temp\DK_wyoung_`x'_model`i'.dta"
	}
	calcci
	plotprep 
	save "$temp\DK_wyoung_`x'_allmodel.dta", replace 
}

** plot 
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
	forval i=1/$numvar {
		loc fignm "DK`i'_wyoung_`x'"
		preserve
		keep if outnum==`i'
		forval j=1/5 {
			loc val`j'=0
			loc label`j' " "
		}
		qui levelsof treatnm
		foreach h of numlist `r(levels)' {
			loc val`h'=0
			qui sum treat_model if treatnm==`h',d
			loc val`h'=`r(p50)'
			if `val`h''>0 {
				loc label`h' "Treatment `h'"
			}
		}
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if equation==1, horizontal) ///
				(dot coef treat_model if equation==2, horizontal) ///
				(dot coef treat_model if equation==3, horizontal), ///
				xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick) ///
				note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
				text(`texty' -`textx' "More inclined to oppose",size(vsmall)) text(`texty' `textx' "More inclined to support",size(vsmall)) ///
				subtitle("`subtitle'", size(small)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}

*** COMBINE WYOUNG GRAPHS
forval i = 1/$numvar {
	grc1leg2  "$fig\DK`i'_wyoung_ologit.gph" "$fig\DK`i'_wyoung_linear.gph", row(1) pos(12) ///
	notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 use PDS Lasso.", size(tiny)) ///
	plotr(margin(zero))
	gr export "$fig\DK`i'_wyoung_combined.png", replace
}

*** TABLE
use "$temp\DK_wyoung_linear_allmodel.dta", clear 
g estimator=1
foreach x in ologit oprobit {
	append using "$temp\DK_wyoung_`x'_allmodel.dta"	
}
replace estimator=2 if regexm(model,"ologit")
replace estimator=3 if regexm(model,"oprobit")
la def estimatorlab 1 "Linear" 2 "Ordered logit" 3 "Ordered probit"
la val estimator estimatorlab
ren tstat_wyoung tstat

qui levelsof estimator
foreach estmtr in `r(levels)' {
	qui levelsof equation 
	foreach modnum in `r(levels)' {
		qui levelsof outnum
		foreach outcome in `r(levels)' {
			qui levelsof treatnm
			foreach treatment in `r(levels)' {
				foreach item in coef stderr pwyoung tstat df {
					qui sum `item' if outnum==`outcome' & treatnm==`treatment' & estimator==`estmtr' & equation==`modnum'
					if `r(N)'>0 {
						if "`item'"=="tstat" {
							loc `item'_o`outcome'_t`treatment'_e`estmtr'_m`modnum'= `r(mean)'
						}
						else {
							if "`item'"=="df" {
								loc decimal 0
							}
							else {
								loc decimal 4
							}
							loc `item'_o`outcome'_t`treatment'_e`estmtr'_m`modnum': di %12.`decimal'fc `r(mean)'
						}
					}
				}
				if `r(N)'>0 {
					loc star_o`outcome'_t`treatment'_e`estmtr'_m`modnum'=cond(abs(`tstat_o`outcome'_t`treatment'_e`estmtr'_m`modnum'')>2.58,"***",cond(abs(`tstat_o`outcome'_t`treatment'_e`estmtr'_m`modnum'')>1.96,"**",cond(abs(`tstat_o`outcome'_t`treatment'_e`estmtr'_m`modnum'')>1.645,"*","")))
					loc N_o`outcome' `df_o`outcome'_t`treatment'_e`estmtr'_m`modnum'' 
				}
			}
		}
		
		texdoc init "$tbl\DK_estimator`estmtr'_model`modnum'.tex", replace force
		tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
		tex \begin{tabular}{l*{13}{c}} 
		tex \hline\hline 
		tex                    	&\multicolumn{1}{c}{(1)} 
		tex 				   	&\multicolumn{1}{c}{(2)}
		tex 				   	&\multicolumn{1}{c}{(3)}
		tex 					&\multicolumn{1}{c}{(4)}
		tex 					&\multicolumn{1}{c}{(5)}
		tex 					&\multicolumn{1}{c}{(6)}
		tex 					&\multicolumn{1}{c}{(7)}
		tex 					&\multicolumn{1}{c}{(8)}
		tex 					&\multicolumn{1}{c}{(9)}
		tex 					&\multicolumn{1}{c}{(10)}
		tex 					&\multicolumn{1}{c}{(11)}
		tex 					&\multicolumn{1}{c}{(12)}
		tex 					&\multicolumn{1}{c}{(13)}\\
		tex 				   	&\multicolumn{1}{c}{DK1}
		tex 					&\multicolumn{1}{c}{DK2}
		tex 					&\multicolumn{1}{c}{DK3}
		tex 					&\multicolumn{1}{c}{DK4}
		tex 					&\multicolumn{1}{c}{DK5}
		tex 					&\multicolumn{1}{c}{DK6}
		tex 					&\multicolumn{1}{c}{DK7}
		tex 					&\multicolumn{1}{c}{DK8}
		tex 					&\multicolumn{1}{c}{DK9}
		tex 					&\multicolumn{1}{c}{DK10}
		tex 					&\multicolumn{1}{c}{DK11}
		tex 					&\multicolumn{1}{c}{DK12}
		tex 					&\multicolumn{1}{c}{DK13}\\
		tex \hline
		tex Narrative 1 		&    `coef_o1_t1_e`estmtr'_m`modnum''\sym{`star_o1_t1_e`estmtr'_m`modnum''}    
		tex						&    `coef_o2_t1_e`estmtr'_m`modnum''\sym{`star_o2_t1_e`estmtr'_m`modnum''} 		
		tex						&    `coef_o3_t1_e`estmtr'_m`modnum''\sym{`star_o3_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o4_t1_e`estmtr'_m`modnum''\sym{`star_o4_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o5_t1_e`estmtr'_m`modnum''\sym{`star_o5_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o6_t1_e`estmtr'_m`modnum''\sym{`star_o6_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o7_t1_e`estmtr'_m`modnum''\sym{`star_o7_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o8_t1_e`estmtr'_m`modnum''\sym{`star_o8_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o9_t1_e`estmtr'_m`modnum''\sym{`star_o9_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o10_t1_e`estmtr'_m`modnum''\sym{`star_o10_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o11_t1_e`estmtr'_m`modnum''\sym{`star_o11_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o12_t1_e`estmtr'_m`modnum''\sym{`star_o12_t1_e`estmtr'_m`modnum''} 
		tex						&    `coef_o13_t1_e`estmtr'_m`modnum''\sym{`star_o13_t1_e`estmtr'_m`modnum''}  \\
		tex  					&    (`stderr_o1_t1_e`estmtr'_m`modnum'')    
		tex						&    (`stderr_o2_t1_e`estmtr'_m`modnum'') 		
		tex						&    (`stderr_o3_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o4_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o5_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o6_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o7_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o8_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o9_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o10_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o11_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o12_t1_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o13_t1_e`estmtr'_m`modnum'')  \\
		tex  					&    [`pwyoung_o1_t1_e`estmtr'_m`modnum'']    
		tex						&    [`pwyoung_o2_t1_e`estmtr'_m`modnum''] 		
		tex						&    [`pwyoung_o3_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o4_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o5_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o6_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o7_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o8_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o9_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o10_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o11_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o12_t1_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o13_t1_e`estmtr'_m`modnum'']  \\
		tex Narrative 2 		&    `coef_o1_t2_e`estmtr'_m`modnum''\sym{`star_o1_t2_e`estmtr'_m`modnum''}    
		tex						&    `coef_o2_t2_e`estmtr'_m`modnum''\sym{`star_o2_t2_e`estmtr'_m`modnum''} 		
		tex						&    `coef_o3_t2_e`estmtr'_m`modnum''\sym{`star_o3_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o4_t2_e`estmtr'_m`modnum''\sym{`star_o4_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o5_t2_e`estmtr'_m`modnum''\sym{`star_o5_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o6_t2_e`estmtr'_m`modnum''\sym{`star_o6_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o7_t2_e`estmtr'_m`modnum''\sym{`star_o7_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o8_t2_e`estmtr'_m`modnum''\sym{`star_o8_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o9_t2_e`estmtr'_m`modnum''\sym{`star_o9_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o10_t2_e`estmtr'_m`modnum''\sym{`star_o10_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o11_t2_e`estmtr'_m`modnum''\sym{`star_o11_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o12_t2_e`estmtr'_m`modnum''\sym{`star_o12_t2_e`estmtr'_m`modnum''} 
		tex						&    `coef_o13_t2_e`estmtr'_m`modnum''\sym{`star_o13_t2_e`estmtr'_m`modnum''}  \\
		tex  					&    (`stderr_o1_t2_e`estmtr'_m`modnum'')    
		tex						&    (`stderr_o2_t2_e`estmtr'_m`modnum'') 		
		tex						&    (`stderr_o3_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o4_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o5_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o6_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o7_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o8_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o9_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o10_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o11_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o12_t2_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o13_t2_e`estmtr'_m`modnum'')  \\
		tex  					&    [`pwyoung_o1_t2_e`estmtr'_m`modnum'']    
		tex						&    [`pwyoung_o2_t2_e`estmtr'_m`modnum''] 		
		tex						&    [`pwyoung_o3_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o4_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o5_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o6_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o7_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o8_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o9_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o10_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o11_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o12_t2_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o13_t2_e`estmtr'_m`modnum'']  \\
		tex Narrative 3 		&    `coef_o1_t3_e`estmtr'_m`modnum''\sym{`star_o1_t3_e`estmtr'_m`modnum''}    
		tex						&    `coef_o2_t3_e`estmtr'_m`modnum''\sym{`star_o2_t3_e`estmtr'_m`modnum''} 		
		tex						&    `coef_o3_t3_e`estmtr'_m`modnum''\sym{`star_o3_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o4_t3_e`estmtr'_m`modnum''\sym{`star_o4_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o5_t3_e`estmtr'_m`modnum''\sym{`star_o5_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o6_t3_e`estmtr'_m`modnum''\sym{`star_o6_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o7_t3_e`estmtr'_m`modnum''\sym{`star_o7_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o8_t3_e`estmtr'_m`modnum''\sym{`star_o8_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o9_t3_e`estmtr'_m`modnum''\sym{`star_o9_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o10_t3_e`estmtr'_m`modnum''\sym{`star_o10_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o11_t3_e`estmtr'_m`modnum''\sym{`star_o11_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o12_t3_e`estmtr'_m`modnum''\sym{`star_o12_t3_e`estmtr'_m`modnum''} 
		tex						&    `coef_o13_t3_e`estmtr'_m`modnum''\sym{`star_o13_t3_e`estmtr'_m`modnum''}  \\
		tex  					&    (`stderr_o1_t3_e`estmtr'_m`modnum'')    
		tex						&    (`stderr_o2_t3_e`estmtr'_m`modnum'') 		
		tex						&    (`stderr_o3_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o4_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o5_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o6_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o7_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o8_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o9_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o10_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o11_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o12_t3_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o13_t3_e`estmtr'_m`modnum'')  \\
		tex  					&    [`pwyoung_o1_t3_e`estmtr'_m`modnum'']    
		tex						&    [`pwyoung_o2_t3_e`estmtr'_m`modnum''] 		
		tex						&    [`pwyoung_o3_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o4_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o5_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o6_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o7_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o8_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o9_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o10_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o11_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o12_t3_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o13_t3_e`estmtr'_m`modnum'']  \\
		tex Narrative 4 		&    `coef_o1_t4_e`estmtr'_m`modnum''\sym{`star_o1_t4_e`estmtr'_m`modnum''}    
		tex						&    `coef_o2_t4_e`estmtr'_m`modnum''\sym{`star_o2_t4_e`estmtr'_m`modnum''} 		
		tex						&    `coef_o3_t4_e`estmtr'_m`modnum''\sym{`star_o3_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o4_t4_e`estmtr'_m`modnum''\sym{`star_o4_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o5_t4_e`estmtr'_m`modnum''\sym{`star_o5_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o6_t4_e`estmtr'_m`modnum''\sym{`star_o6_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o7_t4_e`estmtr'_m`modnum''\sym{`star_o7_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o8_t4_e`estmtr'_m`modnum''\sym{`star_o8_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o9_t4_e`estmtr'_m`modnum''\sym{`star_o9_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o10_t4_e`estmtr'_m`modnum''\sym{`star_o10_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o11_t4_e`estmtr'_m`modnum''\sym{`star_o11_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o12_t4_e`estmtr'_m`modnum''\sym{`star_o12_t4_e`estmtr'_m`modnum''} 
		tex						&    `coef_o13_t4_e`estmtr'_m`modnum''\sym{`star_o13_t4_e`estmtr'_m`modnum''}  \\
		tex  					&    (`stderr_o1_t4_e`estmtr'_m`modnum'')    
		tex						&    (`stderr_o2_t4_e`estmtr'_m`modnum'') 		
		tex						&    (`stderr_o3_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o4_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o5_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o6_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o7_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o8_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o9_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o10_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o11_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o12_t4_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o13_t4_e`estmtr'_m`modnum'')  \\
		tex  					&    [`pwyoung_o1_t4_e`estmtr'_m`modnum'']    
		tex						&    [`pwyoung_o2_t4_e`estmtr'_m`modnum''] 		
		tex						&    [`pwyoung_o3_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o4_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o5_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o6_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o7_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o8_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o9_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o10_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o11_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o12_t4_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o13_t4_e`estmtr'_m`modnum'']  \\
		tex Narrative 5 		&    `coef_o1_t5_e`estmtr'_m`modnum''\sym{`star_o1_t5_e`estmtr'_m`modnum''}    
		tex						&    `coef_o2_t5_e`estmtr'_m`modnum''\sym{`star_o2_t5_e`estmtr'_m`modnum''} 		
		tex						&    `coef_o3_t5_e`estmtr'_m`modnum''\sym{`star_o3_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o4_t5_e`estmtr'_m`modnum''\sym{`star_o4_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o5_t5_e`estmtr'_m`modnum''\sym{`star_o5_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o6_t5_e`estmtr'_m`modnum''\sym{`star_o6_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o7_t5_e`estmtr'_m`modnum''\sym{`star_o7_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o8_t5_e`estmtr'_m`modnum''\sym{`star_o8_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o9_t5_e`estmtr'_m`modnum''\sym{`star_o9_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o10_t5_e`estmtr'_m`modnum''\sym{`star_o10_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o11_t5_e`estmtr'_m`modnum''\sym{`star_o11_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o12_t5_e`estmtr'_m`modnum''\sym{`star_o12_t5_e`estmtr'_m`modnum''} 
		tex						&    `coef_o13_t5_e`estmtr'_m`modnum''\sym{`star_o13_t5_e`estmtr'_m`modnum''}  \\
		tex  					&    (`stderr_o1_t5_e`estmtr'_m`modnum'')    
		tex						&    (`stderr_o2_t5_e`estmtr'_m`modnum'') 		
		tex						&    (`stderr_o3_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o4_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o5_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o6_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o7_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o8_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o9_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o10_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o11_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o12_t5_e`estmtr'_m`modnum'') 
		tex						&    (`stderr_o13_t5_e`estmtr'_m`modnum'')  \\
		tex  					&    [`pwyoung_o1_t5_e`estmtr'_m`modnum'']    
		tex						&    [`pwyoung_o2_t5_e`estmtr'_m`modnum''] 		
		tex						&    [`pwyoung_o3_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o4_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o5_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o6_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o7_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o8_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o9_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o10_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o11_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o12_t5_e`estmtr'_m`modnum''] 
		tex						&    [`pwyoung_o13_t5_e`estmtr'_m`modnum'']  \\
		tex \hline
		tex N             		&      `N_o1'        					
		tex 					&      `N_o2'         					
		tex 					&      `N_o3'         					
		tex 					&      `N_o4'         					
		tex 					&      `N_o5'         					
		tex 					&      `N_o6'         					
		tex 					&      `N_o7'         					
		tex 					&      `N_o8'         					
		tex 					&      `N_o9'         					
		tex 					&      `N_o10'        					
		tex 					&      `N_o11'        					
		tex 					&      `N_o12'        					
		tex 					&      `N_o13' \\     
		tex \hline\hline
		tex \multicolumn{14}{l}{\footnotesize Standard errors in parentheses}\\
		tex \multicolumn{14}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
		tex \end{tabular}
		texdoc close
	}
} 

* ==== CB5 WYOUNG ==== *
*** OLS and PD LASSO

** model 1
* open data
setupdataCB05
est clear

* conduct simple linear regressions to obtain df per outcome
foreach i of numlist $varnumlist {
	qui reg ${lineq`i'} $sampresc, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("reg $lineq3 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq7 $sampresc, $seest" ///
"reg $lineq7 $sampresc, $seest" ///
"reg $lineq7 $sampresc, $seest" ///
"reg $lineq8 $sampresc, $seest" ///
"reg $lineq8 $sampresc, $seest" ///
"reg $lineq8 $sampresc, $seest" ///
"reg $lineq9 $sampresc, $seest" ///
"reg $lineq9 $sampresc, $seest" ///
"reg $lineq9 $sampresc, $seest" ///
"reg $lineq10 $sampresc, $seest" ///
"reg $lineq10 $sampresc, $seest" ///
"reg $lineq11 $sampresc, $seest" ///
"reg $lineq11 $sampresc, $seest" ///
"reg $lineq11 $sampresc, $seest" ///
"reg $lineq12 $sampresc, $seest" ///
"reg $lineq12 $sampresc, $seest" ///
"reg $lineq12 $sampresc, $seest") ///
familyp($treatcomb1 /// 
$treatcomb2 /// 
$treatcomb3 /// 
$treatcomb4 /// 
$treatcomb4 /// 
$treatcomb4 ///
$treatcomb5 /// 
$treatcomb6 /// 
$treatcomb7) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
foreach i of numlist $varnumlist {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB05_wyoung_linear_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB05
	foreach i of numlist $varnumlist {
		qui dsregress $linoutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	* wyoung procedure 
	wyoung, cmd ("reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq7 ${covarset`j'_select7} $sampresc, $seest" ///
	"reg $lineq7 ${covarset`j'_select7} $sampresc, $seest" ///
	"reg $lineq7 ${covarset`j'_select7} $sampresc, $seest" ///
	"reg $lineq8 ${covarset`j'_select8} $sampresc, $seest" ///
	"reg $lineq8 ${covarset`j'_select8} $sampresc, $seest" ///
	"reg $lineq8 ${covarset`j'_select8} $sampresc, $seest" ///
	"reg $lineq9 ${covarset`j'_select9} $sampresc, $seest" ///
	"reg $lineq9 ${covarset`j'_select9} $sampresc, $seest" ///
	"reg $lineq9 ${covarset`j'_select9} $sampresc, $seest" ///
	"reg $lineq10 ${covarset`j'_select10} $sampresc, $seest" ///
	"reg $lineq10 ${covarset`j'_select10} $sampresc, $seest" ///
	"reg $lineq11 ${covarset`j'_select11} $sampresc, $seest" ///
	"reg $lineq11 ${covarset`j'_select11} $sampresc, $seest" ///
	"reg $lineq11 ${covarset`j'_select11} $sampresc, $seest" ///
	"reg $lineq12 ${covarset`j'_select12} $sampresc, $seest" ///
	"reg $lineq12 ${covarset`j'_select12} $sampresc, $seest" ///
	"reg $lineq12 ${covarset`j'_select12} $sampresc, $seest") ///
	familyp($treatcomb1 /// 
	$treatcomb2 /// 
	$treatcomb3 /// 
	$treatcomb4 /// 
	$treatcomb4 /// 
	$treatcomb4 ///
	$treatcomb5 /// 
	$treatcomb6 /// 
	$treatcomb7) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	foreach i of numlist $varnumlist {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB05_wyoung_linear_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB05_wyoung_linear_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB05_wyoung_linear_model`i'.dta"
}
calcci
plotprep 
save "$temp\CB05_wyoung_linear_allmodel.dta", replace

** plot 
set scheme plotplainblind
loc texty .25
loc textx .4
loc intval 1
loc steps .25
foreach i of numlist $varnumlist {
	loc fignm "CB05`i'_wyoung_linear"
	preserve
	keep if outnum==`i'
	forval j=1/5 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof treatnm
	foreach h of numlist `r(levels)' {
		loc val`h'=0
		qui sum treat_model if treatnm==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h' "Treatment `h'"
		}
	}
	twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
			(dot coef treat_model if equation==1, horizontal) ///
			(dot coef treat_model if equation==2, horizontal) ///
			(dot coef treat_model if equation==3, horizontal), ///
			xtitle("Probability relative to control", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
			legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
			yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick)  ///
			note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to prioritize",size(vsmall)) text(`texty' `textx' "More likely to prioritize",size(vsmall)) ///
			subtitle("Linear model", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

* ==== CB7 WYOUNG ==== *
*** OLS and PD LASSO

** model 1
* open data
setupdataCB07
est clear

* conduct simple linear regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'} $sampresc, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("reg $lineq1 $sampresc, $seest" ///
"reg $lineq1 $sampresc, $seest" ///
"reg $lineq1 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest" ///
"reg $lineq6 $sampresc, $seest") ///
familyp($treatcomb1 /// 
$treatcomb2 /// 
$treatcomb3 /// 
$treatcomb4 /// 
$treatcomb5 /// 
$treatcomb5) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB07_wyoung_linear_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB07
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest" ///
	"reg $lineq6 ${covarset`j'_select6} $sampresc, $seest") ///
	familyp($treatcomb1 /// 
	$treatcomb2 /// 
	$treatcomb3 /// 
	$treatcomb4 /// 
	$treatcomb5 /// 
	$treatcomb5) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB07_wyoung_linear_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB07_wyoung_linear_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB07_wyoung_linear_model`i'.dta"
}
calcci
plotprep 
save "$temp\CB07_wyoung_linear_allmodel.dta", replace

** plot 
set scheme plotplainblind
loc texty .25
loc textx .4
loc intval 1
loc steps .25
forval i=1/$numvar {
	loc fignm "CB07`i'_wyoung_linear"
	preserve
	keep if outnum==`i'
	forval j=1/5 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof treatnm
	foreach h of numlist `r(levels)' {
		loc val`h'=0
		qui sum treat_model if treatnm==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h' "Treatment `h'"
		}
	}
	twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
			(dot coef treat_model if equation==1, horizontal) ///
			(dot coef treat_model if equation==2, horizontal) ///
			(dot coef treat_model if equation==3, horizontal), ///
			xtitle("Probability relative to control", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
			legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
			yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick)  ///
			note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to agree",size(vsmall)) text(`texty' `textx' "More likely to agree",size(vsmall)) ///
			subtitle("Linear model", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

*** ORDERED PROBIT AND LOGIT
foreach x in ologit  {
	** model 1
	* open data
	setupdataCB07
	est clear

	* conduct unadjusted regressions to obtain df per outcome
	if "`x'"=="ologit" {
		loc regress_options $seest or
	}
	else {
		loc regress_options $seest
	}

	forval i = 1/$numvar {
		qui `x' ${probeq`i'} $sampresc, $seest
		loc df`i'=`e(N)'
	}

	* wyoung procedure 
	wyoung, cmd ("`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq1 $sampresc, `regress_options'" ///
	"`x' $probeq2 $sampresc, `regress_options'" ///
	"`x' $probeq2 $sampresc, `regress_options'" ///
	"`x' $probeq2 $sampresc, `regress_options'" ///
	"`x' $probeq3 $sampresc, `regress_options'" ///
	"`x' $probeq3 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq4 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq5 $sampresc, `regress_options'" ///
	"`x' $probeq6 $sampresc, `regress_options'" ///
	"`x' $probeq6 $sampresc, `regress_options'" ///
	"`x' $probeq6 $sampresc, `regress_options'") ///
	familyp($treatcomb1 /// 
	$treatcomb2 /// 
	$treatcomb3 /// 
	$treatcomb4 /// 
	$treatcomb5 /// 
	$treatcomb5) ///  
	bootstraps($bstraprep) seed($seednum) replace

	* tidy wyoung result
	g equation=1
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$proboutcome`i'"
	}
	save "$temp\CB07_wyoung_`x'_model1.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB07
		forval i = 1/$numvar {
			qui dsregress $proboutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq1 ${covarset`j'_select1} $sampresc, `regress_options'" ///
		"`x' $probeq2 ${covarset`j'_select2} $sampresc, `regress_options'" ///
		"`x' $probeq2 ${covarset`j'_select2} $sampresc, `regress_options'" ///
		"`x' $probeq2 ${covarset`j'_select2} $sampresc, `regress_options'" ///
		"`x' $probeq3 ${covarset`j'_select3} $sampresc, `regress_options'" ///
		"`x' $probeq3 ${covarset`j'_select3} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq4 ${covarset`j'_select4} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq5 ${covarset`j'_select5} $sampresc, `regress_options'" ///
		"`x' $probeq6 ${covarset`j'_select6} $sampresc, `regress_options'" ///
		"`x' $probeq6 ${covarset`j'_select6} $sampresc, `regress_options'" ///
		"`x' $probeq6 ${covarset`j'_select6} $sampresc, `regress_options'") ///
		familyp($treatcomb1 /// 
		$treatcomb2 /// 
		$treatcomb3 /// 
		$treatcomb4 /// 
		$treatcomb5 /// 
		$treatcomb5) /// 
		bootstraps($bstraprep) seed($seednum) replace
		loc modelnum=`j'+1
		g equation=`modelnum'
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$proboutcome`i'"
		}
		save "$temp\CB07_wyoung_`x'_model`modelnum'.dta", replace
	}

	** tidy all wyoung result for plotting
	use "$temp\CB07_wyoung_`x'_model1.dta", clear
	forval i = 2/3 {
		append using "$temp\CB07_wyoung_`x'_model`i'.dta"
	}
	calcci
	plotprep 
	save "$temp\CB07_wyoung_`x'_allmodel.dta", replace 
}

** plot 
foreach x in ologit  {
	use "$temp\CB07_wyoung_`x'_allmodel.dta", clear 
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
	forval i=1/$numvar {
		loc fignm "CB07`i'_wyoung_`x'"
		preserve
		keep if outnum==`i'
		forval j=1/5 {
			loc val`j'=0
			loc label`j' " "
		}
		qui levelsof treatnm
		foreach h of numlist `r(levels)' {
			loc val`h'=0
			qui sum treat_model if treatnm==`h',d
			loc val`h'=`r(p50)'
			if `val`h''>0 {
				loc label`h' "Treatment `h'"
			}
		}
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if equation==1, horizontal) ///
				(dot coef treat_model if equation==2, horizontal) ///
				(dot coef treat_model if equation==3, horizontal), ///
				xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick) ///
				note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
				text(`texty' -`textx' "More inclined to disagree",size(vsmall)) text(`texty' `textx' "More inclined to agree",size(vsmall)) ///
				subtitle("`subtitle'", size(small)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}

*** COMBINE WYOUNG GRAPHS
forval i = 1/$numvar {
	grc1leg2  "$fig\CB07`i'_wyoung_ologit.gph" "$fig\CB07`i'_wyoung_linear.gph", row(1) pos(12) ///
	notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 use PDS Lasso.", size(tiny)) ///
	plotr(margin(zero))
	gr export "$fig\CB07`i'_wyoung_combined.png", replace
}

* ==== CB8 WYOUNG ==== *
*** OLS and PD LASSO

** model 1
* open data
setupdataCB08
est clear

* conduct simple linear regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'} $sampresc, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("reg $lineq1 $sampresc, $seest" ///
"reg $lineq2 $sampresc, $seest" ///
"reg $lineq3 $sampresc, $seest" ///
"reg $lineq4 $sampresc, $seest" ///
"reg $lineq5 $sampresc, $seest") ///
familyp($treatcomb1 /// 
$treatcomb1 /// 
$treatcomb1 /// 
$treatcomb1 /// 
$treatcomb1) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB08_wyoung_linear_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB08
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} $sampresc, $seest" ///
	"reg $lineq2 ${covarset`j'_select2} $sampresc, $seest" ///
	"reg $lineq3 ${covarset`j'_select3} $sampresc, $seest" ///
	"reg $lineq4 ${covarset`j'_select4} $sampresc, $seest" ///
	"reg $lineq5 ${covarset`j'_select5} $sampresc, $seest") ///
	familyp($treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB08_wyoung_linear_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB08_wyoung_linear_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB08_wyoung_linear_model`i'.dta"
}
calcci
plotprep 
save "$temp\CB08_wyoung_linear_allmodel.dta", replace

** plot 
set scheme plotplainblind
loc texty .25
loc textx .4
loc intval 1
loc steps .25
forval i=1/$numvar {
	loc fignm "CB08`i'_wyoung_linear"
	preserve
	keep if outnum==`i'
	forval j=1/5 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof treatnm
	foreach h of numlist `r(levels)' {
		loc val`h'=0
		qui sum treat_model if treatnm==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h' "Treatment 4"
		}
	}
	twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
			(dot coef treat_model if equation==1, horizontal) ///
			(dot coef treat_model if equation==2, horizontal) ///
			(dot coef treat_model if equation==3, horizontal), ///
			xtitle("Probability relative to control", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
			legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
			yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick)  ///
			note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to agree",size(vsmall)) text(`texty' `textx' "More likely to agree",size(vsmall)) ///
			subtitle("Linear model", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

*** ORDERED PROBIT AND LOGIT
foreach x in ologit {
	** model 1
	* open data
	setupdataCB08
	est clear

	* conduct unadjusted regressions to obtain df per outcome
	if "`x'"=="ologit" {
		loc regress_options $seest or
	}
	else {
		loc regress_options $seest
	}

	forval i = 1/$numvar {
		qui `x' ${probeq`i'} $sampresc, $seest
		loc df`i'=`e(N)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $probeq1 $sampresc, `regress_option'" ///
	"reg $probeq2 $sampresc, `regress_option'" ///
	"reg $probeq3 $sampresc, `regress_option'" ///
	"reg $probeq4 $sampresc, `regress_option'" ///
	"reg $probeq5 $sampresc, `regress_option'") ///
	familyp($treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace

	* tidy wyoung result
	g equation=1
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$proboutcome`i'"
	}
	save "$temp\CB08_wyoung_`x'_model1.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB08
		forval i = 1/$numvar {
			qui dsregress $proboutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("reg $probeq1 ${covarset`j'_select1} $sampresc, `regress_option'" ///
		"reg $probeq2 ${covarset`j'_select2} $sampresc, `regress_option'" ///
		"reg $probeq3 ${covarset`j'_select3} $sampresc, `regress_option'" ///
		"reg $probeq4 ${covarset`j'_select4} $sampresc, `regress_option'" ///
		"reg $probeq5 ${covarset`j'_select5} $sampresc, `regress_option'") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1) /// 
		bootstraps($bstraprep) seed($seednum) replace
		loc modelnum=`j'+1
		g equation=`modelnum'
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$proboutcome`i'"
		}
		save "$temp\CB08_wyoung_`x'_model`modelnum'.dta", replace
	}

	** tidy all wyoung result for plotting
	use "$temp\CB08_wyoung_`x'_model1.dta", clear
	forval i = 2/3 {
		append using "$temp\CB08_wyoung_`x'_model`i'.dta"
	}
	calcci
	plotprep 
	save "$temp\CB08_wyoung_`x'_allmodel.dta", replace 
}

** plot 
foreach x in ologit {
	use "$temp\CB08_wyoung_`x'_allmodel.dta", clear 
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
	forval i=1/$numvar {
		loc fignm "CB08`i'_wyoung_`x'"
		preserve
		keep if outnum==`i'
		forval j=1/5 {
			loc val`j'=0
			loc label`j' " "
		}
		qui levelsof treatnm
		foreach h of numlist `r(levels)' {
			loc val`h'=0
			qui sum treat_model if treatnm==`h',d
			loc val`h'=`r(p50)'
			if `val`h''>0 {
				loc label`h' "Treatment 4"
			}
		}
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if equation==1, horizontal) ///
				(dot coef treat_model if equation==2, horizontal) ///
				(dot coef treat_model if equation==3, horizontal), ///
				xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick) ///
				note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
				text(`texty' -`textx' "More inclined to disagree",size(vsmall)) text(`texty' `textx' "More inclined to agree",size(vsmall)) ///
				subtitle("`subtitle'", size(small)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}

*** COMBINE WYOUNG GRAPHS
forval i = 1/$numvar {
	grc1leg2  "$fig\CB08`i'_wyoung_ologit.gph" "$fig\CB08`i'_wyoung_linear.gph", row(1) pos(12) ///
	notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 use PDS Lasso.", size(tiny)) ///
	plotr(margin(zero))
	gr export "$fig\CB08`i'_wyoung_combined.png", replace
}

* ==== TD WYOUNG ==== *
*** ORDERED PROBIT AND LOGIT
foreach x in ologit {
	** model 1
	* open data
	setupdataTD
	est clear

	* conduct unadjusted regressions to obtain df per outcome
	if "`x'"=="ologit" {
		loc regress_options $seest or
	}
	else {
		loc regress_options $seest
	}

	forval i = 1/$numvar {
		qui `x' ${probeq`i'} $sampresc, $seest
		loc df`i'=`e(N)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $probeq1 $sampresc, `regress_option'" ///
	"reg $probeq2 $sampresc, `regress_option'" ///
	"reg $probeq3 $sampresc, `regress_option'" ///
	"reg $probeq4 $sampresc, `regress_option'" ///
	"reg $probeq5 $sampresc, `regress_option'") ///
	familyp($treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace

	* tidy wyoung result
	g equation=1
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$proboutcome`i'"
	}
	save "$temp\TD_wyoung_`x'_model1.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataTD
		forval i = 1/$numvar {
			qui dsregress $proboutcome`i' ib6.lfCB $sampresc, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("reg $probeq1 ${covarset`j'_select1} $sampresc, `regress_option'" ///
		"reg $probeq2 ${covarset`j'_select2} $sampresc, `regress_option'" ///
		"reg $probeq3 ${covarset`j'_select3} $sampresc, `regress_option'" ///
		"reg $probeq4 ${covarset`j'_select4} $sampresc, `regress_option'" ///
		"reg $probeq5 ${covarset`j'_select5} $sampresc, `regress_option'") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1) /// 
		bootstraps($bstraprep) seed($seednum) replace
		loc modelnum=`j'+1
		g equation=`modelnum'
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$proboutcome`i'"
		}
		save "$temp\TD_wyoung_`x'_model`modelnum'.dta", replace
	}

	** tidy all wyoung result for plotting
	use "$temp\TD_wyoung_`x'_model1.dta", clear
	forval i = 2/3 {
		append using "$temp\TD_wyoung_`x'_model`i'.dta"
	}
	calcci
	plotprep 
	save "$temp\TD_wyoung_`x'_allmodel.dta", replace 
}

** plot 
foreach x in ologit {
	use "$temp\TD_wyoung_`x'_allmodel.dta", clear 
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
	forval i=1/$numvar {
		loc fignm "TD`i'_wyoung_`x'"
		preserve
		keep if outnum==`i'
		forval j=1/5 {
			loc val`j'=0
			loc label`j' " "
		}
		qui levelsof treatnm
		foreach h of numlist `r(levels)' {
			loc val`h'=0
			qui sum treat_model if treatnm==`h',d
			loc val`h'=`r(p50)'
			if `val`h''>0 {
				loc label`h' "Treatment 4"
			}
		}
		if inlist(`i',1,2) {
			loc posstance "More inclined to think a lot"
			loc negstance "More inclined to think a little"
		}
		else if `i'==3 {
			loc posstance "More inclined to be optimistic"
			loc negstance "More inclined to be pessimistic"
		}
		else {
			loc posstance "More inclined to believe huge responsibility"
			loc negstance "More inclined to believe small responsibility"
		}
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if equation==1, horizontal) ///
				(dot coef treat_model if equation==2, horizontal) ///
				(dot coef treat_model if equation==3, horizontal), ///
				xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse noline) ytitle("") ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'", notick) ///
				note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
				text(`texty' -`textx' "`negstance'",size(vsmall)) text(`texty' `textx' "`posstance'",size(vsmall)) ///
				subtitle("`subtitle'", size(small)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}

* ==== POLICY SUPPORT - UNADJUSTED ==== *
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
			notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 use PDS Lasso.", size(tiny)) ///
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
