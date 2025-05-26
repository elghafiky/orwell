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
	use "$temp\processed_$date.dta", clear 

	* set scheme
	set scheme plotplainblind
	
	* globals for estimations
	gl seednum 859687378
	gl bstraprep 1000
	gl cond if crt_intrpt_msg==1
	gl uncond
	gl seest robust
	
	gl covariates i.region urban male age i.edlvl hhhead_female nosocast hhsize
	gl basecogctrl read_stim_time sdbi agreestim crt_intrpt_msg
	gl covarset1 controls(() $covariates)
	gl covarset2 controls(($cognitivecontrols) $covariates)
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
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'2 `x'5
		gl `x'comb2 `x'2 `x'3 
		gl `x'comb3 `x'1 `x'2 `x'3 `x'4 
		gl `x'comb4 `x'1 `x'2 `x'3  
		gl `x'comb5 `x'1 `x'5 
		gl `x'comb6 `x'1 `x'2  
	}

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
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'5
		gl `x'comb2 `x'3 `x'5 
		gl `x'comb3 `x'2 `x'4 
		gl `x'comb4 `x'1 `x'3 `x'5  
		gl `x'comb5 `x'3 `x'5 
		gl `x'comb6 `x'2 `x'4 `x'5 
		gl `x'comb7 `x'2 `x'3 `x'5 
	}
	
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

*** SETUP DATA FOR CB06 ANALYSIS
pro setupdataCB06
	setupdatagen
	loc varnm CB06 
	loc varprefix `varnm'_Orderr
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* generate binary top three indicator
		g topthree`i'=inrange(`varprefix'`i',1,3)
		
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
		
		* matching outcome with treatment
		foreach x in `varprefix'_cloned topthree {
			replace `x'`i'=. if inrange(lfCB,2,4)		
		}
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'5	
	}

	gl linoutcome topthree
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		gl `x'eq1 ${`x'outcome}1 $treatcomb1
		gl `x'eq2 ${`x'outcome}2 $treatcomb1
		gl `x'eq3 ${`x'outcome}3 $treatcomb1
		gl `x'eq4 ${`x'outcome}4 $treatcomb1
		gl `x'eq5 ${`x'outcome}5 $treatcomb1
		gl `x'eq6 ${`x'outcome}6 $treatcomb1
		gl `x'eq7 ${`x'outcome}7 $treatcomb1
		gl `x'eq8 ${`x'outcome}8 $treatcomb1
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
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'2 `x'5
		gl `x'comb2 `x'2 `x'3 `x'4
		gl `x'comb3 `x'2 `x'4 
		gl `x'comb4 `x'1 `x'5 
		gl `x'comb5 `x'1 `x'4 `x'5  
	}
	
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
	foreach x in treat complytreat {
		gl `x'comb1 `x'4
	}
	
	gl linoutcome agree
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		forval i = 1/$numvar {	
			gl `x'eq`i' ${`x'outcome}`i' $treatcomb1 
		}
	}
end

*** SETUP DATA FOR CB11 ANALYSIS
pro setupdataCB11
	setupdatagen
	loc varnm CB11 
	loc varprefix `varnm'r
	qui ds `varprefix'*
	gl numvar: word count `r(varlist)'
	forval i = 1/$numvar {
		* generate binary top three indicator
		g bigrole`i'=inrange(`varprefix'`i',4,5)
		
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
		
		* matching outcome with treatment
		foreach x in `varprefix'_cloned bigrole {
			replace `x'`i'=. if inrange(lfCB,2,4)		
		}
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetime`varnm' $basecogctrl
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'5
	}
	
	gl linoutcome bigrole
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		gl `x'eq1 ${`x'outcome}1 $treatcomb1
		gl `x'eq2 ${`x'outcome}2 $treatcomb1
		gl `x'eq3 ${`x'outcome}3 $treatcomb1
		gl `x'eq4 ${`x'outcome}4 $treatcomb1
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
		* high personal efficacy
		if `i'==3 {
			g higheff`i'=inrange(`varprefix'`i',6,8)
		}
		else {
			g higheff`i'=inrange(`varprefix'`i',4,5)
		}
	
		* recode and clone
		clonevar `varprefix'_cloned`i'=`varprefix'`i'
	}
	
	* matching outcome with treatment
	foreach x in `varprefix'_cloned higheff {
		replace `x'1=. if inlist(lfCB,2,3,5)
		replace `x'2=. if inlist(lfCB,2,3,5)
		replace `x'3=. if inlist(lfCB,2,3,5)
		replace `x'4=. if inrange(lfCB,1,3)
		replace `x'5=. if inrange(lfCB,1,3)
	}
	
	*** SET EQUATIONS
	gl cognitivecontrols pagetimeinfo`varnm' $basecogctrl
	foreach x in treat complytreat {
		gl `x'comb1 `x'1 `x'4
		gl `x'comb2 `x'4 `x'5
	}
	
	gl linoutcome higheff
	gl proboutcome `varprefix'_cloned
	foreach x in lin prob {
		gl `x'eq1 ${`x'outcome}1 $treatcomb1
		gl `x'eq2 ${`x'outcome}2 $treatcomb1
		gl `x'eq3 ${`x'outcome}3 $treatcomb1
		gl `x'eq4 ${`x'outcome}4 $treatcomb2
		gl `x'eq5 ${`x'outcome}5 $treatcomb2
	}
end

*** DATA PREP FOR PLOTTING
pro plotprep 	
	g lci=coef-1.96*stderr
	g uci=coef+1.96*stderr
	loc strtyp: type outcome
	loc strlen=substr("`strtyp'",4,2)
	g outnum=substr(outcome,-1,.) 
	replace outnum=substr(outcome,-2,.) if strlen(outcome)==`strlen'
	destring outnum, replace ignore("`c(alpha)' `c(ALPHA)'")
	g sig = "Null"
	replace sig = "***" if pwyoung < 0.01
	replace sig = "**"  if pwyoung >= 0.01 & pwyoung < 0.05
	replace sig = "*"   if pwyoung >= 0.05 & pwyoung < 0.1
	g treat=subinstr(familyp,"treat","Treatment ",.)
	replace treat=subinstr(treat,"comply","",.)
	replace treat="Treatment 4" if inlist(familyp,"treat5","complytreat5")
	replace treat="Treatment 5" if inlist(familyp,"treat4","complytreat4")
	rename equation eqnum
	clonevar equation=eqnum
	tostring equation, replace 
	replace equation = "Model " + equation
	egen treateq = concat(treat equation), punct(:)
end 

*******************
******* ITT *******
*******************

* ==== POLICY SUPPORT: WESTFALL YOUNG ==== *
foreach sampresc in uncond {
	*** OLS AND PD LASSO

	** model 1
	* open data
	setupdataDK
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq9 ${`sampresc'}, $seest" ///
	"reg $lineq9 ${`sampresc'}, $seest" ///
	"reg $lineq10 ${`sampresc'}, $seest" ///
	"reg $lineq10 ${`sampresc'}, $seest" ///
	"reg $lineq11 ${`sampresc'}, $seest" ///
	"reg $lineq11 ${`sampresc'}, $seest" ///
	"reg $lineq12 ${`sampresc'}, $seest" ///
	"reg $lineq12 ${`sampresc'}, $seest" ///
	"reg $lineq13 ${`sampresc'}, $seest" ///
	"reg $lineq13 ${`sampresc'}, $seest") ///
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
	save "$temp\DK_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataDK
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq9 ${covarset`j'_select9} ${`sampresc'}, $seest" ///
		"reg $lineq9 ${covarset`j'_select9} ${`sampresc'}, $seest" ///
		"reg $lineq10 ${covarset`j'_select10} ${`sampresc'}, $seest" ///
		"reg $lineq10 ${covarset`j'_select10} ${`sampresc'}, $seest" ///
		"reg $lineq11 ${covarset`j'_select11} ${`sampresc'}, $seest" ///
		"reg $lineq11 ${covarset`j'_select11} ${`sampresc'}, $seest" ///
		"reg $lineq12 ${covarset`j'_select12} ${`sampresc'}, $seest" ///
		"reg $lineq12 ${covarset`j'_select12} ${`sampresc'}, $seest" ///
		"reg $lineq13 ${covarset`j'_select13} ${`sampresc'}, $seest" ///
		"reg $lineq13 ${covarset`j'_select13} ${`sampresc'}, $seest") ///
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
		save "$temp\DK_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {
	** tidy all wyoung result for plotting
	use "$temp\DK_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\DK_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "DK_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit {
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
			qui `x' ${probeq`i'} ${`sampresc'}, `regress_options'
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq7 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq7 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq8 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq8 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq9 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq9 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq10 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq10 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq11 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq11 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq12 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq12 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq13 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq13 ${`sampresc'}, `regress_options'") ///
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
		save "$temp\DK_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataDK
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq7 ${covarset`j'_select7} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq7 ${covarset`j'_select7} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq8 ${covarset`j'_select8} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq8 ${covarset`j'_select8} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq9 ${covarset`j'_select9} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq9 ${covarset`j'_select9} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq10 ${covarset`j'_select10} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq10 ${covarset`j'_select10} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq11 ${covarset`j'_select11} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq11 ${covarset`j'_select11} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq12 ${covarset`j'_select12} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq12 ${covarset`j'_select12} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq13 ${covarset`j'_select13} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq13 ${covarset`j'_select13} ${`sampresc'}, `regress_options'") ///
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
			save "$temp\DK_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	foreach x in ologit {
		** tidy all wyoung result for plotting
		use "$temp\DK_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\DK_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "DK_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

* ==== CB5 WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataCB05
	est clear

	* conduct simple linear regressions to obtain df per outcome
	foreach i of numlist $varnumlist {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq9 ${`sampresc'}, $seest" ///
	"reg $lineq9 ${`sampresc'}, $seest" ///
	"reg $lineq9 ${`sampresc'}, $seest" ///
	"reg $lineq10 ${`sampresc'}, $seest" ///
	"reg $lineq10 ${`sampresc'}, $seest" ///
	"reg $lineq11 ${`sampresc'}, $seest" ///
	"reg $lineq11 ${`sampresc'}, $seest" ///
	"reg $lineq11 ${`sampresc'}, $seest" ///
	"reg $lineq12 ${`sampresc'}, $seest" ///
	"reg $lineq12 ${`sampresc'}, $seest" ///
	"reg $lineq12 ${`sampresc'}, $seest") ///
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
	save "$temp\CB05_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB05
		foreach i of numlist $varnumlist {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		* wyoung procedure 
		wyoung, cmd ("reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq9 ${covarset`j'_select9} ${`sampresc'}, $seest" ///
		"reg $lineq9 ${covarset`j'_select9} ${`sampresc'}, $seest" ///
		"reg $lineq9 ${covarset`j'_select9} ${`sampresc'}, $seest" ///
		"reg $lineq10 ${covarset`j'_select10} ${`sampresc'}, $seest" ///
		"reg $lineq10 ${covarset`j'_select10} ${`sampresc'}, $seest" ///
		"reg $lineq11 ${covarset`j'_select11} ${`sampresc'}, $seest" ///
		"reg $lineq11 ${covarset`j'_select11} ${`sampresc'}, $seest" ///
		"reg $lineq11 ${covarset`j'_select11} ${`sampresc'}, $seest" ///
		"reg $lineq12 ${covarset`j'_select12} ${`sampresc'}, $seest" ///
		"reg $lineq12 ${covarset`j'_select12} ${`sampresc'}, $seest" ///
		"reg $lineq12 ${covarset`j'_select12} ${`sampresc'}, $seest") ///
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
		save "$temp\CB05_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {
	** tidy all wyoung result for plotting
	use "$temp\CB05_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\CB05_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "CB05_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

* ==== CB6 WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataCB06
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq7 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest" ///
	"reg $lineq8 ${`sampresc'}, $seest") ///
	familyp($treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
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
	save "$temp\CB06_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB06
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq7 ${covarset`j'_select7} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest" ///
		"reg $lineq8 ${covarset`j'_select8} ${`sampresc'}, $seest") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
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
		save "$temp\CB06_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {	
	** tidy all wyoung result for plotting
	use "$temp\CB06_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\CB06_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "CB06_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit  {
		** model 1
		* open data
		setupdataCB06
		est clear

		* conduct unadjusted regressions to obtain df per outcome
		if "`x'"=="ologit" {
			loc regress_options $seest or
		}
		else {
			loc regress_options $seest
		}

		forval i = 1/$numvar {
			qui `x' ${probeq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq7 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq7 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq8 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq8 ${`sampresc'}, `regress_options'") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
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
		save "$temp\CB06_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataCB06
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			
			wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq7 ${covarset`j'_select7} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq7 ${covarset`j'_select7} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq8 ${covarset`j'_select8} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq8 ${covarset`j'_select8} ${`sampresc'}, `regress_options'") ///
			familyp($treatcomb1 /// 
			$treatcomb1 /// 
			$treatcomb1 /// 
			$treatcomb1 /// 
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
			save "$temp\CB06_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit  {
		** tidy all wyoung result for plotting
		use "$temp\CB06_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\CB06_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "CB06_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

* ==== CB7 WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataCB07
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest" ///
	"reg $lineq6 ${`sampresc'}, $seest") ///
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
	save "$temp\CB07_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB07
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest" ///
		"reg $lineq6 ${covarset`j'_select6} ${`sampresc'}, $seest") ///
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
		save "$temp\CB07_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {	
	** tidy all wyoung result for plotting
	use "$temp\CB07_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\CB07_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "CB07_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
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
			qui `x' ${probeq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq6 ${`sampresc'}, `regress_options'") ///
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
		save "$temp\CB07_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataCB07
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq6 ${covarset`j'_select6} ${`sampresc'}, `regress_options'") ///
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
			save "$temp\CB07_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit  {
		** tidy all wyoung result for plotting
		use "$temp\CB07_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\CB07_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "CB07_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

* ==== CB8 WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataCB08
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest") ///
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
	save "$temp\CB08_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB08
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest") ///
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
		save "$temp\CB08_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {
	** tidy all wyoung result for plotting
	use "$temp\CB08_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\CB08_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "CB08_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
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
			qui `x' ${probeq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("reg $probeq1 ${`sampresc'}, `regress_option'" ///
		"reg $probeq2 ${`sampresc'}, `regress_option'" ///
		"reg $probeq3 ${`sampresc'}, `regress_option'" ///
		"reg $probeq4 ${`sampresc'}, `regress_option'" ///
		"reg $probeq5 ${`sampresc'}, `regress_option'") ///
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
		save "$temp\CB08_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataCB08
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			wyoung, cmd ("reg $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_option'" ///
			"reg $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_option'" ///
			"reg $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_option'" ///
			"reg $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_option'" ///
			"reg $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_option'") ///
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
			save "$temp\CB08_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit {
		** tidy all wyoung result for plotting
		use "$temp\CB08_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\CB08_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "CB08_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

* ==== CB11 WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataCB11
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest") ///
	familyp($treatcomb1 /// 
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
	save "$temp\CB11_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataCB11
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest") ///
		familyp($treatcomb1 /// 
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
		save "$temp\CB11_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {	
	** tidy all wyoung result for plotting
	use "$temp\CB11_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\CB11_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "CB11_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit  {
		** model 1
		* open data
		setupdataCB11
		est clear

		* conduct unadjusted regressions to obtain df per outcome
		if "`x'"=="ologit" {
			loc regress_options $seest or
		}
		else {
			loc regress_options $seest
		}

		forval i = 1/$numvar {
			qui `x' ${probeq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_options'") ///
		familyp($treatcomb1 /// 
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
		save "$temp\CB11_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataCB11
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			
			wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_options'") ///
			familyp($treatcomb1 /// 
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
			save "$temp\CB11_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit  {
		** tidy all wyoung result for plotting
		use "$temp\CB11_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\CB11_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "CB11_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

* ==== TD WYOUNG ==== *
foreach sampresc in uncond {
	*** OLS and PD LASSO

	** model 1
	* open data
	setupdataTD
	est clear

	* conduct simple linear regressions to obtain df per outcome
	forval i = 1/$numvar {
		qui reg ${lineq`i'} ${`sampresc'}, $seest
		loc df`i'=`e(df_r)'
	}

	* wyoung procedure 
	wyoung, cmd ("reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq1 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq2 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq3 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq4 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest" ///
	"reg $lineq5 ${`sampresc'}, $seest") ///
	familyp($treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb1 /// 
	$treatcomb2 /// 
	$treatcomb2) /// 
	bootstraps($bstraprep) seed($seednum) replace

	* tidy wyoung result
	g equation=1
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\TD_wyoung_linear_model1_`sampresc'.dta", replace

	** model 2 & 3
	forval j = 1/2 {
		est clear
		setupdataTD
		forval i = 1/$numvar {
			qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
			gl covarset`j'_select`i' `e(controls_sel)'
			loc df`i'=`e(N)'
		}
		
		wyoung, cmd ("reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq1 ${covarset`j'_select1} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq2 ${covarset`j'_select2} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq3 ${covarset`j'_select3} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq4 ${covarset`j'_select4} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest" ///
		"reg $lineq5 ${covarset`j'_select5} ${`sampresc'}, $seest") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb2 /// 
		$treatcomb2) /// 
		bootstraps($bstraprep) seed($seednum) replace
		loc modelnum=`j'+1
		g equation=`modelnum'
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$linoutcome`i'"
		}
		save "$temp\TD_wyoung_linear_model`modelnum'_`sampresc'.dta", replace
	}
}

foreach sampresc in uncond {	
	** tidy all wyoung result for plotting
	use "$temp\TD_wyoung_linear_model1_`sampresc'.dta", clear
	forval i = 2/3 {
		append using "$temp\TD_wyoung_linear_model`i'_`sampresc'.dta"
	}
	plotprep 
	loc filenm "TD_wyoung_linear_allmodel_`sampresc'"
	save "$temp\\`filenm'.dta", replace
	export delimited "$temp\\`filenm'.csv", replace nolab
}

foreach sampresc in uncond {
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
			qui `x' ${probeq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(N)'
		}

		* wyoung procedure 
		wyoung, cmd ("`x' $probeq1 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq1 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq2 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq3 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq4 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_option'" ///
		"`x' $probeq5 ${`sampresc'}, `regress_option'") ///
		familyp($treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb1 /// 
		$treatcomb2 /// 
		$treatcomb2) /// 
		bootstraps($bstraprep) seed($seednum) replace

		* tidy wyoung result
		g equation=1
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$proboutcome`i'"
		}
		save "$temp\TD_wyoung_`x'_model1_`sampresc'.dta", replace

		** model 2 & 3
		forval j = 1/2 {
			est clear
			setupdataTD
			forval i = 1/$numvar {
				qui dsregress $proboutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
				gl covarset`j'_select`i' `e(controls_sel)'
				loc df`i'=`e(N)'
			}
			wyoung, cmd ("`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq1 ${covarset`j'_select1} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq2 ${covarset`j'_select2} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq3 ${covarset`j'_select3} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq4 ${covarset`j'_select4} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_option'" ///
			"`x' $probeq5 ${covarset`j'_select5} ${`sampresc'}, `regress_option'") ///
			familyp($treatcomb1 /// 
			$treatcomb1 /// 
			$treatcomb1 /// 
			$treatcomb2 /// 
			$treatcomb2) /// 
			bootstraps($bstraprep) seed($seednum) replace
			loc modelnum=`j'+1
			g equation=`modelnum'
			g df=.
			forval i=1/$numvar {
				replace df=`df`i'' if outcome=="$proboutcome`i'"
			}
			save "$temp\TD_wyoung_`x'_model`modelnum'_`sampresc'.dta", replace
		}
	}
}

foreach sampresc in uncond {
	*** ORDERED PROBIT AND LOGIT
	foreach x in ologit {
		** tidy all wyoung result for plotting
		use "$temp\TD_wyoung_`x'_model1_`sampresc'.dta", clear
		forval i = 2/3 {
			append using "$temp\TD_wyoung_`x'_model`i'_`sampresc'.dta"
		}
		plotprep 
		loc filenm "TD_wyoung_`x'_allmodel_`sampresc'"
		save "$temp\\`filenm'.dta", replace
		export delimited "$temp\\`filenm'.csv", replace nolab
	}
}

*******************
******* TOT *******
*******************

* ==== POLICY SUPPORT: WESTFALL YOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataDK
est clear

* conduct simple linear regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure
wyoung, cmd("ivreg2 support1 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support1 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support1 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 support2 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support2 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support3 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support3 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support4 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support4 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support4 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support4 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 support5 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support5 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support5 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support6 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support6 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 support7 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support7 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 support8 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support8 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support9 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support9 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support10 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support10 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 support11 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 support11 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support12 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support12 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 support13 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 support13 (complytreat3=treat3) if inlist(lfCB,3,6), ro first") ///
familyp($complytreatcomb1 /// pol1
$complytreatcomb2 /// pol2 
$complytreatcomb2 /// pol3
$complytreatcomb3 /// pol4
$complytreatcomb4 /// pol5
$complytreatcomb5 /// pol6
$complytreatcomb5 /// pol7
$complytreatcomb6 /// pol8
$complytreatcomb2 /// pol9
$complytreatcomb5 /// pol10
$complytreatcomb6 /// pol11
$complytreatcomb2 /// pol12
$complytreatcomb2) /// pol13
bootstraps($bstraprep) seed($seednum) replace 

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\DK_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataDK
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB ${`sampresc'}, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}

	* wyoung procedure
	wyoung, cmd("ivreg2 support1 ${covarset`j'_select1} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support1 ${covarset`j'_select1} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support1 ${covarset`j'_select1} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 support2 ${covarset`j'_select2} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support2 ${covarset`j'_select2} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support3 ${covarset`j'_select3} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support3 ${covarset`j'_select3} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support4 ${covarset`j'_select4} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support4 ${covarset`j'_select4} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support4 ${covarset`j'_select4} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support4 ${covarset`j'_select4} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 support5 ${covarset`j'_select5} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support5 ${covarset`j'_select5} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support5 ${covarset`j'_select5} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support6 ${covarset`j'_select6} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support6 ${covarset`j'_select6} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 support7 ${covarset`j'_select7} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support7 ${covarset`j'_select7} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 support8 ${covarset`j'_select8} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support8 ${covarset`j'_select8} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support9 ${covarset`j'_select9} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support9 ${covarset`j'_select9} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support10 ${covarset`j'_select10} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support10 ${covarset`j'_select10} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 support11 ${covarset`j'_select11} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 support11 ${covarset`j'_select11} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support12 ${covarset`j'_select12} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support12 ${covarset`j'_select12} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 support13 ${covarset`j'_select13} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 support13 ${covarset`j'_select13} (complytreat3=treat3) if inlist(lfCB,3,6), ro first") ///
	familyp($complytreatcomb1 /// pol1
	$complytreatcomb2 /// pol2 
	$complytreatcomb2 /// pol3
	$complytreatcomb3 /// pol4
	$complytreatcomb4 /// pol5
	$complytreatcomb5 /// pol6
	$complytreatcomb5 /// pol7
	$complytreatcomb6 /// pol8
	$complytreatcomb2 /// pol9
	$complytreatcomb5 /// pol10
	$complytreatcomb6 /// pol11
	$complytreatcomb2 /// pol12
	$complytreatcomb2) /// pol13
	bootstraps($bstraprep) seed($seednum) replace 
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\DK_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\DK_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\DK_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "DK_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== CB5 WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataCB05
est clear

* conduct simple ivreg regressions to obtain df per outcome
foreach i of numlist $varnumlist {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 CB05r_cloned3 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 CB05r_cloned3 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned5 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned5 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned6 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 CB05r_cloned6 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 CB05r_cloned7 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 CB05r_cloned7 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned7 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned8 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 CB05r_cloned8 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned8 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned9 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 CB05r_cloned9 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned9 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned10 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned10 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned11 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 CB05r_cloned11 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 CB05r_cloned11 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 CB05r_cloned12 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 CB05r_cloned12 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 CB05r_cloned12 (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb2 /// 
$complytreatcomb3 /// 
$complytreatcomb4 /// 
$complytreatcomb4 /// 
$complytreatcomb4 ///
$complytreatcomb5 /// 
$complytreatcomb6 /// 
$complytreatcomb7) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
foreach i of numlist $varnumlist {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB05_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB05
	foreach i of numlist $varnumlist {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}

	* wyoung procedure 
	wyoung, cmd ("ivreg2 CB05r_cloned3 ${covarset`j'_select3} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 CB05r_cloned3 ${covarset`j'_select3} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned5 ${covarset`j'_select5} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned5 ${covarset`j'_select5} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned6 ${covarset`j'_select6} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 CB05r_cloned6 ${covarset`j'_select6} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 CB05r_cloned7 ${covarset`j'_select7} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 CB05r_cloned7 ${covarset`j'_select7} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned7 ${covarset`j'_select7} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned8 ${covarset`j'_select8} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 CB05r_cloned8 ${covarset`j'_select8} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned8 ${covarset`j'_select8} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned9 ${covarset`j'_select9} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 CB05r_cloned9 ${covarset`j'_select9} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned9 ${covarset`j'_select9} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned10 ${covarset`j'_select10} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned10 ${covarset`j'_select10} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned11 ${covarset`j'_select11} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 CB05r_cloned11 ${covarset`j'_select11} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 CB05r_cloned11 ${covarset`j'_select11} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 CB05r_cloned12 ${covarset`j'_select12} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 CB05r_cloned12 ${covarset`j'_select12} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 CB05r_cloned12 ${covarset`j'_select12} (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb2 /// 
	$complytreatcomb3 /// 
	$complytreatcomb4 /// 
	$complytreatcomb4 /// 
	$complytreatcomb4 ///
	$complytreatcomb5 /// 
	$complytreatcomb6 /// 
	$complytreatcomb7) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	foreach i of numlist $varnumlist {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB05_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB05_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB05_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "CB05_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== CB6 WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataCB06
est clear

* conduct simple ivreg regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 topthree1 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree1 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree2 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree2 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree3 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree3 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree4 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree4 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree5 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree5 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree6 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree6 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree7 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree7 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 topthree8 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 topthree8 (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 ///
$complytreatcomb1 /// 
$complytreatcomb1) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB06_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB06
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	
	* wyoung procedure
	wyoung, cmd ("ivreg2 topthree1 ${covarset`j'_select1} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree1 ${covarset`j'_select1} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree2 ${covarset`j'_select2} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree2 ${covarset`j'_select2} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree3 ${covarset`j'_select3} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree3 ${covarset`j'_select3} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree4 ${covarset`j'_select4} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree4 ${covarset`j'_select4} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree5 ${covarset`j'_select5} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree5 ${covarset`j'_select5} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree6 ${covarset`j'_select6} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree6 ${covarset`j'_select6} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree7 ${covarset`j'_select7} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree7 ${covarset`j'_select7} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 topthree8 ${covarset`j'_select8} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 topthree8 ${covarset`j'_select8} (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 ///
	$complytreatcomb1 /// 
	$complytreatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB06_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB06_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB06_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "CB06_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== CB7 WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataCB07
est clear

* conduct simple ivreg regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 agree1 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 agree1 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 agree1 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 agree2 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 agree2 (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
"ivreg2 agree2 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree3 (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
"ivreg2 agree3 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree4 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 agree4 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 agree5 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 agree5 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree5 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 agree6 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 agree6 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree6 (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb2 /// 
$complytreatcomb3 /// 
$complytreatcomb4 /// 
$complytreatcomb5 /// 
$complytreatcomb5) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB07_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB07
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	
	* wyoung procedure
	wyoung, cmd ("ivreg2 agree1 ${covarset`j'_select1} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 agree1 ${covarset`j'_select1} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 agree1 ${covarset`j'_select1} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 agree2 ${covarset`j'_select2} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 agree2 ${covarset`j'_select2} (complytreat3=treat3) if inlist(lfCB,3,6), ro first" ///
	"ivreg2 agree2 ${covarset`j'_select2} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree3 ${covarset`j'_select3} (complytreat2=treat2) if inlist(lfCB,2,6), ro first" ///
	"ivreg2 agree3 ${covarset`j'_select3} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree4 ${covarset`j'_select4} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 agree4 ${covarset`j'_select4} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 agree5 ${covarset`j'_select5} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 agree5 ${covarset`j'_select5} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree5 ${covarset`j'_select5} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 agree6 ${covarset`j'_select6} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 agree6 ${covarset`j'_select6} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree6 ${covarset`j'_select6} (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb2 /// 
	$complytreatcomb3 /// 
	$complytreatcomb4 /// 
	$complytreatcomb5 /// 
	$complytreatcomb5) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB07_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB07_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB07_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "CB07_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== CB8 WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataCB08
est clear

* conduct simple ivreg regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 agree1 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree2 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree3 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree4 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 agree5 (complytreat4=treat4) if inlist(lfCB,4,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB08_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB08
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	wyoung, cmd ("ivreg2 agree1 ${covarset`j'_select1} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree2 ${covarset`j'_select2} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree3 ${covarset`j'_select3} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree4 ${covarset`j'_select4} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 agree5 ${covarset`j'_select5} (complytreat4=treat4) if inlist(lfCB,4,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB08_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB08_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB08_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "CB08_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== CB11 WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataCB11
est clear

* conduct simple ivreg regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 bigrole1 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 bigrole1 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 bigrole2 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 bigrole2 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 bigrole3 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 bigrole3 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 bigrole4 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 bigrole4 (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\CB11_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataCB11
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	
	wyoung, cmd ("ivreg2 bigrole1 ${covarset`j'_select1} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 bigrole1 ${covarset`j'_select1} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 bigrole2 ${covarset`j'_select2} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 bigrole2 ${covarset`j'_select2} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 bigrole3 ${covarset`j'_select3} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 bigrole3 ${covarset`j'_select3} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 bigrole4 ${covarset`j'_select4} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 bigrole4 ${covarset`j'_select4} (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\CB11_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\CB11_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\CB11_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "CB11_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== TD WYOUNG ==== *
*** OLS and PD LASSO
** model 1
* open data
setupdataTD
est clear

* conduct simple ivreg regressions to obtain df per outcome
forval i = 1/$numvar {
	qui reg ${lineq`i'}, $seest
	loc df`i'=`e(df_r)'
}

* wyoung procedure 
wyoung, cmd ("ivreg2 higheff1 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 higheff1 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 higheff2 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 higheff2 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 higheff3 (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
"ivreg2 higheff3 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 higheff4 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 higheff4 (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
"ivreg2 higheff5 (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
"ivreg2 higheff5 (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
familyp($complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb1 /// 
$complytreatcomb2 /// 
$complytreatcomb2) /// 
bootstraps($bstraprep) seed($seednum) replace

* tidy wyoung result
g equation=1
g df=.
forval i=1/$numvar {
	replace df=`df`i'' if outcome=="$linoutcome`i'"
}
save "$temp\TD_wyoung_ivreg_model1.dta", replace

** model 2 & 3
forval j = 1/2 {
	est clear
	setupdataTD
	forval i = 1/$numvar {
		qui dsregress $linoutcome`i' ib6.lfCB, ${covarset`j'}
		gl covarset`j'_select`i' `e(controls_sel)'
		loc df`i'=`e(N)'
	}
	
	* wyoung procedure
	wyoung, cmd ("ivreg2 higheff1 ${covarset`j'_select1} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 higheff1 ${covarset`j'_select1} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 higheff2 ${covarset`j'_select2} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 higheff2 ${covarset`j'_select2} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 higheff3 ${covarset`j'_select3} (complytreat1=treat1) if inlist(lfCB,1,6), ro first" ///
	"ivreg2 higheff3 ${covarset`j'_select3} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 higheff4 ${covarset`j'_select4} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 higheff4 ${covarset`j'_select4} (complytreat5=treat5) if inlist(lfCB,5,6), ro first" ///
	"ivreg2 higheff5 ${covarset`j'_select5} (complytreat4=treat4) if inlist(lfCB,4,6), ro first" ///
	"ivreg2 higheff5 ${covarset`j'_select5} (complytreat5=treat5) if inlist(lfCB,5,6), ro first") ///
	familyp($complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb1 /// 
	$complytreatcomb2 /// 
	$complytreatcomb2) /// 
	bootstraps($bstraprep) seed($seednum) replace
	loc modelnum=`j'+1
	g equation=`modelnum'
	g df=.
	forval i=1/$numvar {
		replace df=`df`i'' if outcome=="$linoutcome`i'"
	}
	save "$temp\TD_wyoung_ivreg_model`modelnum'.dta", replace
}

** tidy all wyoung result for plotting
use "$temp\TD_wyoung_ivreg_model1.dta", clear
forval i = 2/3 {
	append using "$temp\TD_wyoung_ivreg_model`i'.dta"
}
plotprep 
loc filenm "TD_wyoung_ivreg_allmodel"
save "$temp\\`filenm'.dta", replace
export delimited "$temp\\`filenm'.csv", replace nolab

* ==== BALANCE TEST ==== *
** prep variables 
setupdatagen

* balance test
loc basechar i.region urban male age unmarried edu* hhhead_female nosocast hhsize
ritest lfCB e(F),  reps(500)  : reg lfCB `basechar'
