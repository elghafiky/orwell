/*******************************************************************************
			  ORWELL NARRATIVE TESTING HETEROGENEITY ANALYSIS							   
*******************************************************************************/
* ==== PRELIMINARIES ==== *
* clear environment
capture log close
clear all 
set more off

* ==== PROGRAMS ==== *
* install once, then turn it off
// net describe ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
// net install ritest 
// net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace
// ssc install ivreg2, replace
// ssc install ranktest, replace
// ssc install coefplot, replace
// ssc install regsave, replace

*** GENERAL DATA SETUP
pro setupdatagen 
	* load data
	use "$ipt\processed_$date.dta", clear
	drop if lfCB==4

	* set scheme
	set scheme plotplainblind
	
	* globals for estimations
	gl seednum 859687378
	gl bstraprep 1000
	gl cond if crt_intrpt_msg==1
	gl uncond
	gl seest robust
	
	* heterogeneity analysis variables
	g west=region==1
	g colgrad=edlvl==4
	qui sum sdbi, d
	g sdbi_high=sdbi>`r(p50)'
	gl hetvars west urban male colgrad nosocast sdbi_high
end

*** SETUP DATA FOR DK ANALYSIS
pro setupdataDK
	setupdatagen
	drop if lfCB==4
	
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
		replace `x'2=. if inlist(lfCB,2,4)
		replace `x'3=. if inlist(lfCB,1,4,5)
		replace `x'4=. if inlist(lfCB,5)
		replace `x'5=. if inlist(lfCB,2,4,5)
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
	foreach y in $hetvars {
		foreach x in treat {
			gl `y'`x'comb1 `y'##`x'1 `y'##`x'2 `y'##`x'5
			gl `y'`x'comb2 `y'##`x'2 `y'##`x'3 
			gl `y'`x'comb3 `y'##`x'1 `y'##`x'2 `y'##`x'3 
			gl `y'`x'comb4 `y'##`x'1 `y'##`x'5 
			gl `y'`x'comb5 `y'##`x'1 `y'##`x'2  
			gl `y'`x'comb6 `y'##`x'1 `y'##`x'3 `y'##`x'5  
			gl `y'`x'comb7 `y'##`x'1 `y'##`x'3 

			gl `y'`x'intr1 1.`y'#1.`x'1 1.`y'#1.`x'2 1.`y'#1.`x'5
			gl `y'`x'intr2 1.`y'#1.`x'2 1.`y'#1.`x'3 
			gl `y'`x'intr3 1.`y'#1.`x'1 1.`y'#1.`x'2 1.`y'#1.`x'3 
			gl `y'`x'intr4 1.`y'#1.`x'1 1.`y'#1.`x'5 
			gl `y'`x'intr5 1.`y'#1.`x'1 1.`y'#1.`x'2  
			gl `y'`x'intr6 1.`y'#1.`x'1 1.`y'#1.`x'3 1.`y'#1.`x'5  
			gl `y'`x'intr7 1.`y'#1.`x'1 1.`y'#1.`x'3 
		}
		
		gl linoutcome support
		foreach x in lin {
			gl `y'`x'eq1 ${`x'outcome}1 ${`y'treatcomb1}
			gl `y'`x'eq2 ${`x'outcome}2 ${`y'treatcomb6}
			gl `y'`x'eq3 ${`x'outcome}3 ${`y'treatcomb2}
			gl `y'`x'eq4 ${`x'outcome}4 ${`y'treatcomb3}
			gl `y'`x'eq5 ${`x'outcome}5 ${`y'treatcomb7}
			gl `y'`x'eq6 ${`x'outcome}6 ${`y'treatcomb4}
			gl `y'`x'eq7 ${`x'outcome}7 ${`y'treatcomb4}
			gl `y'`x'eq8 ${`x'outcome}8 ${`y'treatcomb5}
			gl `y'`x'eq9 ${`x'outcome}9 ${`y'treatcomb2}
			gl `y'`x'eq10 ${`x'outcome}10 ${`y'treatcomb4}
			gl `y'`x'eq11 ${`x'outcome}11 ${`y'treatcomb5}
			gl `y'`x'eq12 ${`x'outcome}12 ${`y'treatcomb2}
			gl `y'`x'eq13 ${`x'outcome}13 ${`y'treatcomb2}
		}
	}
end

*** SETUP DATA FOR CB05 ANALYSIS
pro setupdataCB05
	setupdatagen
	drop if lfCB==4
	
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
	foreach y in $hetvars {
		foreach x in treat  {
			gl `y'`x'comb1 `y'##`x'1 `y'##`x'5
			gl `y'`x'comb2 `y'##`x'3 `y'##`x'5 
			gl `y'`x'comb3 `y'##`x'2 
			gl `y'`x'comb4 `y'##`x'1 `y'##`x'3 `y'##`x'5  
			gl `y'`x'comb5 `y'##`x'3 `y'##`x'5 
			gl `y'`x'comb6 `y'##`x'2 `y'##`x'5 
			gl `y'`x'comb7 `y'##`x'2 `y'##`x'3 `y'##`x'5 

			gl `y'`x'intr1 1.`y'#1.`x'1 1.`y'#1.`x'5
			gl `y'`x'intr2 1.`y'#1.`x'3 1.`y'#1.`x'5 
			gl `y'`x'intr3 1.`y'#1.`x'2 
			gl `y'`x'intr4 1.`y'#1.`x'1 1.`y'#1.`x'3 1.`y'#1.`x'5  
			gl `y'`x'intr5 1.`y'#1.`x'3 1.`y'#1.`x'5 
			gl `y'`x'intr6 1.`y'#1.`x'2 1.`y'#1.`x'5 
			gl `y'`x'intr7 1.`y'#1.`x'2 1.`y'#1.`x'3 1.`y'#1.`x'5 
		}
		
		gl linoutcome `varprefix'_cloned
		foreach x in lin {
			gl `y'`x'eq3 ${`x'outcome}3 ${`y'treatcomb1} // c 
			gl `y'`x'eq5 ${`x'outcome}5 ${`y'treatcomb2} // e
			gl `y'`x'eq6 ${`x'outcome}6 ${`y'treatcomb3} // f
			gl `y'`x'eq7 ${`x'outcome}7 ${`y'treatcomb4} // g
			gl `y'`x'eq8 ${`x'outcome}8 ${`y'treatcomb4} // h
			gl `y'`x'eq9 ${`x'outcome}9 ${`y'treatcomb4} // i
			gl `y'`x'eq10 ${`x'outcome}10 ${`y'treatcomb5} // j
			gl `y'`x'eq11 ${`x'outcome}11 ${`y'treatcomb6} // k
			gl `y'`x'eq12 ${`x'outcome}12 ${`y'treatcomb7} // l
		}
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
	replace sig = "p < .01" if pwyoung < 0.01
	replace sig = "p < .05"  if pwyoung >= 0.01 & pwyoung < 0.05
	replace sig = "p < .1"   if pwyoung >= 0.05 & pwyoung < 0.1
	g treat=subinstr(familyp,"treat","Treatment ",.)
	replace treat=subinstr(treat,"comply","",.)
	replace treat="Treatment 4" if inlist(familyp,"treat5","complytreat5")
	replace treat="Treatment 5" if inlist(familyp,"treat4","complytreat4")
	rename equation eqnum
	clonevar equation=eqnum
	tostring equation, replace 
	replace equation = "Model " + equation
	g narnm="Fix the distribution" if treat=="Treatment 1"
	replace narnm="No victimization" if treat=="Treatment 2"
	replace narnm="Balanced development" if treat=="Treatment 3"
	replace narnm="Equal opportunity" if treat=="Treatment 4"
	replace narnm="Gov. is ruler" if treat=="Treatment 5"
	// egen treateq = concat(treat equation), punct(:)
	egen treateq = concat(narnm equation), punct(:)
end 

* ==== POLICY SUPPORT: WESTFALL YOUNG ==== *
foreach sampresc in uncond {
	foreach het in $hetvars {		

		*** OLS AND PD LASSO
		* open data
		setupdataDK
		est clear
	
		* conduct simple linear regressions to obtain df per outcome
		forval i = 1/$numvar {
			qui reg ${`het'lineq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(df_r)'
		}

		* wyoung procedure 
		wyoung, cmd ("reg ${`het'lineq1} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq1} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq1} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq2} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq2} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq2} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq3} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq3} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq4} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq4} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq4} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq5} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq5} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq6} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq6} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq7} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq7} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq8} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq8} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq9} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq9} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq10} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq10} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq11} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq11} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq12} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq12} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq13} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq13} ${`sampresc'}, $seest") ///
		familyp(${`het'treatintr1} /// pol1
		${`het'treatintr6} /// pol2 
		${`het'treatintr2} /// pol3
		${`het'treatintr3} /// pol4
		${`het'treatintr7} /// pol5
		${`het'treatintr4} /// pol6
		${`het'treatintr4} /// pol7
		${`het'treatintr5} /// pol8
		${`het'treatintr2} /// pol9
		${`het'treatintr4} /// pol10
		${`het'treatintr5} /// pol11
		${`het'treatintr2} /// pol12
		${`het'treatintr2}) /// pol13
		bootstraps($bstraprep) seed($seednum) replace 

		* tidy wyoung result
		g equation=1
		g df=.
		forval i=1/$numvar {
			replace df=`df`i'' if outcome=="$linoutcome`i'"
		}
		plotprep
		loc filenm "$temp\DK_wyoung_linear_`het'_`sampresc'"
		save "`filenm'.dta", replace
	}

	loc first_hetvars = word("$hetvars",1)
	use "$temp\DK_wyoung_linear_`first_hetvars'_`sampresc'.dta", clear
	loc rest_hetvars = subinstr("$hetvars","`first_hetvars'","",.)
	foreach het in `rest_hetvars' {
		append using "$temp\DK_wyoung_linear_`het'_`sampresc'.dta"
	}
	loc filenm "$temp\DK_wyoung_linear_allhet_`sampresc'"
	save "`filenm'.dta", replace 
	export delimited "`filenm'.csv", nolab replace
}

* ==== CB5 WYOUNG ==== *
foreach sampresc in uncond {
	foreach het in $hetvars {
	
		*** OLS and PD LASSO
		* open data
		setupdataCB05
		est clear

		* conduct simple linear regressions to obtain df per outcome
		foreach i of numlist $varnumlist {
			qui reg ${`het'lineq`i'} ${`sampresc'}, $seest
			loc df`i'=`e(df_r)'
		}

		* wyoung procedure 
		wyoung, cmd ("reg ${`het'lineq3} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq3} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq5} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq5} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq6} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq7} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq7} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq7} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq8} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq8} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq8} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq9} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq9} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq9} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq10} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq10} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq11} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq11} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq12} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq12} ${`sampresc'}, $seest" ///
		"reg ${`het'lineq12} ${`sampresc'}, $seest") ///
		familyp(${`het'treatintr1} /// 
		${`het'treatintr2} /// 
		${`het'treatintr3} /// 
		${`het'treatintr4} /// 
		${`het'treatintr4} /// 
		${`het'treatintr4} ///
		${`het'treatintr5} /// 
		${`het'treatintr6} /// 
		${`het'treatintr7}) /// 
		bootstraps($bstraprep) seed($seednum) replace

		* tidy wyoung result
		g equation=1
		g df=.
		foreach i of numlist $varnumlist {
			replace df=`df`i'' if outcome=="$linoutcome`i'"
		}
		plotprep
		loc filenm "$temp\CB05_wyoung_linear_`het'_`sampresc'"
		save "`filenm'.dta", replace
	}

	loc first_hetvars = word("$hetvars",1)
	use "$temp\CB05_wyoung_linear_`first_hetvars'_`sampresc'.dta", clear
	loc rest_hetvars = subinstr("$hetvars","`first_hetvars'","",.)
	foreach het in `rest_hetvars' {
		append using "$temp\CB05_wyoung_linear_`het'_`sampresc'.dta"
	}
	loc filenm "$temp\CB05_wyoung_linear_allhet_`sampresc'"
	save "`filenm'.dta", replace 
	export delimited "`filenm'.csv", nolab replace
}
