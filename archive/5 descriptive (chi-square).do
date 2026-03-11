* ==== ADDITIONAL DESCRIPTIVE ANALYSIS ==== *
clear
log using "$lg\Orwell_Additional Descriptive Analysis", replace

use "$ipt\raw_$date.dta", clear 

	* dummy agree with stimulus
	gen CB04_d = .
		replace CB04_d = 1 if inlist(CB04, 4,5,6)
		replace CB04_d = 0 if inlist(CB04, 1,2,3)

		la def agree 1 "Agree" 0 "Disagree", modify
		la val CB04_d agree
		
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
	la def crt 1 "right" 0 "wrong", modify
		la val crt_intrpt_msg crt

	* Chi-Square Emotion & Agreement	
	foreach x in A B C D E {
		di "Group `x' - Emotion & Agreement"
		forval i = 1/9 {
		tab CB04_d CB02`x'r`i', row chi2
		}
	}
	
	sum CB02Dr* if CB04_d==1
	sum CB02Dr* if CB04_d==0
	
	* Chi-Square Emotion & Understanding
	foreach x in A B C D E {
		di "Group `x' - Emotion & Understanding"
		forval i = 1/9 {
		tab crt_intrpt_msg CB02`x'r`i', row chi2
		}
	}

	* Chi-Square Urban & Agreement
	bys lfCB: tab CB04 ID02, col chi2
	
	* Chi-Square Gender & Undestanding
	bys lfCB: tab crt_intrpt_msg ID03, col chi2
	
	
log close
translate "$lg\Orwell_Additional Descriptive Analysis.smcl" "$lg\Orwell_Additional Descriptive Analysis.pdf"


/// T-test for Significant Chi-Square Results
log using "$lg\Orwell_Descriptive Analysis (T-test)", replace

use "$ipt\raw_$date.dta", clear 

	* dummy agree with stimulus
	gen CB04_d = .
		replace CB04_d = 1 if inlist(CB04, 4,5,6)
		replace CB04_d = 0 if inlist(CB04, 1,2,3)

		la def agree 1 "Agree" 0 "Disagree", modify
		la val CB04_d agree
		
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
	la def crt 1 "right" 0 "wrong", modify
		la val crt_intrpt_msg crt
		
	* 