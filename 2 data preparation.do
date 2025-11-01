* ==== PRELIMINARIES ==== *
* GOAL: PREPARE DATA FOR ANALYSIS

* clear environment
capture log close
clear all 
set more off

* ==== DATA PREPARATION ==== *
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

* clean outlier in hhsize
replace hhsize=. if hhsize>12

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

* separate treatment group and compliance indicator
forval i = 1/6 {
	g treat`i'=lfCB==`i'
	g complytreat`i'=treat`i'*crt_intrpt_msg
}

* agreement with stimulus
g agreestim=(lfCB==6)|(inrange(CB04,4,6) & (inrange(lfCB,1,3) | lfCB==5))|(inrange(CB04,1,3) & lfCB==4)

* save data
save "$ipt\processed_$date.dta", replace 
export delimited "$ipt\processed_$date.csv", replace nolab