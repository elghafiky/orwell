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

* ==== PROGRAMS ==== *
// net describe ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
// net install ritest 

* ==== BALANCE TEST ==== *
* set date
gl date "20250225"

* load data
use "$ipt\raw_$date.dta", clear 

** prep variables 

* region
clonevar region=ID01
recode region (1/16 = 1) (20/21=1) (17/19=2) (22/30=2) (31/38=3)
qui sum region
forval i = 1/`r(max)' {
	g region`i'=region==`i'
}

* urban
g urban=ID02==1

* male
g male=ID03==1

* unmarried
g unmarried=ID05==1

* education
qui sum ID06
forval i=1/`r(max)' {
	g edu`i'=ID06==`i'
}

* gender household head
g hhhead_female=RT01==2

* social assistance
g nosocast=RT02==3

* household size 
egen hhsize=rowtotal(RT03*)

* balance test
loc basechar region1-region3 urban male ID04 unmarried edu* hhhead_female nosocast hhsize
ritest lfCB e(F),  reps(500)  : reg lfCB `basechar'