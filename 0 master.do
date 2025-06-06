* ==== PRELIMINARIES ==== *

* clear environment
capture log close
clear all 
set more off

* directory
if "`c(username)'"=="User" {
	gl basedir "H:"
} 
else if "`c(username)'"=="azzah" {
	gl basedir "G:"
}
else if "`c(username)'"=="elgha" {
	gl basedir "G:"
}

* globals
cd "$basedir\Shared drives\Projects\2025\Orwell\Breadcrumbs\10 Quantitative Narrative Testing\9 Main survey"
gl ipt "2a input"
gl temp "2b temp"
gl opt "2c output"
gl lg "3 log"
gl fig "4 figures"
gl tbl "5 tables"
gl codedir "C:\Users\\`c(username)'\Documents\GitHub\orwell"

* set data date
gl date "20250304"

* ==== CODES ==== *
// do "$codedir\2 data preparation.do"
// do "$codedir\3 analysis.do"
