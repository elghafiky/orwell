* This code generates BKY (2006) sharpened two-stage q-values as described in Anderson (2008), "Multiple Inference and Gender Differences in the Effects of Early Intervention: A Reevaluation of the Abecedarian, Perry Preschool, and Early Training Projects", Journal of the American Statistical Association, 103(484), 1481-1495

* BKY (2006) sharpened two-stage q-values are introduced in Benjamini, Krieger, and Yekutieli (2006), "Adaptive Linear Step-up Procedures that Control the False Discovery Rate", Biometrika, 93(3), 491-507

capture log close
clear all
set more off

* ==== PROGRAMS ==== *
pro qval
	* Collect the total number of p-values tested
	quietly sum pval
	local totalpvals = r(N)

	* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

	quietly gen int original_sorting_order = _n
	quietly sort pval
	quietly gen int rank = _n if pval != .

	* Set the initial counter to 1 

	local qval = 1

	* Generate the variable that will contain the BKY (2006) sharpened q-values

	gen bky06_qval = 1 if pval != .

	* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.

	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval' / ( 1 + `qval' )
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj' * rank / `totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = ( fdr_temp1 >= pval ) if pval != .
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1 * rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max( reject_rank1 )

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj' * ( `totalpvals' / ( `totalpvals' - total_rejected1[1] ) )
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st' * rank / `totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = ( fdr_temp2 >= pval ) if pval != .
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2 * rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max( reject_rank2 )

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank != .
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}


	quietly sort original_sorting_order

	display "Code has completed."
	display "Benjamini Krieger Yekutieli (2006) sharpened q-vals are in variable 'bky06_qval'"
	display	"Sorting order is the same as the original vector of p-values"

	* Note: Sharpened FDR q-vals can be LESS than unadjusted p-vals when many hypotheses are rejected, because if you have many true rejections, then you can tolerate several false rejections too (this effectively just happens for p-vals that are so large that you are not going to reject them regardless).
end

pro calcci
	replace bky06_qval = bky06_qval-.01 if bky06_qval==1
	g tstat_fromq = invt(N,(bky06_qval/2))
	g se_fromq = abs(coef)/abs(tstat_fromq)
	g lci=coef-1.96*se_fromq
	g uci=coef+1.96*se_fromq 
end

pro plotprep 
	drop if pval==.
	replace var=substr(var,-6,.)
	encode var, g(treat)
	encode model, g(mod)
	g treat_model = .
	replace treat_model = mod if treat==1
	loc j=4
	forval i=2/5 {
		replace treat_model = mod + `j' if treat==`i'
		loc j=`j'+4
	}
end 

* ==== PROCEDURE ==== *
foreach y in a b c {	
	* linear model
	foreach x in support resist undecided {
		* load pvalues data
		use "$temp\polsupport_`x'_indicator_model`y'.dta", clear 
	
		* run qvalue procedure
		qval
		
		* rave temporary file
		tempfile model`x'`y'
		save `model`x'`y'', replace 
	}

	* probability model
	foreach x in oprobit ologit {
		* load pvalues data
		use "$temp\polsupport_`x'_likert_model`y'.dta", clear 
	
		* run qvalue procedure
		qval
		
		* save temporary file
		tempfile model`x'`y'
		save `model`x'`y'', replace 
	}
}

* linear model
use `modelresista', clear
foreach x in support undecided  {
	append using `model`x'a'
}
foreach y in b c {
	foreach x in resist undecided support {
		append using `model`x'`y''
	}
}
drop if pval==.
save "$temp\polsupport_indicator_allmod.dta", replace 

* probability model
foreach x in oprobit ologit {
	use `model`x'a', clear
	foreach y in b c {
			append using `model`x'`y''
	}
	* save result
	drop if pval==.
	save "$temp\polsupport_likert_`x'.dta", replace 
}

* ==== CI BASED ON Q-VALUES - LINEAR MODEL ==== *
* load data 
use "$temp\polsupport_indicator_allmod.dta", clear

* calculate CI
calcci
foreach x of varlist lci uci {
	replace `x'=1 if `x'>1
	replace `x'=-1 if `x'<-1
}

* prep data
plotprep

* plot 
set scheme plotplainblind
loc texty .5
loc textx .4
loc intval 1
loc steps .2
qui levelsof outcome
foreach x in `r(levels)' {
	if "`x'"=="undecided" {
		loc stance "be `x'"
	}
	else {
		loc stance "`x'"
	}
	forval i = 1/13 {
		preserve
		keep if policy==`i' & outcome=="`x'"
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if model=="a", horizontal) ///
				(dot coef treat_model if model=="b", horizontal) ///
				(dot coef treat_model if model=="c", horizontal), ///
				xtitle("Probability relative to control", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(2) holes(2) order(- "OLS:" 2 "Model 1: Base model" - "PDS Lasso:" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse) ytitle("") ylabel(2 "Treatment 1" 6 "Treatment 2" 10 "Treatment 3" 14 "Treatment 4" 18 "Treatment 5") ///
				note("Note: Confidence interval crossing zero indicates a null effect." "Confidence interval >|1| has been trimmed.", size(tiny)) ///
				text(`texty' -`textx' "Less likely to `stance'",size(vsmall)) text(`texty' `textx' "More likely to `stance'",size(vsmall)) ///
				subtitle("Linear model", size(small)) saving("$fig\DK`i'_`x'_adjusted.gph", replace )
		gr export "$fig\DK`i'_`x'_adjusted.png", replace 		
		restore 
	}
}

* ==== CI BASED ON Q-VALUES - PROBABILITY MODEL ==== *
foreach x in oprobit ologit {
	* load data 
	use "$temp\polsupport_likert_`x'.dta", clear

	* calculate CI
	calcci

	* prep data
	plotprep			

	* plot 
	set scheme plotplainblind
	loc texty .5
	loc textx .4
	loc intval 1
	loc steps .2
	if "`x'"=="oprobit" {
		loc axistitle "z-score/probit index"
		loc subtitle "Ordered probit"
	}
	else {
		loc axistitle "Odds ratio relative to control"
		loc subtitle "Ordered logit"
	}
	forval i = 1/13 {
		preserve
		keep if policy==`i'
		twoway 	(rcap uci lci treat_model, horizontal lcolor(gray)) ///
				(dot coef treat_model if model=="a", horizontal) ///
				(dot coef treat_model if model=="b", horizontal) ///
				(dot coef treat_model if model=="c", horizontal), ///
				xtitle("`axistitle'", size(small)) xline(0, lpattern(dash) lcolor(red)) xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval')  /// 
				legend(pos(12) row(1) order(2 "Model 1: Base model" 3 "Model 2: Model 1 + covariates" 4 "Model 3: Model 2 + cognitive controls") size(vsmall)) ///
				yscale(reverse) ytitle("") ylabel(2 "Treatment 1" 6 "Treatment 2" 10 "Treatment 3" 14 "Treatment 4" 18 "Treatment 5") ///
				note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
				text(`texty' -`textx' "More inclined to oppose",size(vsmall)) text(`texty' `textx' "More inclined to support",size(vsmall)) ///
				subtitle("`subtitle'", size(small)) saving("$fig\DK`i'_`x'_adjusted.gph", replace )
		gr export "$fig\DK`i'_`x'_adjusted.png", replace 		
		restore 
	}
 
}

* ==== COMBINE GRAPHS ==== *
forval i = 1/13 {
	grc1leg2  "$fig\DK`i'_ologit_adjusted.gph" "$fig\DK`i'_support_adjusted.gph", row(1) pos(12) ///
	notetonote caption("Linear Model 1 uses OLS. Linear Model 2 and 3 uses PDS Lasso.", size(tiny)) ///
	plotr(margin(zero))
	gr export "$fig\DK`i'_combined_adjusted.png", replace
}