* ==== PRELIMINARIES ==== *
* GOAL: VISUALIZE REGRESSION RESULTS FOR VIRIYAENB REPORT

* clear environment
capture log close
clear all 
set more off

* ==== CB5 ==== *
** linear model
set scheme plotplainblind
use "$temp\CB05_wyoung_linear_allmodel.dta", clear
keep if (treatnm==1 & inlist(outnum,3,8,9)) | ///
		(treatnm==2 & inlist(outnum,11,12)) | ///
		(treatnm==3 & inlist(outnum,5,8)) | ///
		(treatnm==4 & inlist(outnum,6,11)) | ///
		(treatnm==5 & inlist(outnum,8,9,11,12)) 
				
la def outnumlab 1 "Making Indonesia advanced nation" ///
				 2 "Making Indonesia respected by other countries" ///
				 3 "Increasing economic capacity of all citizens" ///
				 4 "Fulfilling citizens' daily needs" ///
				 5 "Improving human resources quality" ///
				 6 "Enabling citizens to take decisions democratically" ///
				 7 "Building public infrastructure" ///
				 8 "Providing basic services" ///
				 9 "Improving underdeveloped regions" ///
				 10 "Enabling citizens to achieve their chosen goals" ///
				 11 "Guaranteeing fairness for all citizens" ///
				 12 "Ensuring the next generation has a decent life" 
la val outnum outnumlab	

g eqnum = equation
qui levelsof treatnm 
foreach k of numlist `r(levels)' {
	preserve
	keep if treatnm==`k'
	qui levelsof outnum, local(outnumlist) 
	loc second_outnum = word("`outnumlist'",2)
	loc last_outnum = substr("`outnumlist'",-2,.)
	qui distinct outnum 
	if `r(ndistinct)'>1 {
		loc j=4
		forval i = `second_outnum'/`last_outnum' {
			qui count if outnum==`i' 
			if `r(N)'>0 {
				replace eqnum = equation + `j' if outnum==`i' 
				loc j=`j'+4
			}
		}
	}

	loc fignm "CB05_treat`k'_wyoung_linear"
	qui sum lci
	loc maxlci=abs(`r(min)')
	qui sum uci
	loc maxuci=abs(`r(max)')	
	loc intval=max(`maxlci',`maxuci')
	loc steps=`intval'/4
	loc texty= 0
	loc textx=`intval'/2.5 
	qui sum eqnum
	loc scalemax=`r(max)'+1
	
	forval j=1/13 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof outnum
	foreach h of numlist `r(levels)' {
		qui sum eqnum if outnum==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h': label outnumlab `h'
		}
	}

	twoway 	(bar coef eqnum if equation==1, horizontal) ///
			(bar coef eqnum if equation==2, horizontal) ///
			(bar coef eqnum if equation==3, horizontal) ///
			(rcap uci lci eqnum, horizontal lcolor(gray)), ///
			xtitle("Probability relative to control", size(vsmall)) xline(0, lpattern(dash) lcolor(red)) ///
			xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval',format(%12.2fc) labsize(vsmall))  /// 
			legend(pos(12) row(1) order(1 "Model 1" 2 "Model 2" 3 "Model 3") size(vsmall)) ///
			yscale(range(0 `scalemax') reverse noline) ytitle("") ///
			ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'" ///
			`val6' "`label6'" `val7' "`label7'" `val8' "`label8'" `val9' "`label9'" `val10' "`label10'" ///
			`val11' "`label11'" `val12' "`label12'" `val13' "`label13'", notick labsize(vsmall) nogrid) ///
			note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to prioritize",size(vsmall)) text(`texty' `textx' "More likely to prioritize",size(vsmall)) ///
			subtitle("", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

* ==== CB7 ==== *
** linear model
set scheme plotplainblind
use "$temp\CB07_wyoung_linear_allmodel.dta", clear
				
la def outnumlab 1 "Reduce reliance on destructive econ. once society prospers" ///
				 2 "Gov. can relocate people if gov. provides replacement residence" ///
				 3 "We should not blindly accept government decision" ///
				 4 "Wealth inequality is normal in a society" ///
				 5 "Gov. laws determine people's chance for success" ///
				 6 "Gov. decisions determine who rules in the economy" 
la val outnum outnumlab	

g eqnum = equation
qui levelsof treatnm 
foreach k of numlist `r(levels)' {
	preserve
	keep if treatnm==`k'
	qui levelsof outnum, local(outnumlist) 
	loc second_outnum = word("`outnumlist'",2)
	loc last_outnum = substr("`outnumlist'",-2,.)
	qui distinct outnum 
	if `r(ndistinct)'>1 {
		loc j=4
		forval i = `second_outnum'/`last_outnum' {
			qui count if outnum==`i' 
			if `r(N)'>0 {
				replace eqnum = equation + `j' if outnum==`i' 
				loc j=`j'+4
			}
		}
	}

	loc fignm "CB07_treat`k'_wyoung_linear"
	qui sum lci
	loc maxlci=abs(`r(min)')
	qui sum uci
	loc maxuci=abs(`r(max)')	
	loc intval=max(`maxlci',`maxuci')
	loc steps=`intval'/4
	loc texty= 0
	loc textx=`intval'/2.5 
	qui sum eqnum
	loc scalemax=`r(max)'+1
	
	forval j=1/13 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof outnum
	foreach h of numlist `r(levels)' {
		qui sum eqnum if outnum==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h': label outnumlab `h'
		}
	}

	twoway 	(bar coef eqnum if equation==1, horizontal) ///
			(bar coef eqnum if equation==2, horizontal) ///
			(bar coef eqnum if equation==3, horizontal) ///
			(rcap uci lci eqnum, horizontal lcolor(gray)), ///
			xtitle("Probability relative to control", size(vsmall)) xline(0, lpattern(dash) lcolor(red)) ///
			xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval',format(%12.2fc) labsize(vsmall))  /// 
			legend(pos(12) row(1) order(1 "Model 1" 2 "Model 2" 3 "Model 3") size(vsmall)) ///
			yscale(range(0 `scalemax') reverse noline) ytitle("") ///
			ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'" ///
			`val6' "`label6'" `val7' "`label7'" `val8' "`label8'" `val9' "`label9'" `val10' "`label10'" ///
			`val11' "`label11'" `val12' "`label12'" `val13' "`label13'", notick labsize(vsmall) nogrid) ///
			note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to agree",size(vsmall)) text(`texty' `textx' "More likely to agree",size(vsmall)) ///
			subtitle("", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

* ==== CB8 ==== *
** linear model
set scheme plotplainblind
use "$temp\CB08_wyoung_linear_allmodel.dta", clear
recode treatnm (1=4)
				
la def outnumlab 1 "Ruler" ///
				 2 "Regulator" ///
				 3 "Leader" ///
				 4 "Caretaker" ///
				 5 "Referee" 
la val outnum outnumlab	

g eqnum = equation
qui levelsof treatnm 
foreach k of numlist `r(levels)' {
	preserve
	keep if treatnm==`k'
	qui levelsof outnum, local(outnumlist) 
	loc second_outnum = word("`outnumlist'",2)
	loc last_outnum = substr("`outnumlist'",-2,.)
	qui distinct outnum 
	if `r(ndistinct)'>1 {
		loc j=4
		forval i = `second_outnum'/`last_outnum' {
			qui count if outnum==`i' 
			if `r(N)'>0 {
				replace eqnum = equation + `j' if outnum==`i' 
				loc j=`j'+4
			}
		}
	}

	loc fignm "CB08_treat`k'_wyoung_linear"
	qui sum lci
	loc maxlci=abs(`r(min)')
	qui sum uci
	loc maxuci=abs(`r(max)')	
	loc intval=max(`maxlci',`maxuci')
	loc steps=`intval'/4
	loc texty= 0
	loc textx=`intval'/2.5 
	qui sum eqnum
	loc scalemax=`r(max)'+1
	
	forval j=1/13 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof outnum
	foreach h of numlist `r(levels)' {
		qui sum eqnum if outnum==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h': label outnumlab `h'
		}
	}

	twoway 	(bar coef eqnum if equation==1, horizontal) ///
			(bar coef eqnum if equation==2, horizontal) ///
			(bar coef eqnum if equation==3, horizontal) ///
			(rcap uci lci eqnum, horizontal lcolor(gray)), ///
			xtitle("Probability relative to control", size(vsmall)) xline(0, lpattern(dash) lcolor(red)) ///
			xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval',format(%12.2fc) labsize(vsmall))  /// 
			legend(pos(12) row(1) order(1 "Model 1" 2 "Model 2" 3 "Model 3") size(vsmall)) ///
			yscale(range(0 `scalemax') reverse noline) ytitle("") ///
			ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'" ///
			`val6' "`label6'" `val7' "`label7'" `val8' "`label8'" `val9' "`label9'" `val10' "`label10'" ///
			`val11' "`label11'" `val12' "`label12'" `val13' "`label13'", notick labsize(vsmall) nogrid) ///
			note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to agree",size(vsmall)) text(`texty' `textx' "More likely to agree",size(vsmall)) ///
			subtitle("", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

* ==== DK ==== *
** linear model
set scheme plotplainblind
use "$temp\DK_wyoung_linear_allmodel.dta", clear
keep if (treatnm==1 & inlist(outnum,6,7,10)) | ///
		(treatnm==2 & (inrange(outnum,2,4) | inlist(outnum,9) | inrange(outnum,11,13))) | ///
		(treatnm==3 & inlist(outnum,12,13)) | ///
		(treatnm==5 & inlist(outnum,1,6,7,10)) 
				
la def outnumlab 1 "Utility price hike alongside soc. ast." ///
				 2 "Expand gov. budget for mass transport dev." ///
				 3 "Gov. budget for env. friendly tech. and energy dev." ///
				 4 "Displaced people obtain benefit from infra. util." ///
				 5 "Subsidy for building energy-efficient houses and buildings" ///
				 6 "Enlarge social assistance amount" ///
				 7 "Expand social assistance recipient coverage" ///
				 8 "Higher motorized-vehicle tax" ///
				 9 "Env. friendly tech and energy subsidy for industry" ///
				 10 "Unemployment insurance" ///
				 11 "Forest clearing for agriculture" ///
				 12 "Forest clearing for settlement dev." ///
				 13 "Forest clearing for infra. dev."
la val outnum outnumlab	

g eqnum = equation
qui levelsof treatnm 
foreach k of numlist `r(levels)' {
	preserve
	keep if treatnm==`k'
	qui levelsof outnum, local(outnumlist) 
	loc second_outnum = word("`outnumlist'",2)
	loc last_outnum = substr("`outnumlist'",-2,.)
	qui distinct outnum 
	if `r(ndistinct)'>1 {
		loc j=4
		forval i = `second_outnum'/`last_outnum' {
			qui count if outnum==`i' 
			if `r(N)'>0 {
				replace eqnum = equation + `j' if outnum==`i' 
				loc j=`j'+4
			}
		}
	}

	loc fignm "DK_treat`k'_wyoung_linear"
	qui sum lci
	loc maxlci=abs(`r(min)')
	qui sum uci
	loc maxuci=abs(`r(max)')	
	loc intval=max(`maxlci',`maxuci')
	loc steps=`intval'/4
	loc texty= 0
	loc textx=`intval'/2.5 
	qui sum eqnum
	loc scalemax=`r(max)'+1
	
	forval j=1/13 {
		loc val`j'=0
		loc label`j' " "
	}
	qui levelsof outnum
	foreach h of numlist `r(levels)' {
		qui sum eqnum if outnum==`h',d
		loc val`h'=`r(p50)'
		if `val`h''>0 {
			loc label`h': label outnumlab `h'
		}
	}

	twoway 	(bar coef eqnum if equation==1, horizontal) ///
			(bar coef eqnum if equation==2, horizontal) ///
			(bar coef eqnum if equation==3, horizontal) ///
			(rcap uci lci eqnum, horizontal lcolor(gray)), ///
			xtitle("Probability relative to control", size(vsmall)) xline(0, lpattern(dash) lcolor(red)) ///
			xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval',format(%12.2fc) labsize(vsmall))  /// 
			legend(pos(12) row(1) order(1 "Model 1" 2 "Model 2" 3 "Model 3") size(vsmall)) ///
			yscale(range(0 `scalemax') reverse noline) ytitle("") ///
			ylabel(`val1' "`label1'" `val2' "`label2'" `val3' "`label3'" `val4' "`label4'" `val5' "`label5'" ///
			`val6' "`label6'" `val7' "`label7'" `val8' "`label8'" `val9' "`label9'" `val10' "`label10'" ///
			`val11' "`label11'" `val12' "`label12'" `val13' "`label13'", notick labsize(vsmall) nogrid) ///
			note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) ///
			text(`texty' -`textx' "Less likely to support",size(vsmall)) text(`texty' `textx' "More likely to support",size(vsmall)) ///
			subtitle("", size(small)) saving("$fig\\`fignm'.gph", replace )
	gr export "$fig\\`fignm'.png", replace 		
	restore 
}

* ==== TD ==== *
** linear model
set scheme plotplainblind
use "$temp\TD_wyoung_ologit_allmodel.dta", clear
recode treatnm (1=4)
				
la def outnumlab 1 "Create an economy that fulfills everyone's needs" ///
				 2 "Force the gov. to create an economy that fulfills everyone's needs" ///
				 3 "" ///
				 4 "Ensuring everyone has a chance for success" ///
				 5 "Ensuring everyone has a decent life" 
la val outnum outnumlab	

g eqnum = equation
qui levelsof treatnm 
foreach k of numlist `r(levels)' {
	loc outset1 keep if inlist(outnum,1,2)
	loc outset2 keep if inlist(outnum,3)
	loc outset3 keep if inlist(outnum,4,5)
	forval o = 1/3 {
		preserve
		`outset`o''
		
		loc fignm "TD_set`o'_treat`k'_wyoung_ologit"
		qui sum lci
		loc maxlci=abs(`r(min)')
		qui sum uci
		loc maxuci=abs(`r(max)')	
		loc intval=max(`maxlci',`maxuci')
		loc steps=`intval'/4
		loc texty= 0
		loc textx=`intval'/2.5 
		qui sum eqnum
		loc scalemax=`r(max)'+1
		
		forval j=1/13 {
			loc val`j'=0
			loc label`j' " "
		}
		qui levelsof outnum
		foreach h of numlist `r(levels)' {
			qui sum eqnum if outnum==`h',d
			loc val`h'=`r(p50)'
			if `val`h''>0 {
				loc label`h': label outnumlab `h'
			}
		}
		
		if `o'==1 {
			loc posstance "More inclined to believe a lot we can do"
			loc negstance "More inclined to believe a little we can do"
			loc title "How much society can do to"
			loc textsize tiny			
		}
		else if `o'==2 {
			loc posstance "More inclined to be optimistic"
			loc negstance "More inclined to be pessimistic"
			loc title ""
			loc textsize vsmall
		}
		else {
			loc posstance "More inclined to believe huge responsibility"
			loc negstance "More inclined to believe small responsibility"
			loc title "How much responsibility society have in"
			loc textsize tiny
		}

		twoway 	(bar coef eqnum if equation==1, horizontal) ///
				(bar coef eqnum if equation==2, horizontal) ///
				(bar coef eqnum if equation==3, horizontal) ///
				(rcap uci lci eqnum, horizontal lcolor(gray)), ///
				by(outnum, note("Note: Confidence interval crossing zero indicates a null effect.", size(tiny)) t1title("`title'", size(small)) legend(pos(12))) ///
				xtitle("Odds ratio relative to control", size(vsmall)) xline(0, lpattern(dash) lcolor(red)) ///
				xscale(range(-`intval' `intval')) xlab(-`intval'(`steps')`intval',format(%12.2fc) labsize(vsmall))  /// 
				legend(pos(12) row(1) order(1 "Model 1" 2 "Model 2" 3 "Model 3") size(vsmall)) ///
				yscale(range(0 `scalemax') reverse noline) ytitle("") ylabel(none) ///
				text(`texty' -`textx' "`negstance'",size(`textsize')) text(`texty' `textx' "`posstance'",size(`textsize')) ///
				subtitle(`subtitle',size(vsmall)) saving("$fig\\`fignm'.gph", replace )
		gr export "$fig\\`fignm'.png", replace 		
		restore 
	}
}
				 
* ==== ARCHIVE ==== *
// bys treatnm equation (outnum): g outnumid=_n
// g eqnum = outnumid
// qui levelsof treatnm 
// foreach k of numlist `r(levels)' {
// 	qui distinct outnum if treatnm==`k'
// 	loc h=`r(ndistinct)'+1
// 	loc j=`h'
// 	forval i = 2/3 {
// 		replace eqnum = outnumid + `j' if equation==`i' & treatnm==`k'
// 		loc j=`j'+`h'
// 	}
// }
//
//
// * plot 
// preserve
// keep if treatnm==1
// forval i=1/13 {
// 	loc legnum=`i'+1
// 	loc leg`legnum'
// 	loc leglab`legnum' ""
// }
// qui levelsof outnum if treatnm==1
// foreach i of numlist `r(levels)' {
// 	loc legnum=`i'+1
// 	loc leg`legnum'=`legnum'
// 	loc leglab: label outnumlab `i'
// 	loc leglab`legnum' "`leglab'"
// }
// twoway 	(bar coef eqnum if outnum==1, horizontal) ///
// 		(bar coef eqnum if outnum==2, horizontal) ///
// 		(bar coef eqnum if outnum==3, horizontal) ///
// 		(bar coef eqnum if outnum==4, horizontal) ///
// 		(bar coef eqnum if outnum==5, horizontal) ///
// 		(bar coef eqnum if outnum==6, horizontal) ///
// 		(bar coef eqnum if outnum==7, horizontal) ///
// 		(bar coef eqnum if outnum==8, horizontal) ///
// 		(bar coef eqnum if outnum==9, horizontal) ///
// 		(bar coef eqnum if outnum==10, horizontal) ///
// 		(bar coef eqnum if outnum==11, horizontal) ///
// 		(bar coef eqnum if outnum==12, horizontal) ///
// 		(bar coef eqnum if outnum==13, horizontal) ///
// 		(rcap uci lci eqnum, horizontal lcolor(gray)), ///
// 		legend(order(`leg2' "`leglab2'" ///
// 		`leg3' "`leglab3'" ///
// 		`leg4' "`leglab4'" ///
// 		`leg5' "`leglab5'" ///
// 		`leg6' "`leglab6'" ///
// 		`leg7' "`leglab7'" ///
// 		`leg8' "`leglab8'" ///
// 		`leg9' "`leglab9'" ///
// 		`leg10' "`leglab10'" ///
// 		`leg11' "`leglab11'" ///
// 		`leg12' "`leglab12'" ///
// 		`leg13' "`leglab13'" ///
// 		`leg14' "`leglab14'" ))
// restore		
