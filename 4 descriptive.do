* ==== DESCRIPTIVE ==== *
* GOAL: EXPORT VISUALIZATION (4 PRIORITY + 3 ADDITIONAL)

* clear environment
capture log close
clear all 
set more off


	* install resources
	ssc install treemap, replace
		*resource: https://github.com/asjadnaqvi/stata-treemap
	ssc install palettes, replace
	ssc install colrspace, replace

	ssc install spmap, replace
	
	*spshape2dta "$ipt\xxx.shp"
	
	*u "$ipt\xxx.dta", clear
	grmap, activate
	grmap

* ==== DATA PREP ==== *

* load data
use "$ipt\raw_$date.dta", clear 	
	
	* province
	clonevar prov=ID01

	* urban
	g urban=ID02==1

	* male
	clonevar sex=ID03
	g male=ID03==1
	
	* age
	clonevar age=ID04

	* education
	clonevar edlvl=ID06
	
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
	

* ==== VISUALIZATION ==== *	
ssc install schemepack, replace
set scheme plotplainblind  

	
	* (1) ID01 ID02 - Geographical Distribution (Chloropleth Map) [to be added]
	
	* (2) ID03 - Gender (Block Chart)
	bys uuid: gen resp = _N

	treemap resp, by(sex) ///
		labsize(2.5) title("Respondent's Sex Distribution") ///
		share labprop noval format(%7.2f) palette(mono) ///
		note("Note: The smallest block represents a group" "comprising 0.16% of respondents choosing not to tell their sex.", size(small)) ///
		name("VIS_ID03_block_chart", replace) ///
		saving("$fig\VIS_ID03_block_chart.gph", replace)
	gr export "$fig\VIS_ID03_block_chart.png", replace
	
	* (3) ID04 - Age (Histogram) 
	histogram age, frequency width(2)        ///
		title("Histogram of Respondent's Age")   ///
		addlabopts(mlabsize(4-pt)) ///
		name("VIS_ID04_histogram", replace) ///
		saving("$fig\VIS_ID04_histogram.gph", replace)
	gr export "$fig\VIS_ID04_histogram.png", replace
	
	* (4) ID06 - Education (Block Chart)
	treemap resp, by(edlvl) 	///
		labsize(1.5) title("Respondent's Education Distribution") ///
		share labprop noval format(%7.2f) palette(mono) ///
		note("Note: The smallest block represents a group comprising 0.67% of" "respondents with Elementary School or equivalent education level.", size(small)) ///
		name("VIS_ID06_block_chart", replace) ///
		saving("$fig\VIS_ID06_block_chart.gph", replace)
	gr export "$fig\VIS_ID06_block_chart.png", replace
	
	* (5) CB01 - Correct Stimulus by Treatment Arm
	tab lfCB crt_intrpt_msg, row nofreq
	
	clonevar crt_intrpt_msg_flip = crt_intrpt_msg
	recode crt_intrpt_msg_flip (1=1) (0=2)

	graph bar, over(crt_intrpt_msg_flip) over(lfCB) ///
		asyvars percentages  stack                 ///
		legend(order(1 "Yes" 2 "No")) ///
		ytitle("Proportion") ///
		title("Correct Interpretation of Stimulus")	///
		subtitle("by Treatment Arm (%)") ///
		blabel(bar, format(%7.2f)) ///
		name("VIS_CB01_bar_chart", replace) ///
		saving("$fig\VIS_CB01_bar_chart.gph", replace)
	gr export "$fig\VIS_CB01_bar_chart.png", replace
	
	
	* (6) CB02 - Emotion by Treatment Arm (Facet by Emotion)

	forval i= 1/9 {
		egen CB02r`i'=rowtotal(CB02Ar`i' CB02Br`i' CB02Cr`i' CB02Dr`i' CB02Er`i')
	}
 
	local r1 = "Anger"
	local r2 = "Anxiety"
	local r3 = "Disgust"
	local r4 = "Fear"
	local r5 = "Sad"
	local r6 = "Happy"
	local r7 = "Calm"
	local r8 = "Longing"
	local r9 = "Hopeless"
	
	forval i = 1/9 {
		graph bar if CB02r`i'==1, over(lfCB) ///
			ytitle("Proportion") ///
			title(`r`i'') ///
			blabel(bar, format(%7.2f)) ///
			yscale(range(0 40)) ///
			name(fig_r`i', replace)
	}
	
	graph combine fig_r1 fig_r2 fig_r3 fig_r4 fig_r5 fig_r6 fig_r7 fig_r8 fig_r9,cols(3) ///
		title("Proportion of Emotion After Reading Stimulus" "by Treatment Arm (%)") ///
		name("VIS_CB02_bar_chart", replace) ///
		saving("$fig\VIS_CB02_bar_chart.gph", replace)
	gr export "$fig\VIS_CB02_bar_chart.png", replace
	
	
	
	* (7) CB03 - Sentence in Stimulus by Correctly Interpreting (Facet by Emotion)
	la def crt_flip 2 "Incorrectly Interpret" 1 "Correctly Interpret", modify
	la val crt_intrpt_msg_flip crt_flip
	
		** Group A
		graph bar, over(CB03A) by(crt_intrpt_msg_flip, total row(1) note("") title("Sentence Resonation of Group A") subtitle("by Correctly Interpret Stimulus (%)")) ///
			asyvars percentages ///
			ytitle("Proportion") ///
			blabel(bar, format(%7.2f)) ///
			legend(symxsize(*2)) ///
			name(fig_CB03A, replace) ///
			saving("$fig\VIS_CB03_A_bar_chart.gph", replace)
		gr export "$fig\VIS_CB03_A_bar_chart.png", replace
	
		** Group B
		graph bar, over(CB03B) by(crt_intrpt_msg_flip, total row(1) note("") title("Sentence Resonation of Group B") subtitle("by Correctly Interpret Stimulus (%)")) ///
			asyvars percentages ///
			ytitle("Proportion") ///
			blabel(bar, format(%7.2f)) ///
			legend(symxsize(*2)) ///
			name(fig_CB03B, replace) ///
			saving("$fig\VIS_CB03_B_bar_chart.gph", replace)
		gr export "$fig\VIS_CB03_B_bar_chart.png", replace
	

		** Group C
		graph bar, over(CB03C) by(crt_intrpt_msg_flip, total row(1) note("") title("Sentence Resonation of Group C") subtitle("by Correctly Interpret Stimulus (%)")) ///
			asyvars percentages ///
			ytitle("Proportion") ///
			blabel(bar, format(%7.2f)) ///
			legend(symxsize(*2)) ///
			name(fig_CB03C, replace) ///
			saving("$fig\VIS_CB03_C_bar_chart.gph", replace)
		gr export "$fig\VIS_CB03_C_bar_chart.png", replace
			
		** Group D
		graph bar, over(CB03D) by(crt_intrpt_msg_flip, total row(1) note("") title("Sentence Resonation of Group D") subtitle("by Correctly Interpret Stimulus (%)")) ///
			asyvars percentages ///
			ytitle("Proportion") ///
			blabel(bar, format(%7.2f)) ///
			legend(symxsize(*2)) ///
			name(fig_CB03D, replace) ///
			saving("$fig\VIS_CB03_D_bar_chart.gph", replace)
		gr export "$fig\VIS_CB03_D_bar_chart.png", replace
			
		* Group E
		graph bar, over(CB03E) by(crt_intrpt_msg_flip, total row(1) note("") title("Sentence Resonation of Group E") subtitle("by Correctly Interpret Stimulus (%)")) ///
			asyvars percentages ///
			ytitle("Proportion") ///
			blabel(bar, format(%7.2f)) ///
			legend(symxsize(*2)) ///
			name(fig_CB03E, replace) ///
			saving("$fig\VIS_CB03_E_bar_chart.gph", replace)
		gr export "$fig\VIS_CB03_E_bar_chart.png", replace	
	
	
* ==== END (TO BE ADDED WITH CHOROPLETH MAP) ==== *	
	