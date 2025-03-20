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
	
	spshape2dta "$ipt\shapefile\idn_admbnda_adm1_bps_20200401.shp"
	
	u "idn_admbnda_adm1_bps_20200401.dta", clear
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
	
	* count of respondents
	bys uuid: gen resp = _N

* ==== VISUALIZATION ==== *	
ssc install schemepack, replace
set scheme plotplainblind  

	* (2) ID03 - Gender (Block Chart)
	treemap resp, by(sex) ///
		labsize(2.5) title("Respondent's Sex Distribution (N = 4,315)") ///
		share labprop noval format(%7.2f) palette(mono) ///
		note("Note: The smallest block represents a group" "comprising 0.16% of respondents choosing not to tell their sex.", size(small)) ///
		name("VIS_ID03_block_chart", replace) ///
		saving("$fig\VIS_ID03_block_chart.gph", replace)
	gr export "$fig\VIS_ID03_block_chart.png", replace
	
	* (3) ID04 - Age (Histogram) 
	histogram age, width(2)      ///
		title("Respondent's Age Distiribution") ///
		name("VIS_ID04_histogram", replace) ///
		saving("$fig\VIS_ID04_histogram.gph", replace)
	gr export "$fig\VIS_ID04_histogram.png", replace
	
	* (4) ID06 - Education (Block Chart)
	la def edlvl 2 "Primary school" 3 "Junior high school" 4 "High school" 5 "Higher education", modify
	la val edlvl edlvl
	
	treemap resp, by(edlvl) 	///
		labsize(3) title("Respondent's Education Distribution (N = 4,315)") ///
		share labprop noval format(%7.2f) palette(mono) ///
		note("Note: The smallest block represents a group comprising 0.67% of" "respondents with elementary school education level.", size(small)) ///
		name("VIS_ID06_block_chart", replace) ///
		saving("$fig\VIS_ID06_block_chart.gph", replace)
	gr export "$fig\VIS_ID06_block_chart.png", replace
	
{//	NON - PRIORITY
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
	
}


	* (1) ID01 ID02 - Geographical Distribution (Chloropleth Map) [to be added]
	** transform prov from 38 to 34
	clonevar prov_34 = prov
	recode prov_34 (34 = 33) (36 37 38 = 35)
	
	
	collapse (count) resp (mean) urban, by(prov_34)
		g prop_prov = resp/4135
		g percent_prov = prop_prov*100
		format percent_prov %7.2f
		
		g percent_urban = urban*100
		format percent_urban %7.2f

	
	** generate province code for GIS matching
	gen ADM1_PCODE = ""
		replace ADM1_PCODE = "ID11" if prov_34==1
		replace ADM1_PCODE = "ID12" if prov_34==2
		replace ADM1_PCODE = "ID13" if prov_34==3
		replace ADM1_PCODE = "ID14" if prov_34==4
		replace ADM1_PCODE = "ID15" if prov_34==5
		replace ADM1_PCODE = "ID16" if prov_34==6
		replace ADM1_PCODE = "ID17" if prov_34==7
		replace ADM1_PCODE = "ID18" if prov_34==8
		replace ADM1_PCODE = "ID19" if prov_34==9
		replace ADM1_PCODE = "ID21" if prov_34==10		
		replace ADM1_PCODE = "ID31" if prov_34==11
		replace ADM1_PCODE = "ID32" if prov_34==12
		replace ADM1_PCODE = "ID33" if prov_34==13
		replace ADM1_PCODE = "ID34" if prov_34==14
		replace ADM1_PCODE = "ID35" if prov_34==15
		replace ADM1_PCODE = "ID36" if prov_34==16
		replace ADM1_PCODE = "ID51" if prov_34==17
		replace ADM1_PCODE = "ID52" if prov_34==18
		replace ADM1_PCODE = "ID53" if prov_34==19
		replace ADM1_PCODE = "ID61" if prov_34==20
		replace ADM1_PCODE = "ID62" if prov_34==21
		replace ADM1_PCODE = "ID63" if prov_34==22
		replace ADM1_PCODE = "ID64" if prov_34==23
		replace ADM1_PCODE = "ID65" if prov_34==24
		replace ADM1_PCODE = "ID71" if prov_34==25
		replace ADM1_PCODE = "ID72" if prov_34==26
		replace ADM1_PCODE = "ID73" if prov_34==27
		replace ADM1_PCODE = "ID74" if prov_34==28
		replace ADM1_PCODE = "ID75" if prov_34==29
		replace ADM1_PCODE = "ID76" if prov_34==30
		replace ADM1_PCODE = "ID81" if prov_34==31
		replace ADM1_PCODE = "ID82" if prov_34==32
		replace ADM1_PCODE = "ID91" if prov_34==33
		replace ADM1_PCODE = "ID94" if prov_34==35

	** merge with shapefile
	merge 1:1 ADM1_PCODE using "idn_admbnda_adm1_bps_20200401.dta"
		drop if _m != 3
		drop _m
	
	** map	
	grmap percent_prov, title("Respondent's Province Distribution (%)") ///
		legstyle(2) legend(pos(7) size(3.5) ) ///
		note("Note: the distribution uses 34-province classification to accomodate the availability of GIS data", size(small)) ///
		name("VIS_ID01_map", replace) ///
		saving("$fig\VIS_ID01_map.gph", replace)
	gr export "$fig\VIS_ID01_map.png", replace
	 
	grmap percent_urban, title("Proportion of Urban Residents by Province (%)") ///
		legstyle(2) legend(pos(7) size(3.5)) ///
		note("Note: the distribution uses 34-province classification to accomodate the availability of GIS data", size(small)) ///
	 	name("VIS_ID02_map", replace) ///
		saving("$fig\VIS_ID02_map.gph", replace)
	gr export "$fig\VIS_ID02_map.png", replace

* ==== END OF DO-FILE ==== *	
	