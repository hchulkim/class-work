/*******************************************************************************
AUTHORS: Andres Rovira and Fabio Schanaider
CREATED: 2023-04-01
PURPOSE: Solve HCMG 901 Problem Set 4
*******************************************************************************/

global home "~"
if "$S_OS" == "Windows" global home "`:env USERPROFILE'" 
global code "$home/Dropbox (Penn)/Classes/2_health_applied_metrics/ps4/code"
global tex  "$home/Dropbox (Penn)/Apps/Overleaf/hcmg901_ps4"
cd "$code"

// ssc install blindschemes
// ssc install texsave
// ssc install outreg2
// ssc install estout
// net install grc1leg
copy "https://eml.berkeley.edu/~jmccrary/DCdensity/DCdensity.ado" "./", replace

set scheme plotplainblind, perm

/* Data and reference code sources:
- Angrist and Lavy (1999): https://economics.mit.edu/people/faculty/josh-angrist/angrist-data-archive
- Angrist et al. (2019): https://www-aeaweb-org.proxy.library.upenn.edu/articles?id=10.1257/aeri.20180120
Components are copied to code/reference and input folders
*/

********************************************************************************
**# Part 2
********************************************************************************

// Combine 4th and 5th grader data
use "../input/al_1999/final4.dta", clear
rename *4* *5*
append using "../input/al_1999/final5.dta"
rename *5* *0*

// Clean some variables like AL1999 do
replace avgverb = avgverb - 100 if avgverb > 100
replace avgmath = avgmath - 100 if avgmath > 100
replace avgverb  = . if verbsize == 0
replace passverb = . if verbsize == 0
replace avgmath  = . if mathsize == 0
replace passmath = . if mathsize == 0

// Use their sample restrictions?
keep if 1 < classize & classize < 45 & c_size > 5
keep if c_leom == 1 & c_pik < 3

label var c_size   "School-Grade Enrollment"
label var classize "Class Size"
label var avgmath  "Math Score"
label var avgverb  "Grammar Score"
label var tipuach  "Percent Disadvantaged"

save "../intermediate/al_1999_data.dta", replace
use "../intermediate/al_1999_data.dta", clear

local test_score_vars avgverb avgmath

eststo clear
forvalues g = 4/5 {
	foreach var of varlist `test_score_vars' {
		eststo: regress `var' classize if grade == `g', robust
	}
}

esttab * using "$tex/tab_2.tex", replace label se booktabs ///
	mgroups("4th Graders" "5th Graders", pattern(1 0 1 0) span prefix(\multicolumn{@span}{c}{) suffix(})) ///
	title("Bivariate Regressons of Test Scores on Class Size")

	
********************************************************************************
**# Part 3
********************************************************************************

// Predictions of Maimonides' Rule	
gen pred_num_classes = int((c_size - 1) / 40) + 1
gen pred_class_size = c_size / pred_num_classes

label var pred_num_classes "Predicted Number of Classes"
label var pred_class_size  "Predicted Class Size"

sort c_size

// Predicted number of classes by enrollment
tw conn pred_num_classes c_size if grade == 4, name(grade4, replace) title("4th Graders") 
tw conn pred_num_classes c_size if grade == 5, name(grade5, replace) title("5th Graders") 
graph combine grade4 grade5, cols(1)
graph export "$tex/fig_3_num_classes.png", replace

// Predicted class size by enrollment
tw conn pred_class_size c_size if grade == 4, name(grade4, replace) title("4th Graders") 
tw conn pred_class_size c_size if grade == 5, name(grade5, replace) title("5th Graders") 
graph combine grade4 grade5, cols(1)
graph export "$tex/fig_3_class_size.png", replace


********************************************************************************
**# Part 4
********************************************************************************

// Actual vs Predicted Mean Class Sizes by Enrollment
egen mean_class_size_byenroll = mean(classize), by(c_size)	
label var mean_class_size_byenroll "Actual Mean Class Size"
tw line mean_class_size_byenroll pred_class_size c_size if grade == 4, ///
	name(grade4, replace) title("4th Graders") 
tw line mean_class_size_byenroll pred_class_size c_size if grade == 5, ///
	name(grade5, replace) title("5th Graders") 
grc1leg grade4 grade5, cols(1)
graph export "$tex/fig_4.png", replace

// Mean class size at school-grade level
egen mean_class_size = mean(classize), by(schlcode grade)
label var mean_class_size "Mean Class Size"

eststo clear
estpost tabstat mean_class_size, c(stat) stat(mean sd min p25 p50 p75 max)
esttab using "$tex/tab_4_descriptives.tex", replace booktabs nonumber noobs label ///
	cells("mean(fmt(%6.1fc)) sd(fmt(%6.1fc)) min p25 p50 p75 max") ///
	collabels("Mean" "SD" "Min" "Q1" "Median" "Q4" "Max") ///
	title("Descriptive Statistics on Actual Mean Class Size")

// Collapse to school-grade level and summarize mean covariates within
collapse (mean) avgmath avgverb (count) num_classes = classid (sum) classize, ///
	by(schlcode grade mean_class_size pred_class_size pred_num_classes tipuach c_size cohsize)
isid schlcode grade 
label var avgmath  "Mean Math Score"
label var avgverb  "Mean Grammar Score"

// Compliance must use number of classes due to imprecision in translation of cohort size to class sizes

// Overshooters: mean class size exceeds 40
gen overshooter = (mean_class_size > 40)
// Early Splitters: actual number of classes exceeds predicted number
gen earlysplitter = (num_classes > pred_num_classes)
// Compliers: all others. even if their number of classes or class size misses prediction
gen complier = (overshooter == 0) & (earlysplitter == 0)

eststo clear
local covars mean_class_size tipuach avgmath avgverb c_size
local stats "c(stat) stat(mean sd)"
eststo m1: estpost tabstat `covars', `stats'
eststo m2: estpost tabstat `covars' if complier == 1, `stats'
	summarize complier, meanonly
	local pc_comply = string(100 * r(mean), "%4.1fc")
eststo m3: estpost tabstat `covars' if earlysplitter == 1, `stats'
	summarize earlysplitter, meanonly
	local pc_earlysplit = string(100 * r(mean), "%4.1fc")
eststo m4: estpost tabstat `covars' if overshooter == 1, `stats'
	summarize overshooter, meanonly
	local pc_overshoot = string(100 * r(mean), "%4.1fc")
esttab m* using "$tex/tab_4_characteristics.tex", replace booktabs nonumber noobs label ///
	cells("mean(fmt(%6.1fc)) sd(fmt(%6.1fc))") ///
	collabels("Mean" "SD") ///
	mgroups("All" "Compliers (`pc_comply'\%)" ///
	        "Early Splitters (`pc_earlysplit'\%)" ///
			"Overshooters (`pc_overshoot'\%)", ///
	        pattern(1 1 1 1) span prefix(\multicolumn{@span}{c}{) suffix(})) ///
	title("Characteristics of Schools by Rule Compliance")




********************************************************************************
**# Part 5
********************************************************************************

local test_score_vars avgmath avgverb
local bandwidths 2 5 8 10 15 20 25 30
local poly_degs  0 1 2 3 4

foreach test_score of varlist `test_score_vars' {
	
	use "../intermediate/al_1999_data.dta", clear
	gen above_cutoff_1 = (c_size > 40)
	
	local test_score_lbl : var label `test_score'
	
	tempname p 
	tempfile f
	postfile `p' bw deg b se using `f'
	foreach bw of local bandwidths {
		gen in_bw_`bw' = (c_size >= 40 - `bw') & (c_size <= 40 + `bw')
		
		foreach d of local poly_degs {
				
			// Create the polynomial terms if not already there
			local c_size_poly
			forvalues i = 1/`d' {
				cap gen c_size_`i' = c_size^`i'
				local c_size_poly `c_size_poly' c_size_`i'
			}
			
			// Run IV regression
			ivregress 2sls `test_score' `c_size_poly' (classize = above_cutoff_1) ///
				if in_bw_`bw' == 1, vce(cluster schlcode)
			post `p' (`bw') (`d') (_b[classize]) (_se[classize])
		}
	}
	postclose `p'
	use `f', clear

	rename b est1
	rename se est2
	reshape long est, i(bw deg) j(etype)
	reshape wide est, i(deg etype) j(bw)

	format est* %13.2fc
	tostring deg est*, replace force usedisplayformat
	foreach var of varlist est* {
		replace `var' = "(" + `var' + ")" if etype == 2 & !missing(`var')
		local lbl : var label `var'
		local lbl = subinstr("`lbl'", " est", "", .)
		local lbl = "BW = `lbl'"
		label var `var' "`lbl'"
	}
	replace deg = "" if etype == 2
	drop etype 
	label var deg "Polynomial Degree"
	texsave * using "$tex/tab_5_`test_score'.tex", varlabels frag replace location("H") ///
		title("Fuzzy RD Estimates of of Class Size on `test_score_lbl'")

}


********************************************************************************
**# Part 6
********************************************************************************

forvalues g = 4/5 {
	
	use "../intermediate/al_1999_data.dta", clear
	contract schlcode c_size if grade == `g'
	isid schlcode
	hist c_size, discrete freq ///
		addplot(pci 0 40 20 40, lcolor(red) ///
		        || pci 0 80 20 80, lcolor(red) ///
		        || pci 0 120 20 120, lcolor(red)) ///
		legend(off) title("`g'th Grade Enrollment") ///
		xtitle("") ///
		saving("../output/hist_`g'.gph", replace)

	use "../intermediate/al_1999_data.dta", clear
	contract schlcode c_size if grade == `g'
	isid schlcode
	DCdensity c_size, breakpoint(41) generate(Xj Yj r0 fhat se_fhat) b(1) 
	graph save "../output/mccrary_`g'.gph", replace
	
}

graph combine "../output/hist_5.gph" "../output/hist_4.gph" ///
	"../output/mccrary_5.gph" "../output/mccrary_4.gph", rows(2) cols(2)
graph export "$tex/fig_6.png"

	
********************************************************************************
**# Part 7
********************************************************************************
	
use "../intermediate/al_1999_data.dta", clear
gen pred_num_classes = int((c_size - 1) / 40) + 1
gen pred_class_size = c_size / pred_num_classes
gen above_cutoff_1 = (c_size > 40)
gen c_size_2_d100 = (c_size^2) / 100

local grades 5 4
local donut_widths 1 2 3

tempname p 
tempfile f
postfile `p' grade donut est_type lang1 lang2 math1 math2 using `f'
foreach g of local grades {
	foreach dw of local donut_widths {
		
		cap gen in_donut_`dw' = (c_size < 40 - `dw') | (c_size > 40 + `dw')
		
		ivregress 2sls avgverb tipuach c_size (classize = pred_class_size) ///
				if in_donut_`dw' == 1 & grade == `g', vce(cluster schlcode)
			local l1_b  =  _b[classize]
			local l1_se = _se[classize]
		
		ivregress 2sls avgverb tipuach c_size c_size_2_d100 (classize = pred_class_size) ///
				if in_donut_`dw' == 1 & grade == `g', vce(cluster schlcode)
			local l2_b  =  _b[classize]
			local l2_se = _se[classize]
		
		ivregress 2sls avgmath tipuach c_size (classize = pred_class_size) ///
				if in_donut_`dw' == 1 & grade == `g', vce(cluster schlcode)
			local m3_b  =  _b[classize]
			local m3_se = _se[classize]
		
		ivregress 2sls avgmath tipuach c_size c_size_2_d100 (classize = pred_class_size) ///
				if in_donut_`dw' == 1 & grade == `g', vce(cluster schlcode)
			local m4_b  =  _b[classize]
			local m4_se = _se[classize]
		
		post `p' (`g') (`dw') (0) (`l1_b')  (`l2_b')  (`m3_b')  (`m4_b') 
		post `p' (`g') (`dw') (1) (`l1_se') (`l2_se') (`m3_se') (`m4_se') 
	}
}
postclose `p'
use `f', clear

gen donut_lower = 40 - donut
gen donut_upper = 40 + donut
format lang* math* %13.4fc
tostring grade donut* lang* math*, replace force usedisplayformat
foreach var of varlist lang* math* {
	replace `var' = "(" + `var' + ")" if est_type == 1 & !missing(`var')
}
gen donut_int = "[" + donut_lower + "," + donut_upper + "]"
foreach var of varlist grade donut_int {
	replace `var' = "" if est_type == 1
}
drop donut donut_lower donut_upper est_type
order grade donut_int

// Manually enter notes to append to end of table
preserve
	clear
	input str200 grade str200 donut_int str200 lang1 str200 lang2 str200 math1 str200 math2
		"Controls:"               "" ""  ""  ""  ""
		"Percent Disadvantaged"   "" "X" "X" "X" "X"
		"Enrollment"              "" "X" "X" "X" "X"
		"Enrollment Squared /100" "" ""  "X" ""  "X"
	end
	tempfile end_of_table
	save `end_of_table', replace
restore
append using `end_of_table'

label var grade "Grade"
label var donut_int "Donut"
label var lang1 "Language (1)"
label var lang2 "Language (2)"
label var math1 "Math (3)"
label var math2 "Math (4)"
texsave * using "$tex/tab_7.tex", varlabels frag replace location("H") hlines(6 12) ///
	title("Replication of Table A6")

	
	
	
	

