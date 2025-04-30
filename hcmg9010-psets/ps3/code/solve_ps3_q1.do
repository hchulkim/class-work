/*******************************************************************************
AUTHORS: Andres Rovira, Stephanie Grove
CREATED: 2023-03-17
PURPOSE: Solve 1st part of HCMG 901 Problem Set 3
*******************************************************************************/

global home "~"
if "$S_OS" == "Windows" global home "`:env USERPROFILE'" 
global code "$home/Dropbox (Penn)/Classes/2_health_applied_metrics/ps3/code"
global tex  "$home/Dropbox (Penn)/Apps/Overleaf/hcmg901_ps3"
cd "$code"

// ssc install texsave
// ssc install outreg2
// ssc install bacondecomp
// ssc install estout

********************************************************************************
**# 1.2
********************************************************************************

use "../input/divorce_example.dta", clear
isid year stfips sex

gen post = (year >= _nfd)
replace post = 1 if nfd == "PRE"

keep year stfips sex asmrs post
reshape wide asmrs, i(year stfips post) j(sex)
isid year stfips

local desc_vars post asmrs1 asmrs2
local stats count sd mean
tempfile desc_table
foreach stat of local stats {
	preserve
		collapse (`stat') `desc_vars'
		gen statistic = "`stat'"
		if "`stat'" == "count" {
			save `desc_table', replace
		}
		else {
			append using `desc_table'
			save `desc_table', replace
		}
	restore
}
use `desc_table', clear

order statistic
replace statistic = "Mean" if statistic == "mean"
replace statistic = "SD" if statistic == "sd"
replace statistic = "N" if statistic == "count"
label var post   "Post Policy Change"
label var asmrs1 "Suicide Rate Men"
label var asmrs2 "Suicide Rate Women"
format post asmrs? %13.2fc
tostring *, replace force usedisplayformat
foreach var of varlist * {
	replace `var' = subinstr(`var', ".00", "", .)
}
texsave * using "$tex/tab_2.tex", varlabels frag replace location("H") ///
	title("Descriptive Statistics") 
	
	
********************************************************************************
**# 1.3
********************************************************************************

use "../input/divorce_example.dta", clear
keep if year >= 1964 

gen post = (year >= _nfd)
replace post = 1 if nfd == "PRE"

gen years_post = year - _nfd
gen cat_post = 100 if years_post < 0 | nfd == "NRS"
replace cat_post = years_post if years_post == 0
forvalues i = 1(2)17 {
	replace cat_post = `i' if years_post == `i' | years_post == `i' + 1
}
replace cat_post = 19 if years_post >= 19 | nfd == "PRE"

label define lbl_years_post 0 "Year of Change" 1 "1-2 Years Later"
label define lbl_sex        1 "Male" 2 "Female"
label values sex lbl_sex

eststo clear
forvalues i = 1/2 {
	cap drop asmrs_elas_`i'
	summarize asmrs if sex == `i' & post == 0, meanonly
	gen asmrs_elas_`i' = 100 * asmrs / r(mean)
	if `i' == 1 label var asmrs_elas_`i' "Male Suicide Elas"
	if `i' == 2 label var asmrs_elas_`i' "Female Suicide Elas"
	eststo: regress asmrs_elas_`i' ib100.cat_post i.stfips i.year if sex == `i', robust
}

esttab est2 est1 using "$tex/tab_3.tex", replace ///
	nostar keep(*.cat_post) b(%9.1fc) se(%9.1fc) noobs ///
	coeflab(0.cat_post   "Year of Change" ///
	        1.cat_post   "1-2 Years Later" ///
			3.cat_post   "3-4 Years Later" ///
			5.cat_post   "5-6 Years Later" ///
			7.cat_post   "7-8 Years Later" ///
			9.cat_post   "9-10 Years Later" ///
			11.cat_post  "11-12 Years Later" ///
			13.cat_post  "13-14 Years Later" ///
			15.cat_post  "15-16 Years Later" ///
			17.cat_post  "17-18 Years Later" ///
			19.cat_post  "19+ Years Later" ///
			100.cat_post "preceding policy change") ///
	drop(100.cat_post) varwidth(25) mtitles("Female" "Male") ///
	title("Replication of Table I") ///
	stats(F p, fmt(a3 3) labels("F Statistic" "(p value)")) 


********************************************************************************
**# 1.4
********************************************************************************

do "DD_figure5_SW_replication_edited.do" 2
do "DD_figure5_SW_replication_edited.do" 1


********************************************************************************
**# 1.6
********************************************************************************

use "../input/divorce_example.dta" if year >= 1964 & sex == 2, clear

gen post = (year >= _nfd)
replace post = 1 if nfd == "PRE"

replace asmrs = asmrs * 10
keep asmrs year stfips post  
xtset stfips year

bacondecomp asmrs post, ddetail stub("gb_")
egen wgt = total(gb_S), by(gb_cgroup) 
collapse (mean) mean_b = gb_B [pweight = gb_S], by(gb_cgroup wgt)
egen tot_wgt = total(wgt)
gen norm_wgt = wgt / tot_wgt
drop wgt tot_wgt

graph export "$tex/fig_6.png", replace as(png)

label var gb_cgroup "Group"
label var mean_b    "DD Estimate"
label var norm_wgt  "Weight"
format mean_b norm_wgt %13.2fc
tostring mean_b norm_wgt, replace force usedisplayformat
decode gb_cgroup, gen(group)
drop gb_cgroup
order group
texsave * using "$tex/tab_6.tex", varlabels frag replace location("H") ///
	title("DD Estimates and Weights by Category") 





























