********************************************************************************
* AUTHORS: Andres Rovira (with Stephanie Grove, Xianya Zhou, Fabio Schanaider)
* CREATED: 2023-02-08
* PURPOSE: Solve coding portions of HCMG 901 Problem Set 1
********************************************************************************

global home "~"
if "$S_OS" == "Windows" global home "`:env USERPROFILE'" 
cd "$home/Dropbox (Penn)/Classes/2_health_applied_metrics/ps1/code"
global ol "$home/Dropbox (Penn)/Apps/Overleaf/hcmg901_ps1"

// ssc install texsave
// ssc install blindschemes
// ssc install psmatch2

set scheme plotplainblind

********************************************************************************
**# Append datasets together
********************************************************************************

clear all
gen dataset = ""
foreach f in cps1 nsw nsw_dw psid1 {
	append using "../input/`f'.dta"
	replace dataset = "`f'" if missing(dataset)
}
save "../intermediate/combined.dta", replace


********************************************************************************
**# Part 1
********************************************************************************

use "../intermediate/combined.dta", clear

tostring treat, replace
gen group = dataset + "_" + treat

levelsof group, local(groups)
local glist ""
foreach g of local groups {
	local glist "`glist' `g'"
}
local xvars age education black hispanic married nodegree re74 re75 re78

tempname memhold
tempfile results
postfile `memhold' str20 varname sd_ind `glist' using `results'
foreach var of varlist `xvars' {
	local mean_postline "(0)"
	local sd_postline   "(1)"
	foreach g of local groups {
		quietly summarize `var' if group == "`g'"
		local mean_postline "`mean_postline' (`r(mean)')"
		local sd_postline   "`sd_postline' (`r(sd)')"
	}
	local mean_postline = subinstr("`mean_postline'", "()", "(.)", .)
	local sd_postline   = subinstr("`sd_postline'", "()", "(.)", .)
	post `memhold' ("`var'") `mean_postline'
	post `memhold' ("`var'") `sd_postline'
}
postclose `memhold'
use `results', clear

// Format latex table
foreach var of local glist {
	format `var' %13.2fc
	tostring `var', replace force usedisplayformat
	replace `var' = "" if `var' == "."
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace varname = "" if sd_ind == 1
drop sd_ind
order varname nsw_1 nsw_0 nsw_dw_1 nsw_dw_0 cps1_0 psid1_0
label var varname ""
label var nsw_1    "NSW Treated"
label var nsw_0    "NSW Control"
label var nsw_dw_1 "NSW-DW Treated"
label var nsw_dw_0 "NSW-DW Control"
label var cps1_0   "CPS"
label var psid1_0  "PSID"
texsave * using "$ol/tab1.tex", varlabels frag replace title("Descriptive Statistics") location("hbt")


********************************************************************************
**# Part 2
********************************************************************************

global adj_vars age age2 education nodegree black hispanic

tempname memhold 
tempfile results
postfile `memhold' str20 comp_grp sd_ind c1 c2 c3 c4 c5 c6 c7 c8 c9 using `results'
foreach d in nsw psid1 cps1 {
	
	use "../intermediate/combined.dta", clear
	keep if (dataset == "nsw" & treat == 1) | (dataset == "`d'" & treat == 0)
	gen age2 = age^2
	gen re_growth = re78 - re75

	preserve
		keep treat re75 re78
		gen id = _n
		reshape long re, i(id treat) j(year)
		regress re i.year if treat == 0, robust
		local c1b  =  _b[78.year]
		local c1se = _se[78.year]
	restore

	regress re75 treat, robust
		local c2b  =  _b[treat]
		local c2se = _se[treat]
	regress re75 treat $adj_vars, robust
		local c3b  =  _b[treat]
		local c3se = _se[treat]
	regress re78 treat, robust
		local c4b  =  _b[treat]
		local c4se = _se[treat]
	regress re78 treat $adj_vars, robust
		local c5b  =  _b[treat]
		local c5se = _se[treat]
	regress re_growth treat, robust
		local c6b  =  _b[treat]
		local c6se = _se[treat]
	regress re_growth treat age, robust
		local c7b  =  _b[treat]
		local c7se = _se[treat]
	regress re_growth treat re75, robust
		local c8b  =  _b[treat]
		local c8se = _se[treat]
	regress re_growth treat re75 $adj_vars, robust
		local c9b  =  _b[treat]
		local c9se = _se[treat]
		
	post `memhold' ("`d'") (0) (`c1b')  (`c2b')  (`c3b')  (`c4b')  (`c5b')  (`c6b')  (`c7b')  (`c8b')  (`c9b')
	post `memhold' ("`d'") (1) (`c1se') (`c2se') (`c3se') (`c4se') (`c5se') (`c6se') (`c7se') (`c8se') (`c9se')
}
postclose `memhold'
use `results', clear

// Format latex table
format %13.0fc c? 
tostring c?, replace force usedisplayformat
foreach var of varlist c? {
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace comp_grp = "" if sd_ind == 1
drop sd_ind
replace comp_grp = "PSID-1"    if comp_grp == "psid1"
replace comp_grp = "CPS-SSA-1" if comp_grp == "cps1"
replace comp_grp = "Controls"  if comp_grp == "nsw"
label var comp_grp "Comparison Group"
label var c1 "Earnings Growth"
label var c2 "Unadjusted"
label var c3 "Adjusted"
label var c4 "Unadjusted"
label var c5 "Adjusted"
label var c6 "Without Age"
label var c7 "With Age"
label var c8 "Unadjusted"
label var c9 "Adjusted"
texsave * using "$ol/tab2.tex", varlabels frag replace autonumber ///
	title("Replication of Lalonde (1986) Table 5") //
	//headerlines("{}&{}&\multicolumn{2}{c}{Treatment Earnings Less Comparison, 1975}&\multicolumn{2}{c}{Treatment Earnings Less Comparison, 1978}&\multicolumn{2}{c}{Diff-in-Diff}&\multicolumn{2}{c}{Unrestricted Diff-in-Diff} \tabularnewline")
	

********************************************************************************
**# Part 4
********************************************************************************

use "../intermediate/combined.dta", clear
drop if dataset == "nsw"

gen age2 = age^2
gen age3 = age^3
gen education2 = education^2

gen ps_sample_cps  = (dataset == "nsw_dw" & treat == 1) | (dataset == "cps1"  & treat == 0)
gen ps_sample_psid = (dataset == "nsw_dw" & treat == 1) | (dataset == "psid1" & treat == 0)

global dw_psvars age age2 age3 education education2 married nodegree black hispanic re74 re75 c.education#c.re74

logit treat $dw_psvars if ps_sample_cps == 1
predict ps_cps if ps_sample_cps == 1

logit treat $dw_psvars if ps_sample_psid == 1
predict ps_psid if ps_sample_psid == 1

save "../intermediate/combined_w_pscore.dta", replace


local covars age education nodegree black hispanic married re74 re75 re78
foreach var of varlist `covars' {
	summarize `var' if dataset == "cps1" [iweight=ps_cps]
		local mean_`var'_cps = string(r(mean), "%4.2f")
		dis "`mewn_`var'_cps'"
	summarize `var' if dataset == "psid1" [iweight=ps_psid]
		local mean_`var'_psid = string(r(mean), "%4.2f")
	summarize `var' if dataset == "nsw_dw" & treat == 1
		local mean_`var'_treat = string(r(mean), "%4.2f")
}

cap erase "$ol/tab4.tex" 
file open fh using "$ol/tab4.tex", write replace 
#delimit ;
file write fh
"\begin{table}[!htbp]" _n
"\centering" _n
"\caption{\textbf{Group Means of X Variables}}" _n
"\resizebox{\textwidth}{!}{ " _n
"\begin{tabular}{l|ccc}" _n
"\toprule" _n
"Variable & Treated NSW & Matched CPS & Matched PSID \\" _n
"\midrule " _n
"age & `mean_age_treat' & `mean_age_cps' & `mean_age_psid' \\ " _n
"education & `mean_education_treat' & `mean_education_cps' & `mean_education_psid' \\ " _n
"nodegree & `mean_nodegree_treat' & `mean_nodegree_cps' & `mean_nodegree_psid' \\ " _n
"black & `mean_black_treat' & `mean_black_cps' & `mean_black_psid' \\ " _n
"hispanic & `mean_hispanic_treat' & `mean_hispanic_cps' & `mean_hispanic_psid' \\ " _n
"married & `mean_married_treat' & `mean_married_cps' & `mean_married_psid' \\ " _n
"re74 & `mean_re74_treat' & `mean_re74_cps' & `mean_re74_psid' \\ " _n
"re75 & `mean_re75_treat' & `mean_re75_cps' & `mean_re75_psid' \\ " _n
"re78 & `mean_re78_treat' & `mean_re78_cps' & `mean_re78_psid' \\ " _n
"\bottomrule" _n
"\end{tabular} " _n
"}" _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh 


********************************************************************************
**# Part 5
********************************************************************************

// CPS version
use "../intermediate/combined_w_pscore.dta", clear
keep if ps_sample_cps == 1
summarize ps_cps if treat == 1
drop if ps_cps < `r(min)'
local n_discarded = `r(N_drop)'

sort treat ps_cps
gen first_ctrl_bin = (treat == 0) & (ps_cps <= 0.05)
gsort first_ctrl_bin -ps_cps
by first_ctrl_bin: gen num = _n
count if first_ctrl_bin == 1
local n_first_bin = `r(N)'
drop if first_ctrl_bin == 1 & num > 200

twoway ///
	(histogram ps_cps if treat == 0, start(0) width(0.05) color(red%30)  frequency) ///
	(histogram ps_cps if treat == 1, start(0) width(0.05) color(blue%30) frequency), ///
	legend(order(1 "CPS" 2 "DW Treated")) xlabel(, format(%2.1fc)) yscale(range(0 200)) ///
	xtitle("Estimated Propensity Score, `n_discarded' comparison units discarded, first bin contains `n_first_bin' units", size(vsmall))
graph export "$ol/fig5_cps.png", replace


// PSID version
use "../intermediate/combined_w_pscore.dta", clear
keep if ps_sample_psid == 1
summarize ps_psid if treat == 1
drop if ps_psid < `r(min)'
local n_discarded = `r(N_drop)'

sort treat ps_psid
gen first_ctrl_bin = (treat == 0) & (ps_psid <= 0.05)
gsort first_ctrl_bin -ps_psid
by first_ctrl_bin: gen num = _n
count if first_ctrl_bin == 1
local n_first_bin = `r(N)'
drop if first_ctrl_bin == 1 & num > 100

twoway ///
	(histogram ps_psid if treat == 0, start(0) width(0.05) color(red%30)  frequency) ///
	(histogram ps_psid if treat == 1, start(0) width(0.05) color(blue%30) frequency), ///
	legend(order(1 "PSID" 2 "DW Treated")) xlabel(, format(%2.1fc)) yscale(range(0 100)) ///
	xtitle("Estimated Propensity Score, `n_discarded' comparison units discarded, first bin contains `n_first_bin' units", size(vsmall))
graph export "$ol/fig5_psid.png", replace


********************************************************************************
**# Parts 6 and 8
********************************************************************************

global dw_regvars age education married nodegree black hispanic re74 re75

local cps_full_row  "Full CPS"
local psid_full_row "Full PSID"
local cps_title  "Replication of Dehejia and Wahba Table 2"
local psid_title "Replication of Dehejia and Wahba Table 3"

foreach d in cps psid {
	
	tempname memhold 
	tempfile results
	postfile `memhold' str50 ctrl_samp sd_ind obs mean_ps te_dim reg_te ols_te using `results'

	// Row 1
	use "../intermediate/combined_w_pscore.dta", clear
	count if dataset == "nsw_dw" & treat == 1
		local obs = `r(N)'
	summarize ps_`d' if dataset == "nsw_dw" & treat == 1
		local mean_ps = `r(mean)'
	bootstrap, reps(100): regress re78 treat if dataset == "nsw_dw"
		local te_dim = _b[treat]
		local te_dim_se = _se[treat]
	bootstrap, reps(100): regress re78 treat $dw_regvars if dataset == "nsw_dw"
		local reg_te = _b[treat]
		local reg_te_se = _se[treat] 
		
	post `memhold' ("NSW") (0) (`obs') (`mean_ps') (`te_dim')    (`reg_te')    (.)
	post `memhold' ("NSW") (1) (.)     (.)         (`te_dim_se') (`reg_te_se') (.)

	// Row 2
	use "../intermediate/combined_w_pscore.dta", clear
	count if dataset == "`d'1"
		local obs = `r(N)'
	summarize ps_`d' if dataset == "`d'1"
		local mean_ps = `r(mean)'
	bootstrap, reps(100): regress ps_`d' treat if dataset == "`d'1" | (dataset == "nsw_dw" & treat == 1)
		local mean_ps_se = _se[treat]
	bootstrap, reps(100): regress re78 treat if dataset == "`d'1" | (dataset == "nsw_dw" & treat == 1)
		local te_dim = _b[treat]
		local te_dim_se = _se[treat]
	bootstrap, reps(100): regress re78 treat $dw_regvars if dataset == "`d'1" | (dataset == "nsw_dw" & treat == 1)
		local reg_te = _b[treat]
		local reg_te_se = _se[treat] 
		
	post `memhold' ("``d'_full_row'") (0) (`obs') (`mean_ps')    (`te_dim')    (`reg_te')    (.)
	post `memhold' ("``d'_full_row'") (1) (.)     (`mean_ps_se') (`te_dim_se') (`reg_te_se') (.)

	// Remaining rows
	forvalues row = 3/10 {
		
		use "../intermediate/combined_w_pscore.dta", clear
		keep if ps_sample_`d' == 1
		
		if `row' == 3 {
			local samp_name "W/o Replacement: Random"
			tempvar sortorder
			gen `sortorder' = runiform()
			sort `sortorder'
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement
		}
		else if `row' == 4 {
			local samp_name "W/o Replacement: Low to High"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement
		}
		else if `row' == 5 {
			local samp_name "W/o Replacement: High to Low"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement descending
		}
		else if `row' == 6 {
			local samp_name "With Replacement: Nearest Neighbor"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) 
		}
		else if `row' == 7 {
			local samp_name "With Replacement: Caliper = 0.00001"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.00001) 
		}
		else if `row' == 8 {
			local samp_name "With Replacement: Caliper = 0.00005"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.00005)
		}
		else if `row' == 9 {
			local samp_name "With Replacement: Caliper = 0.0001"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.0001)
		}
		else if `row' == 10 {
			local samp_name "With Replacement: Caliper = 0.001"
			if "`d'" == "cps" continue
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.001)
		}
		
		keep if !missing(_weight)

		count if treat == 0
			local obs = `r(N)'
		regress re78 treat $dw_regvars, robust
			local ols_te = _b[treat]
			local ols_te_se = _se[treat]
		
		expand _weight, generate(num) // applies weighting to subsequent estimations
		
		summarize ps_`d' if treat == 0
			local mean_ps = `r(mean)'
		bootstrap, reps(100): regress ps_`d' treat
			local mean_ps_se = _se[treat]
		bootstrap, reps(100): regress re78 treat
			local te_dim = _b[treat]
			local te_dim_se = _se[treat]
		bootstrap, reps(100): regress re78 treat $dw_regvars
			local reg_te = _b[treat]
			local reg_te_se = _se[treat] 
			
		post `memhold' ("`samp_name'") (0) (`obs') (`mean_ps')    (`te_dim')    (`reg_te')    (`ols_te')
		post `memhold' ("`samp_name'") (1) (.)     (`mean_ps_se') (`te_dim_se') (`reg_te_se') (`ols_te_se')
		
	}

	postclose `memhold'
	use `results', clear

	// Format latex table
	format %13.0fc obs te_dim reg_te ols_te
	format %13.2fc mean_ps 
	foreach var of varlist obs mean_ps te_dim reg_te ols_te {
		tostring `var', replace force usedisplayformat
		replace `var' = "" if `var' == "."
		replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
	}
	replace ctrl_samp = "" if sd_ind == 1
	drop sd_ind
	label var ctrl_samp "Control Sample"
	label var obs "Obs"
	label var mean_ps "Mean Propensity Score"
	label var te_dim "Treatment Effect (Diff. in Means)"
	label var reg_te "Regression Treatment Effect"
	label var ols_te "Q8: Simple OLS"
	texsave * using "$ol/tab6_`d'.tex", varlabels frag replace title("``d'_title'") 

}


********************************************************************************
**# Part 10
********************************************************************************

use "../intermediate/combined.dta", clear
keep if inlist(dataset, "nsw", "cps1")

gen age2 = age^2
gen age3 = age^3
gen education2 = education^2

gen ps_sample_cps = (dataset == "nsw" & treat == 1) | (dataset == "cps1"  & treat == 0)

global nondw_psvars age age2 age3 education education2 married nodegree black hispanic re75 

logit treat $nondw_psvars if ps_sample_cps == 1
predict ps_cps if ps_sample_cps == 1

save "../intermediate/combined_w_pscore_nondw.dta", replace


********************************************************************************
**# Part 11
********************************************************************************

global nondw_regvars age education married nodegree black hispanic re75

local cps_full_row  "Full CPS"
local psid_full_row "Full PSID"
local cps_title  "Replication of Dehejia and Wahba Table 2 with Full NSW Sample"
local psid_title "Replication of Dehejia and Wahba Table 3 with Full NSW Sample"

foreach d in cps {
	
	tempname memhold 
	tempfile results
	postfile `memhold' str50 ctrl_samp sd_ind obs mean_ps te_dim reg_te ols_te using `results'

	// Row 1
	use "../intermediate/combined_w_pscore_nondw.dta", clear
	count if dataset == "nsw" & treat == 1
		local obs = `r(N)'
	summarize ps_`d' if dataset == "nsw" & treat == 1
		local mean_ps = `r(mean)'
	bootstrap, reps(100): regress re78 treat if dataset == "nsw"
		local te_dim = _b[treat]
		local te_dim_se = _se[treat]
	bootstrap, reps(100): regress re78 treat $nondw_regvars if dataset == "nsw"
		local reg_te = _b[treat]
		local reg_te_se = _se[treat] 
		
	post `memhold' ("NSW") (0) (`obs') (`mean_ps') (`te_dim')    (`reg_te')    (.)
	post `memhold' ("NSW") (1) (.)     (.)         (`te_dim_se') (`reg_te_se') (.)

	// Row 2
	use "../intermediate/combined_w_pscore_nondw.dta", clear
	count if dataset == "`d'1"
		local obs = `r(N)'
	summarize ps_`d' if dataset == "`d'1"
		local mean_ps = `r(mean)'
	bootstrap, reps(100): regress ps_`d' treat if dataset == "`d'1" | (dataset == "nsw" & treat == 1)
		local mean_ps_se = _se[treat]
	bootstrap, reps(100): regress re78 treat if dataset == "`d'1" | (dataset == "nsw" & treat == 1)
		local te_dim = _b[treat]
		local te_dim_se = _se[treat]
	bootstrap, reps(100): regress re78 treat $nondw_regvars if dataset == "`d'1" | (dataset == "nsw" & treat == 1)
		local reg_te = _b[treat]
		local reg_te_se = _se[treat] 
		
	post `memhold' ("``d'_full_row'") (0) (`obs') (`mean_ps')    (`te_dim')    (`reg_te')    (.)
	post `memhold' ("``d'_full_row'") (1) (.)     (`mean_ps_se') (`te_dim_se') (`reg_te_se') (.)

	// Remaining rows
	forvalues row = 3/10 {
		
		use "../intermediate/combined_w_pscore_nondw.dta", clear
		keep if ps_sample_`d' == 1
		
		if `row' == 3 {
			local samp_name "W/o Replacement: Random"
			tempvar sortorder
			gen `sortorder' = runiform()
			sort `sortorder'
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement
		}
		else if `row' == 4 {
			local samp_name "W/o Replacement: Low to High"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement
		}
		else if `row' == 5 {
			local samp_name "W/o Replacement: High to Low"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) noreplacement descending
		}
		else if `row' == 6 {
			local samp_name "With Replacement: Nearest Neighbor"
			psmatch2 treat, outcome(re78) pscore(ps_`d') neighbor(1) 
		}
		else if `row' == 7 {
			local samp_name "With Replacement: Caliper = 0.00001"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.00001) 
		}
		else if `row' == 8 {
			local samp_name "With Replacement: Caliper = 0.00005"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.00005)
		}
		else if `row' == 9 {
			local samp_name "With Replacement: Caliper = 0.0001"
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.0001)
		}
		else if `row' == 10 {
			local samp_name "With Replacement: Caliper = 0.001"
			if "`d'" == "cps" continue
			psmatch2 treat, outcome(re78) pscore(ps_`d') caliper(0.001)
		}
		
		keep if !missing(_weight)

		count if treat == 0
			local obs = `r(N)'
		regress re78 treat $nondw_regvars, robust
			local ols_te = _b[treat]
			local ols_te_se = _se[treat]
		
		expand _weight, generate(num) // applies weighting to subsequent estimations
		
		summarize ps_`d' if treat == 0
			local mean_ps = `r(mean)'
		bootstrap, reps(100): regress ps_`d' treat
			local mean_ps_se = _se[treat]
		bootstrap, reps(100): regress re78 treat
			local te_dim = _b[treat]
			local te_dim_se = _se[treat]
		bootstrap, reps(100): regress re78 treat $nondw_regvars
			local reg_te = _b[treat]
			local reg_te_se = _se[treat] 
			
		post `memhold' ("`samp_name'") (0) (`obs') (`mean_ps')    (`te_dim')    (`reg_te')    (`ols_te')
		post `memhold' ("`samp_name'") (1) (.)     (`mean_ps_se') (`te_dim_se') (`reg_te_se') (`ols_te_se')
		
	}

	postclose `memhold'
	use `results', clear

	// Format latex table
	format %13.0fc obs te_dim reg_te ols_te
	format %13.2fc mean_ps 
	foreach var of varlist obs mean_ps te_dim reg_te ols_te {
		tostring `var', replace force usedisplayformat
		replace `var' = "" if `var' == "."
		replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
	}
	replace ctrl_samp = "" if sd_ind == 1
	drop sd_ind
	label var ctrl_samp "Control Sample"
	label var obs "Obs"
	label var mean_ps "Mean Propensity Score"
	label var te_dim "Treatment Effect (Diff. in Means)"
	label var reg_te "Regression Treatment Effect"
	label var ols_te "Q8: Simple OLS"
	texsave * using "$ol/tab11_`d'.tex", varlabels frag replace title("``d'_title'") 

}









 