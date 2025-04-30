/*******************************************************************************
AUTHORS: Andres Rovira and Fabio Schanaider 
CREATED: 2023-02-24
PURPOSE: Solve coding portions of HCMG 901 Problem Set 2
*******************************************************************************/

global home "~"
if "$S_OS" == "Windows" global home "`:env USERPROFILE'" 
global code "$home/Dropbox (Penn)/Classes/2_health_applied_metrics/ps2/code"
global ol "$home/Dropbox (Penn)/Apps/Overleaf/hcmg901_ps2"
cd "$code"

// ssc install texsave


********************************************************************************
**# Replicate analysis data from Finkelstein et al. (2012) using their code
********************************************************************************

// Downloaded data from "https://data.nber.org/oregon/4.data.html"
// Unzipped into the input folder

global repl_code "../input/OHIE_Public_Use_Files/OHIE_QJE_Replication_Code"
global repl_data "$repl_code/Data"

// Manual step: As instructed in "oregon_hie_qje_replication.do" copy data files
//	from input/OHIE_Public_Use_Files/OHIE_Data to a Data folder under $repl_code

// Run their data prep code
cd "$repl_code"
do "SubPrograms/prepare_data.do"
cd "$code"

// Relabel some variables for tables created later
use "$repl_data/data_for_analysis.dta", clear
label var birthyear_list  "Birth year"
label var female_list     "Female"
label var english_list    "English materials requested"
label var self_list       "Signed self up"
label var first_day_list  "Signed up first day"
label var have_phone_list "Gave phone number"
label var pobox_list      "Gave PO box as address"
label var zip_msa         "ZIP is an MSA"
save "$repl_data/data_for_analysis.dta", replace

// Define variable lists used by paper
global baseline_list birthyear_list female_list english_list self_list ///
                     first_day_list have_phone_list pobox_list zip_msa
global survey_useext_list rx_any_12m doc_any_12m er_any_12m hosp_any_12m
global mdcd_covg_vars ohp_all_ever_survey ohp_all_at_12m


********************************************************************************
**# Part a, balance table by lottery outcome
********************************************************************************

use "$repl_data/data_for_analysis.dta", clear

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 varname sd_ind control treatment diff using `tabfile'
foreach var of varlist $baseline_list {
	regress `var' if treatment == 0 & returned_12m == 1 [pweight = weight_12m]
		local c_mean = _b[_cons]
		local c_sd   = e(rmse)
	regress `var' if treatment == 1 & returned_12m == 1 [pweight = weight_12m]
		local t_mean = _b[_cons]
		local t_sd   = e(rmse)
	regress `var' treatment ddd* if returned_12m == 1 [pweight = weight_12m], vce(cluster household_id)
		local d_mean = _b[treatment]
		local d_se   = _se[treatment]
	local lbl : var label `var'
	post `tabpost' ("`lbl'") (0) (`c_mean') (`t_mean') (`d_mean')
	post `tabpost' ("`lbl'") (1) (`c_sd')   (`t_sd')   (`d_se')
}					 
postclose `tabpost'
use `tabfile', clear

format control treatment %13.2fc 
format diff %13.4fc
foreach var of varlist control treatment diff {
	tostring `var', replace force usedisplayformat
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace varname = "" if sd_ind == 1
drop sd_ind
label var control   "Control Mean (SD)"
label var treatment "Treatment Mean (SD)"
label var diff      "Difference"
texsave * using "$ol/tab_a_balance.tex", varlabels frag replace location("H") ///
	title("Balance Table of Lottery Winners and Losers") 


********************************************************************************
**# Part a, ITT lottery effect on extensive margin of healthcare types
********************************************************************************
			
use "$repl_data/data_for_analysis.dta", clear

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 varname sd_ind itt using `tabfile'
foreach var of varlist $survey_useext_list {
	local lbl : var label `var'
	regress `var' treatment ddd* if sample_12m_resp == 1 [pweight = weight_12m], vce(cluster household_id)
	post `tabpost' ("`lbl'") (0) (_b[treatment])
	post `tabpost' ("`lbl'") (1) (_se[treatment])
}
postclose `tabpost'
use `tabfile', clear

format itt %13.4fc
tostring itt, replace force usedisplayformat
replace itt = "(" + itt + ")" if sd_ind == 1 & !missing(itt)
replace varname = "" if sd_ind == 1
drop sd_ind
label var itt "ITT"
texsave * using "$ol/tab_a_itt.tex", varlabels frag replace location("H") ///
	title("ITT effects of winning lottery on types of health care usage (extensive margin)")


********************************************************************************
**# Part b, balance table by medicaid receipt
********************************************************************************

use "$repl_data/data_for_analysis.dta", clear

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 varname sd_ind control treatment diff using `tabfile'
foreach var of varlist $baseline_list {
	regress `var' if ohp_all_at_12m == 0 & returned_12m == 1 [pweight = weight_12m]
		local c_mean = _b[_cons]
		local c_sd   = e(rmse)
	regress `var' if ohp_all_at_12m == 1 & returned_12m == 1 [pweight = weight_12m]
		local t_mean = _b[_cons]
		local t_sd   = e(rmse)
	regress `var' ohp_all_at_12m if returned_12m == 1 [pweight = weight_12m], vce(cluster household_id)
		local d_mean = _b[ohp_all_at_12m]
		local d_se   = _se[ohp_all_at_12m]
	local lbl : var label `var'
	post `tabpost' ("`lbl'") (0) (`c_mean') (`t_mean') (`d_mean')
	post `tabpost' ("`lbl'") (1) (`c_sd')   (`t_sd')   (`d_se')
}					 
postclose `tabpost'
use `tabfile', clear

format control treatment %13.2fc 
format diff %13.4fc
foreach var of varlist control treatment diff {
	tostring `var', replace force usedisplayformat
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace varname = "" if sd_ind == 1
drop sd_ind
label var control   "Control Mean (SD)"
label var treatment "Treatment Mean (SD)"
label var diff      "Difference"
texsave * using "$ol/tab_b_balance.tex", varlabels frag replace location("H") ///
	title("Balance Table by Receipt of Medicaid")


********************************************************************************
**# Part c, OLS of utilization on medicaid coverage
********************************************************************************

use "$repl_data/data_for_analysis.dta", clear

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 util_var sd_ind str100 mdcd_var est using `tabfile'
foreach yvar of varlist $survey_useext_list {
	local lbl : var label `yvar'
	foreach xvar of varlist $mdcd_covg_vars {
		regress `yvar' `xvar' if sample_12m_resp == 1 [pweight = weight_12m], vce(cluster household_id)
		post `tabpost' ("`lbl'") (0) ("`xvar'") (_b[`xvar'])
		post `tabpost' ("`lbl'") (1) ("`xvar'") (_se[`xvar'])
	}
}
postclose `tabpost'
use `tabfile', clear

reshape wide est, i(util_var sd_ind) j(mdcd_var) string
rename est* *
foreach var of varlist $mdcd_covg_vars {
	format `var' %13.4fc
	tostring `var', replace force usedisplayformat
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace util_var = "" if sd_ind == 1
drop sd_ind
label var ohp_all_ever_survey "Ever on Medicaid"
label var ohp_all_at_12m      "Currently on Medicaid"
texsave * using "$ol/tab_c.tex", varlabels frag replace location("H") ///
	title("OLS Estimates of Utilization Outcomes on Medicaid Coverage Indicators")


********************************************************************************
**# Part e, first stage estimates
********************************************************************************

// Table 3 Columns 5 & 6

use "$repl_data/data_for_analysis.dta", clear

label var ohp_all_ever_survey "Ever on Medicaid"
label var ohp_all_at_12m      "Currently on Medicaid"

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 varname sd_ind c_mean first_stage using `tabfile'
foreach var of varlist $mdcd_covg_vars {
	local lbl : var label `var'
	regress `var' if treatment == 0 & sample_12m_resp == 1 [pweight = weight_12m]
	local c_mean = _b[_cons]
	regress `var' treatment ddd* if sample_12m_resp == 1 [pweight = weight_12m], vce(cluster household_id)
	post `tabpost' ("`lbl'") (0) (`c_mean') (_b[treatment])
	post `tabpost' ("`lbl'") (1) (.)        (_se[treatment])
}
postclose `tabpost'
use `tabfile', clear

foreach var of varlist c_mean first_stage {
	format `var' %13.4fc
	tostring `var', replace force usedisplayformat
	replace `var' = "" if `var' == "."
	replace `var' = "(" + `var' + ")" if sd_ind == 1 & !missing(`var')
}
replace varname = "" if sd_ind == 1
drop sd_ind
label var c_mean      "Control mean"
label var first_stage "Estimated FS"
texsave * using "$ol/tab_e.tex", varlabels frag replace location("H") ///
	title("First-Stage Estimates")


********************************************************************************
**# Part f, compliers analysis
********************************************************************************

use "$repl_data/data_for_analysis.dta", clear

gen born_before_1968 = (birthyear_list < 1968)
label var born_before_1968 "Born before 1968"

global background_dummies born_before_1968 female_list english_list self_list ///
                          first_day_list have_phone_list pobox_list zip_msa

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 xvarname str100 yvar coef using `tabfile'
foreach yvar of varlist $mdcd_covg_vars {	
	
	// Size of complier group? Do a simpler first-stage with weights but no controls
	regress `yvar' treatment if sample_12m_resp == 1 [pweight = weight_12m]
		local complier_share = _b[treatment]
		post `tabpost' ("Share of compliers") ("`yvar'") (`complier_share')
	
	// Characterize compliers by covariates: coef from first-stage with covar turned on vs overall
	foreach xvar of varlist $background_dummies {
		local lbl : var label `xvar'
		regress `yvar' treatment if sample_12m_resp == 1 & `xvar' == 1 [pweight = weight_12m]
			local fs_xeq1 = _b[treatment]
		local x_comp_ratio = `fs_xeq1' / `complier_share'
		post `tabpost' ("`lbl'") ("`yvar'") (`x_comp_ratio')
	}
}
postclose `tabpost'
use `tabfile', clear

gen orig_order = _n
egen yvar_order = min(orig_order), by(yvar)
bysort yvar_order (orig_order): gen order_within = _n
drop orig_order yvar_order
reshape wide coef order_within, i(xvarname) j(yvar) string
sort order*
drop order*
rename coef* *
format $mdcd_covg_vars %13.2fc
tostring $mdcd_covg_vars, replace force usedisplayformat
label var ohp_all_ever_survey "Ever on Medicaid"
label var ohp_all_at_12m      "Currently on Medicaid"
texsave * using "$ol/tab_f.tex", varlabels frag replace hlines(1) location("H") ///
	title("Shares of Compliers and Ratios of Covariates Among Compliers vs Full Sample")


********************************************************************************
**# Part g, LATE estimation with 2SLS
********************************************************************************

use "$repl_data/data_for_analysis.dta", clear

tempname tabpost
tempfile tabfile
postfile `tabpost' str100 varname sd_ind late using `tabfile'
foreach var of varlist $survey_useext_list {
	local lbl : var label `var'
	ivregress 2sls `var' (ohp_all_ever_survey = treatment) ddd* ///
		if sample_12m_resp == 1 [pweight = weight_12m], vce(cluster household_id)
	post `tabpost' ("`lbl'") (0) (_b[ohp_all_ever_survey])
	post `tabpost' ("`lbl'") (1) (_se[ohp_all_ever_survey])
}
postclose `tabpost'
use `tabfile', clear

format late %13.4fc
tostring late, replace force usedisplayformat
replace late = "(" + late + ")" if sd_ind == 1 & !missing(late)
replace varname = "" if sd_ind == 1
drop sd_ind
label var late "LATE"
texsave * using "$ol/tab_g.tex", varlabels frag replace location("H") ///
	title("2SLS Estimates of LATE")

































