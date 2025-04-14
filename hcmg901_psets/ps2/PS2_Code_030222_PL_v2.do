
/*******************************************************************************
								BASIC SET UP
*******************************************************************************/		
clear all 
set more off 
cap log close 
version 16 

*set globals
if c(username)=="ashle" {
	global drive "C:\Users\ashle\Dropbox\Wharton\2_Courses\HCMG 901\3_Assignments\PS2"
} 
else if c(username)=="lugthart" {
	global drive "/home/bepp/lugthart/Documents/HCMG_901/PS2"
}

* set styles
set scheme s1mono
graph set window fontface "Times New Roman"


*set other locals
#d ; 
local list_vars "birthyear_list
		female_list 
		english_list 
		self_list 
		first_day_list 
		have_phone_list 
		pobox_list 
		zip_msa" ;
#d cr

*set seed
set seed 457906 // taken from replication code 

/*******************************************************************************
A: BALANCE AND INTENT TO TREAT 
*******************************************************************************/
/* BALANCE TABLE */
use "$drive/data_for_analysis.dta", clear // produced from replication code


foreach var in `list_vars' {
	
	** Table A13, Column 1 **
	sum `var' if treatment == 0 
	local `var'_con_mn = string(r(mean), "%4.3f")
	local `var'_con_sd = string(r(sd), "%4.3f")

	** Table A13, Column 4 **
	// household size and survey wave dummies and their interactions
	//survey weights
	//reg `var' treatment hh0dum* hh6dum* hh12dum* surv0dum* surv6dum* surv12dum* [aweight = weight_12m], vce(cluster household_id)  
	qui reg `var' treatment i.hhsize_12m##i.draw_survey_12m i.draw_lottery [pw = weight_12m], vce(cluster household_id)
	local `var'_b = string(_b[treatment], "%10.3fc")
	local `var'_se = string(_se[treatment], "%10.3fc")
}

cap erase "$drive/QA_Balance.tex" 
file open fh using "$drive/QA_Balance.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Balance Table}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Control mean (SD)}} & \multicolumn{1}{c}{\textbf{Diff. Between Treatment and Control}}  \\  " _n
" & & Survey Respondents Subsample \\" _n
"\midrule " _n
"Year of Birth & `birthyear_list_con_mn' & `birthyear_list_b'   \\ " _n
" & (`birthyear_list_con_sd') & (`birthyear_list_se')  \\ " _n
"Female & `female_list_con_mn' & `female_list_b'  \\ " _n
" & (`female_list_con_sd') & (`female_list_se')  \\ " _n
"English as pref. lang. & `english_list_con_mn' & `english_list_b'   \\ " _n
" & (`english_list_con_sd') & (`english_list_se') \\ " _n
"Signed up self & `self_list_con_mn' & `self_list_b' \\ " _n
" & (`self_list_con_sd') & (`self_list_se')   \\ " _n
"Signed up 1st day & `first_day_list_con_mn' & `first_day_list_b'  \\ " _n
" & (`first_day_list_con_sd') & (`first_day_list_se')  \\ " _n
"Gave phone num. & `have_phone_list_con_mn' & `have_phone_list_b'  \\ " _n
" & (`have_phone_list_con_sd') & (`have_phone_list_se')  \\ " _n
"Address PO Box & `pobox_list_con_mn' & `pobox_list_b'  \\ " _n
" & (`pobox_list_con_sd') & (`pobox_list_se')   \\ " _n
"In MSA & `zip_msa_con_mn' & `zip_msa_b'   \\ " _n
" & (`zip_msa_con_sd') & (`zip_msa_se')  \\ " _n
"\midrule " _n
"Observations & 74,922 & 74,922 \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh                                                                                                                                                                                                                                       


/* INTENT TO TREAT */
* Table IV Panel A, Col 2 
* see III.B. Survey Data on Outcomes for survey wave info
* Note that control for baseline level of outcome is "through notification date" suggesting the 0 month survey 
//gen hosp_noter_num = hosp_num_mod_12m er_num_mod_12m 

* List of outcome variables--extensive margin only (Panel A)
#d ; 
local itt_vars "hosp_any_ 
		er_any_ 
		not_er_noner_
		" ;
#d cr

* Regressions 
foreach var in `itt_vars' { 
	reg `var'12m treatment i.hhsize_12m i.draw_lottery `var'0m [pw = weight_12m], vce(cluster household_id)
	local `var'b = string(_b[treatment], "%10.4fc")
	local `var'se = string(_se[treatment], "%10.4fc")
}

cap erase "$drive/QA_ITT.tex" 
file open fh using "$drive/QA_ITT.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{ITT}}" _n
"\begin{tabular}{p{15em}cc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{ITT effect}} \\  " _n
"\midrule " _n
"All hospital admissions & `hosp_any_b'  \\ " _n
" & (`hosp_any_se') \\ " _n
"Admissions through ER & `er_any_b'  \\ " _n
" & (`er_any_se') \\ " _n
"Admissions not through ER & `not_er_noner_b'   \\ " _n
" & (`not_er_noner_se') \\ " _n
"\midrule " _n
"Observations  & 74,922 \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh





/*******************************************************************************
B: BALANCE FOR MEDICAID VS NOT
*******************************************************************************/



foreach var in `list_vars' {
	
	** Medicaid==0 means **
	sum `var' if ohp_all_at_12m== 0 
	local `var'_con_mn = string(r(mean), "%4.3f")
	local `var'_con_sd = string(r(sd), "%4.3f")
	local `var'_con_n = string(r(N), "%10.0fc")
	
	** Diff. with medicaid==1 **
	// household size and survey wave dummies and their interactions
	//survey weights
	//reg `var' treatment hh0dum* hh6dum* hh12dum* surv0dum* surv6dum* surv12dum* [aweight = weight_12m], vce(cluster household_id)  
	qui reg `var' ohp_all_at_12m i.hhsize_12m##i.draw_survey_12m i.draw_lottery [pw = weight_12m], vce(cluster household_id)
	local `var'_b = string(_b[ohp_all_at_12m ], "%10.3fc")
	local `var'_se = string(_se[ohp_all_at_12m ], "%10.3fc")
	local `var'_n = string(e(N), "%10.0fc")
}

cap erase "$drive/QB_Balance.tex" 
file open fh using "$drive/QB_Balance.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Balance Table for Not Enrolled vs Enrolled in Medicaid}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Not enrolled mean (SD)}} & \multicolumn{1}{c}{\textbf{Diff. b/w not enrolled \& enrolled}}  \\  " _n
" & & Survey Respondents Subsample \\" _n
"\midrule " _n
"Year of Birth & `birthyear_list_con_mn' & `birthyear_list_b'   \\ " _n
" & (`birthyear_list_con_sd') & (`birthyear_list_se')  \\ " _n
"Female & `female_list_con_mn' & `female_list_b'  \\ " _n
" & (`female_list_con_sd') & (`female_list_se')  \\ " _n
"English as pref. lang. & `english_list_con_mn' & `english_list_b'   \\ " _n
" & (`english_list_con_sd') & (`english_list_se') \\ " _n
"Signed up self & `self_list_con_mn' & `self_list_b' \\ " _n
" & (`self_list_con_sd') & (`self_list_se')   \\ " _n
"Signed up 1st day & `first_day_list_con_mn' & `first_day_list_b'  \\ " _n
" & (`first_day_list_con_sd') & (`first_day_list_se')  \\ " _n
"Gave phone num. & `have_phone_list_con_mn' & `have_phone_list_b'  \\ " _n
" & (`have_phone_list_con_sd') & (`have_phone_list_se')  \\ " _n
"Address PO Box & `pobox_list_con_mn' & `pobox_list_b'  \\ " _n
" & (`pobox_list_con_sd') & (`pobox_list_se')   \\ " _n
"In MSA & `zip_msa_con_mn' & `zip_msa_b'   \\ " _n
" & (`zip_msa_con_sd') & (`zip_msa_se')  \\ " _n
"\midrule " _n
"Observations & `birthyear_list_con_n' &  `birthyear_list_n' & \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh                                                                                                                                                                                              



/*******************************************************************************
C: BALANCE FOR MEDICAID VS NOT
*******************************************************************************/

#d ; 
local LHS_vars 	rx_any_12m
		doc_any_12m
		er_any_12m
		hosp_any_12m
		
		rx_num_mod_12m
		doc_num_mod_12m
		er_num_mod_12m
		hosp_num_mod_12m
		; 


local RHS_vars	ohp_std_ever_survey
		ins_any_12m  ; 
#d cr 




* Regressions 
foreach RHS_var in `RHS_vars' {
		if "`RHS_var'" == "ohp_std_ever_survey"{ 
			local type = "ohp"
		} 
		if "`RHS_var'" == "ins_any_12m" { 
			local type = "any"
		} 
	foreach LHS_var in `LHS_vars' { 
		reg `LHS_var' `RHS_var' [pw = weight_12m], vce(cluster household_id)
		local `type'_`LHS_var'_b = string(_b[`RHS_var'], "%10.4fc")
		local `type'_`LHS_var'_se = string(_se[`RHS_var'], "%10.4fc")
		local `type'_`LHS_var'_n = string(e(N), "%10.0fc")
	}
}
di `any_rx_any_12m_se'
di `ohp_rx_any_12m_se'
di `ohp_rx_any_12m_n'
di `ohp_rx_num_mod_12m_n'
di `ohp_rx_num_12m_mod_b'

cap erase "$drive/QC_OLS.tex" 
file open fh using "$drive/QC_OLS.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{OLS, independent variable is Medicaid enrollment}}" _n
"\begin{tabular}{p{12em}cccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Ever on OHP Std (SE)}} & \multicolumn{1}{c}{\textbf{Ever on OHP Std (SE)}} & \multicolumn{1}{c}{\textbf{Curr. have any ins.}}  & \multicolumn{1}{c}{\textbf{Curr. have any ins.}}  \\  " _n
" & Extensive Margin (any) & Intensive Margin (\#) & Extensive Margin (any) & Intensive Margin (\#) \\" _n
"\midrule " _n
"Prescription drugs currently & `ohp_rx_any_12m_b' & `ohp_rx_num_mod_12m_b' & `any_rx_any_12m_b' & `any_rx_num_mod_12m_b'  \\ " _n
" & (`ohp_rx_any_12m_se') & (`ohp_rx_num_mod_12m_se') & (`any_rx_any_12m_se') & (`any_rx_num_mod_12m_se')  \\ " _n
"Primary care visits & `ohp_doc_any_12m_b' & `ohp_doc_num_mod_12m_b' & `any_doc_any_12m_b' & `any_doc_num_mod_12m_b'  \\ " _n
" & (`ohp_doc_any_12m_se') & (`ohp_doc_num_mod_12m_se') & (`any_doc_any_12m_se') & (`any_doc_num_mod_12m_se')  \\ " _n
"ER visits last 6 months & `ohp_er_any_12m_b' & `ohp_er_num_mod_12m_b' & `any_er_any_12m_b' & `any_er_num_mod_12m_b'  \\ " _n
" & (`ohp_er_any_12m_se') & (`ohp_er_num_mod_12m_se') & (`any_er_any_12m_se') & (`any_er_num_mod_12m_se')  \\ " _n
"Inpatient hosp. visits, last 6 m & `ohp_hosp_any_12m_b' & `ohp_hosp_num_mod_12m_b' & `any_hosp_any_12m_b' & `any_hosp_num_mod_12m_b'  \\ " _n
" & (`ohp_hosp_any_12m_se') & (`ohp_hosp_num_mod_12m_se') & (`any_hosp_any_12m_se') & (`any_hosp_num_mod_12m_se')  \\ " _n
"\midrule " _n
"Observations & `ohp_rx_any_12m_n' & `ohp_rx_num_mod_12m_n'  & `any_rx_any_12m_n' & `any_rx_num_mod_12m_n' \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh            

/*******************************************************************************
D.a.: BALANCE FOR MEDICAID VS NOT
*******************************************************************************/

#d ; 
local LHS_vars	ohp_std_ever_survey
				ins_any_12m  ; 
#d cr

foreach LHS_var in `LHS_vars' { 
	reg `LHS_var' treatment i.hhsize_12m##i.draw_survey_12m  [pw = weight_12m], vce(cluster household_id)
	local `LHS_var'_b = string(_b[treatment], "%10.3fc")
	local `LHS_var'_se = string(_se[treatment], "%10.3fc")
	local reg_n = string(e(N), "%10.0fc")
	sum `LHS_var' if treatment==0 
	local `LHS_var'_mn = string(r(mean), "%10.3fc")
	local `LHS_var'_sd = string(r(sd), "%10.3fc")
}

cap erase "$drive/QDa_first_stage.tex" 
file open fh using "$drive/QDa_first_stage.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Rep. of Table 3, Cols 5 \& 6}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Control Mean (SD)}}  & \multicolumn{1}{c}{\textbf{First stage (SE)}} \\  " _n
"\midrule " _n
"Ever on OHP Stnd & `ohp_std_ever_survey_mn' & `ohp_std_ever_survey_b'  \\ " _n
" & (`ohp_std_ever_survey_sd') & (`ohp_std_ever_survey_se') \\ " _n
"Currently have any ins. & `ins_any_12m_mn' & `ins_any_12m_b'  \\ " _n
" & (`ins_any_12m_sd') & (`ins_any_12m_se') \\ " _n
"\midrule " _n
"Observations  &  & `reg_n' \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh


/* 2SLS Estimates */
local LHS_vars 	rx_any_12m /// 
		doc_any_12m ///
		er_any_12m ///
		hosp_any_12m
		
local RHS_vars	ohp_std_ever_survey ///
				ins_any_12m  


foreach LHS_var of varlist `LHS_vars' {
	foreach RHS_var of varlist `RHS_vars' {
		if "`RHS_var'" == "ohp_std_ever_survey" {
			local type = "ohp"
		}
		if "`RHS_var'" == "ins_any_12m" {
			local type = "ins"
		}
		ivregress 2sls `LHS_var' i.hhsize_12m##i.draw_survey_12m  (`RHS_var' = treatment) [pw = weight_12m], vce(cluster household_id) 
		local `LHS_var'_`type'_b = string(_b[`RHS_var'], "%10.3fc")
		local `LHS_var'_`type'_se = string(_se[`RHS_var'], "%10.3fc")
	}
}



cap erase "$drive/QDab_2sls.tex" 
file open fh using "$drive/QDab_2sls.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{2SLS Estimates}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Ever on OHP Stnd}}  & \multicolumn{1}{c}{\textbf{Currently Have Ins.}} \\  " _n
"\midrule " _n
"Prescription drugs currently & `rx_any_12m_ohp_b' & `rx_any_12m_ins_b'  \\ " _n
" & (`rx_any_12m_ohp_se') & (`rx_any_12m_ins_se') \\ " _n
"Primary care visits & `doc_any_12m_ohp_b' & `doc_any_12m_ins_b'  \\ " _n
" & (`doc_any_12m_ohp_se') & (`doc_any_12m_ins_se') \\ " _n
"ER visits last 6 months & `er_any_12m_ohp_b' & `er_any_12m_ins_b'  \\ " _n
" & (`er_any_12m_ohp_se') & (`er_any_12m_ins_se') \\ " _n
"Inpatient hospital visits, last 6 months & `hosp_any_12m_ohp_b' & `hosp_any_12m_ins_b'  \\ " _n
" & (`hosp_any_12m_ohp_se') & (`hosp_any_12m_ins_se') \\ " _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh


/*******************************************************************************
D.c.: COMPLIERS ANALYSIS 
*******************************************************************************/
cap drop old 
gen old = birthyear_list < 1968 

rename female_list fem
rename self_list self

local dem_vars "old fem self"
local RHS_vars	ohp_std_ever_survey ///
				ins_any_12m  

foreach RHS_var of varlist `RHS_vars' { 
	sum `RHS_var' if treatment == 1 
	local treated = r(mean) 
	sum `RHS_var' if treatment == 0 
	local untreated = r(mean) 
	local csize_`RHS_var' = string(`treated' - `untreated', "%10.3fc") 
		
	foreach var of varlist `dem_vars' {
		sum `RHS_var' if treatment == 1 & `var' == 1 
		local treated_`var' = r(mean)
		
		sum `RHS_var' if treatment == 0 & `var' == 1 
		local untreated_`var' = r(mean)
		
		local frac_`var'_`RHS_var' =  string((`treated_`var'' - `untreated_`var'')/`csize_`RHS_var'', "%10.3fc")
	}	
}

cap erase "$drive/QDc_compliers.tex" 
file open fh using "$drive/QDc_compliers.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Meet The Compliers}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Ever on OHP Stnd}}  & \multicolumn{1}{c}{\textbf{Currently Have Ins.}} \\  " _n
"\midrule " _n
"Share Compliers & `csize_ohp_std_ever_survey' & `csize_ins_any_12m'  \\ " _n
"Old & `frac_old_ohp_std_ever_survey' & `frac_old_ins_any_12m'  \\ " _n
"Female & `frac_fem_ohp_std_ever_survey' & `frac_fem_ins_any_12m'  \\ " _n
"Self Sign-Up & `frac_self_ohp_std_ever_survey' & `frac_self_ins_any_12m'  \\ " _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh













