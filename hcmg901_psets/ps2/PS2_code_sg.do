********************************************************************************
*									PSET 2									   *
*								 Steph Grove 								   *
*								   2/24/23									   *
********************************************************************************

clear all 
set more off 
cap log close 
version 17
set seed 457906 
*^from replication code 

*** SET GLOBALS ***

if c(username)=="Steph" {
	global drive "/Users/Steph/Dropbox (Penn)/coursework/HCMG_901_personal/PS2"
	global output "/Users/Steph/Dropbox (Penn)/Apps/Overleaf/HCMG- PS2 Grove/figures_and_tables/"
} 
else if c(username)=="sgrove" {
	global drive "C:/Users/sgrove/Dropbox (Penn)/coursework/HCMG_901_personal/PS2"
	global output "C:/Users/sgrove/Dropbox (Penn)/Apps/Overleaf/HCMG- PS2 Grove/figures_and_tables/"

}
global data "${drive}/OHIE_Public_Use_Files/Data"

global iterations = 2 
*Number of multiple inference iterations - should be 10,000


********************************************************************************
*								  Set Switches 							       *
********************************************************************************
* set switch to 1 to execute, 0 for off

* p0: clean and save "data_for_analysis.dta" in the Data subfolder (taken from replication package)
local p0 = 0
* p1: homework Q2a, create balance & ITT tables
local p2a = 0
* p2b: homework Q2b
local p2b = 0
* p2c: homework Q2c
local p2c = 1
* p2e: homework Q2e
local p2e = 1
* p2f: homework Q2f
local p2f = 1
* p2g: homework Q2g
local p2g = 1

********************************************************************************
*							Switch 0: prepare data 							   *
********************************************************************************

if `p0'{
include "${drive}/OHIE_Public_Use_Files/OHIE_QJE_Replication_Code/SubPrograms/prepare_data.do"
}

********************************************************************************
*						Switch A: create balance & ITT tables 				   *
********************************************************************************

if `p2a'{
	use "${data}/data_for_analysis.dta", clear


	*create dummie variables
	tabulate edu_0m, generate(school)
	tab smk_curr_bi, gen(smoke_cig)
	tab employ_hrs_0m, gen(employm)

	* inc as % of FPL
	recode hhinc_pctfpl_0m (min/50 = 1 "below 50% of FPL") (50/75 = 2 "50-75% FPL") (75/100 = 3 "75-100% FPL") (100/150 = 4 "100-150% of FPL") (150/max = 5 "above 150% of FPL"), gen(fpl_categ_0m)
	tab fpl_categ_0m, gen(fpl_categ_0m_)

	*create distributional dummies for age
	gen by_1959_1988 = birthyear_list>1958
	gen by_1945_1958 = birthyear_list<= 1958

	*create list of variables to use in the balance table 
	local vars "female_list birthyear_list by_1945_1958 by_1959_1988 english_list zip_msa race_white_0m race_black_0m race_hisp_0m  ins_noins_0m ins_ohp_0m ins_medicare_0m ins_privpay_0m ins_othcov_0m ins_months_0m self_list have_phone_list "

	lookfor school smoke_cig employm fpl_categ_0m_
	return list
	local vars `vars' `r(varlist)'

	/* female_list birthyear_list by_1945_1958 by_1959_1988 english_list zip_msa race_white_0m race_black_0m race_hisp_0m race_asian_0m ins_noins_0m ins_ohp_0m ins_medicare_0m ins_privpay_0m ins_othcov_0m ins_months_0m self_list have_phone_list  school1 school2 school3 school4 smoke_cig1 smoke_cig2 employm1 employm2 employm3 employm4 fpl_categ_0m_1 fpl_categ_0m_2 fpl_categ_0m_3 fpl_categ_0m_4 fpl_categ_0m_5
	*/

	foreach var in `vars' {
		
		*control means and sd
		sum `var' if treatment == 0 
		local `var'_avg = string(r(mean), "%4.3f")
		local `var'_cse = string(r(sd), "%4.3f")

		*treatment means and sd
		reg `var' treatment i.hhsize_12m##i.draw_survey_12m i.draw_lottery [pw = weight_12m], vce(cluster household_id)
		local `var'_b = string(_b[treatment], "%10.3fc")
		local `var'_bse = string(_se[treatment], "%10.3fc")
	}

	*CREATE LATEX BALANCE TABLE
	
	di("`fpl_categ_0m_1_avg'")

	cap erase "${output}/TA_Balance.tex" 
	file open fh using "${output}/TA_Balance.tex" , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp]" _n
	"\centering" _n
	"\caption{\textbf{Balance Table of Survey Respondents: Lottery Losers vs Winners Group}}" _n
	"\begin{tabularx}{\linewidth}{lcc}" _n
	"\toprule" _n
	"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Loser Mean (SE)}} & \multicolumn{1}{c}{\textbf{Diff Between Winners and Losers (SE)}}  \\  " _n
	"\midrule " _n
	"\% Female & `female_list_avg'& `female_list_b' \\ " _n
	" &	 (`female_list_cse') & (`female_list_bse') \\ " _n
	"Birth Year	& `birthyear_list_avg'	&`birthyear_list_b' \\ " _n
	"&(`birthyear_list_cse')	&(`birthyear_list_bse') \\ " _n
	"\% Born 1945-1958	& `by_1945_1958_avg'&`by_1945_1958_b' \\ " _n
	"&	`by_1945_1958_cse'&	`by_1945_1958_bse' \\ " _n
	"\% Born 1959-1988	&`by_1959_1988_avg'	&`by_1959_1988_b' \\ " _n
	"&	`by_1959_1988_cse'	&`by_1959_1988_bse'\\ " _n
	"\% English Preferred	& `english_list_avg'	&`english_list_b'\\ " _n
	" & (`english_list_cse') & (`english_list_bse') \\ " _n
	"\% MSA	 &`zip_msa_avg'	&`zip_msa_b' \\" _n
	"&	 `zip_msa_cse'&	`zip_msa_bse' \\" _n 
	"\textbf{Race} & & \\" _n 
	"\% White &  `race_white_0m_avg'&`race_white_0m_b' \\" _n
	"& `race_white_0m_cse'&	`race_white_0m_bse' \\" _n
	"\% Black & `race_black_0m_avg'	&`race_black_0m_b'\\" _n
	" &	 `race_black_0m_cse'	&`race_black_0m_bse'\\" _n
	"\% Hispanic &`race_hisp_0m_avg'&`race_hisp_0m_b'\\" _n
	"& `race_hisp_0m_cse'&`race_hisp_0m_bse'\\" _n
	"\% Asian & `race_asian_0m_avg'	& `race_asian_0m_b' \\" _n
	"& `race_asian_0m_cse'&`race_asian_0m_bse' \\" _n
	"\textbf{Income (\% federal poverty line)} & & \\" _n 
	"$<$ 50\% & `fpl_categ_0m_1_avg' & `fpl_categ_0m_1_b' \\" _n
	"& `fpl_categ_0m_1_cse' & `fpl_categ_0m_1_bse' \\" _n
	"50-75\% & `fpl_categ_0m_2_avg' & `fpl_categ_0m_2_b' \\" _n
	"& `fpl_categ_0m_2_cse' & `fpl_categ_0m_2_bse' \\" _n
	"75-100\% & `fpl_categ_0m_3_avg' & `fpl_categ_0m_3_b' \\" _n
	"& `fpl_categ_0m_3_cse' & `fpl_categ_0m_3_bse' \\" _n
	"100-150\% & `fpl_categ_0m_4_avg' & `fpl_categ_0m_4_b' \\" _n
	"& `fpl_categ_0m_4_cse' & `fpl_categ_0m_4_bse' \\" _n
	"Above 150\% & `fpl_categ_0m_5_avg' & `fpl_categ_0m_5_b' \\" _n
	"& `fpl_categ_0m_5_cse' & `fpl_categ_0m_5_bse' \\" _n	
	"\midrule " _n
	"Observations & 45,088 & 29,834 \\" _n
	"\bottomrule" _n
	"\end{tabularx} " _n
	"\label{tab:addlabel}" _n
	"\end{table}" _n
	;
	#delimit cr
	file close fh     



      
	  
	* Intent to Treat Table - Table IV in paper, column 2


	foreach var in hosp_any_ er_any_ not_er_noner_ { 
		reg `var'12m treatment i.hhsize_12m i.draw_lottery `var'0m [pw = weight_12m], vce(cluster household_id)
		local `var'b = string(_b[treatment], "%10.4fc")
		local `var'se = string(_se[treatment], "%10.4fc")
	}

	*CREATE LATEX ITT TABLE
	cap erase "${output}/TA_ITT.tex" 
	file open fh using "${output}/TA_ITT.tex" , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp!]" _n
	"\centering" _n
	"\caption{\textbf{ITT Effect: Paper Table IV Col (2)}}" _n
	"\begin{tabular}{p{15em}cc}" _n
	"\toprule" _n
	"\textbf{Extensive Margin} & & \\" _n
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
	
		
}		
		
********************************************************************************
*					Switch B: Enrolled Medicaid vs Not      				   *
********************************************************************************

if `p2b'{
	
	use "${data}/data_for_analysis.dta", clear


	*create dummie variables
	tabulate edu_0m, generate(school)
	tab smk_curr_bi, gen(smoke_cig)
	tab employ_hrs_0m, gen(employm)

	* inc as % of FPL
	recode hhinc_pctfpl_0m (min/50 = 1 "below 50% of FPL") (50/75 = 2 "50-75% FPL") (75/100 = 3 "75-100% FPL") (100/150 = 4 "100-150% of FPL") (150/max = 5 "above 150% of FPL"), gen(fpl_categ_0m)
	tab fpl_categ_0m, gen(fpl_categ_0m_)

	*create distributional dummies for age
	gen by_1959_1988 = birthyear_list>1958
	gen by_1945_1958 = birthyear_list<= 1958

	*create list of variables to use in the balance table 
	local vars "female_list birthyear_list by_1945_1958 by_1959_1988 english_list zip_msa race_white_0m race_black_0m race_hisp_0m  ins_noins_0m ins_ohp_0m ins_medicare_0m ins_privpay_0m ins_othcov_0m ins_months_0m self_list have_phone_list "

	lookfor school smoke_cig employm fpl_categ_0m_
	return list
	local vars `vars' `r(varlist)'

	/* female_list birthyear_list by_1945_1958 by_1959_1988 english_list zip_msa race_white_0m race_black_0m race_hisp_0m race_asian_0m ins_noins_0m ins_ohp_0m ins_medicare_0m ins_privpay_0m ins_othcov_0m ins_months_0m self_list have_phone_list  school1 school2 school3 school4 smoke_cig1 smoke_cig2 employm1 employm2 employm3 employm4 fpl_categ_0m_1 fpl_categ_0m_2 fpl_categ_0m_3 fpl_categ_0m_4 fpl_categ_0m_5
	*/
	
	foreach var in `vars' {
		
		*no medicaid means and sd
		sum `var' if ohp_all_at_12m  == 0 
		local `var'_avg = string(r(mean), "%4.3f")
		local `var'_cse = string(r(sd), "%4.3f")

		*medicaid means and sd
		*survey weights
		reg `var' ohp_all_at_12m i.hhsize_12m##i.draw_survey_12m i.draw_lottery if ohp_all_at_12m!=. [pw = weight_12m], vce(cluster household_id)
		local `var'_b = string(_b[ohp_all_at_12m], "%10.3fc")
		local `var'_bse = string(_se[ohp_all_at_12m], "%10.3fc")
	}

	*CREATE LATEX BALANCE TABLE
	
	di("`fpl_categ_0m_1_avg'")

	cap erase "${output}/TB_Balance.tex" 
	file open fh using "${output}/TB_Balance.tex" , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp]" _n
	"\centering" _n
	"\caption{\textbf{Balance Table of Not Enrolled vs Enrolled in Medicaid}}" _n
	"\begin{tabularx}{\linewidth}{lcc}" _n
	"\toprule" _n
	"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Not Enrolled Mean (SE)}} & \multicolumn{1}{c}{\textbf{Diff Between Enrolled and Not (SE)}}  \\  " _n
	"\midrule " _n
	"\% Female & `female_list_avg'& `female_list_b' \\ " _n
	" &	 (`female_list_cse') & (`female_list_bse') \\ " _n
	"Birth Year	& `birthyear_list_avg'	&`birthyear_list_b' \\ " _n
	"&(`birthyear_list_cse')	&(`birthyear_list_bse') \\ " _n
	"\% Born 1945-1958	& `by_1945_1958_avg'&`by_1945_1958_b' \\ " _n
	"&	`by_1945_1958_cse'&	`by_1945_1958_bse' \\ " _n
	"\% Born 1959-1988	&`by_1959_1988_avg'	&`by_1959_1988_b' \\ " _n
	"&	`by_1959_1988_cse'	&`by_1959_1988_bse'\\ " _n
	"\% English Preferred	& `english_list_avg'	&`english_list_b'\\ " _n
	" & (`english_list_cse') & (`english_list_bse') \\ " _n
	"\% MSA	 &`zip_msa_avg'	&`zip_msa_b' \\" _n
	"&	 `zip_msa_cse'&	`zip_msa_bse' \\" _n 
	"\textbf{Race} & & \\" _n 
	"\% White &  `race_white_0m_avg'&`race_white_0m_b' \\" _n
	"& `race_white_0m_cse'&	`race_white_0m_bse' \\" _n
	"\% Black & `race_black_0m_avg'	&`race_black_0m_b'\\" _n
	" &	 `race_black_0m_cse'	&`race_black_0m_bse'\\" _n
	"\% Hispanic &`race_hisp_0m_avg'&`race_hisp_0m_b'\\" _n
	"& `race_hisp_0m_cse'&`race_hisp_0m_bse'\\" _n
	"\% Asian & `race_asian_0m_avg'	& `race_asian_0m_b' \\" _n
	"& `race_asian_0m_cse'&`race_asian_0m_bse' \\" _n
	"\textbf{Income (\% federal poverty line)} & & \\" _n 
	"$<$ 50\% & `fpl_categ_0m_1_avg' & `fpl_categ_0m_1_b' \\" _n
	"& `fpl_categ_0m_1_cse' & `fpl_categ_0m_1_bse' \\" _n
	"50-75\% & `fpl_categ_0m_2_avg' & `fpl_categ_0m_2_b' \\" _n
	"& `fpl_categ_0m_2_cse' & `fpl_categ_0m_2_bse' \\" _n
	"75-100\% & `fpl_categ_0m_3_avg' & `fpl_categ_0m_3_b' \\" _n
	"& `fpl_categ_0m_3_cse' & `fpl_categ_0m_3_bse' \\" _n
	"100-150\% & `fpl_categ_0m_4_avg' & `fpl_categ_0m_4_b' \\" _n
	"& `fpl_categ_0m_4_cse' & `fpl_categ_0m_4_bse' \\" _n
	"Above 150\% & `fpl_categ_0m_5_avg' & `fpl_categ_0m_5_b' \\" _n
	"& `fpl_categ_0m_5_cse' & `fpl_categ_0m_5_bse' \\" _n	
	"\midrule " _n
	"Observations & 53,641 & 22,482 \\" _n
	"\bottomrule" _n
	"\end{tabularx} " _n
	"\label{tab:addlabel}" _n
	"\end{table}" _n
	;
	#delimit cr
	file close fh     
	
}

********************************************************************************
*					Switch C: OLS Utilization on Coverage		   			   *
********************************************************************************

if `p2c'{
	use "${data}/data_for_analysis.dta", clear

	local util_vars rx_any_12m doc_any_12m er_any_12m hosp_any_12m rx_num_mod_12m doc_num_mod_12m er_num_mod_12m hosp_num_mod_12m

	local coverage_vars ohp_std_ever_survey ohp_all_mo_survey


	* Regressions 
	foreach cov in `coverage_vars' {
			if "`cov'" == "ohp_std_ever_survey"{ 
				local short "ohp"
			} 
			if "`cov'" == "ohp_all_mo_survey" { 
				local short "any"
			} 
		foreach util_var in `util_vars' { 
			reg `util_var' `cov' [pw = weight_12m], vce(cluster household_id)
			local `short'_`util_var'_b = string(_b[`cov'], "%10.4fc")
			local `short'_`util_var'_se = string(_se[`cov'], "%10.4fc")
			local `short'_`util_var'_n = string(e(N), "%10.0fc")
		}
	}

	*\multicolumn{2}{r}{} & \multicolumn{4}{c}{\textbf{Treatment Less Comparison Earnings}} & \multicolumn{2}{c}{\textbf{Diff in Diff}} & \multicolumn{2}{c}{\textbf{Unrestricted DiD}} 
	di("`ohp_rx_num_mod_12m_n'")

	cap erase "${output}/TC_Medicaid_OLS.tex" 
	file open fh using "${output}/TC_Medic_OLS.tex" , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp!]" _n
	"\centering" _n
	"\caption{\textbf{OLS: Utilization on Ever- and Number of Months Enrolled in Medicaid}}" _n
	"\begin{tabularx}{\linewidth}{lcccc}" _n
	"\toprule" _n
	"& \multicolumn{2}{c}{Ever OHP Std (SE)} & \multicolumn{2}{c}{Number Months Enrolled (SE)}  \\  " _n
	" & Extensive (any) & Intensive (\#) & Extensive (any) & Intensive (\#) \\" _n
	"\midrule " _n
	"RX & `ohp_rx_any_12m_b' & `ohp_rx_num_mod_12m_b' & `any_rx_any_12m_b' & `any_rx_num_mod_12m_b'  \\ " _n
	"& (`ohp_rx_any_12m_se') & (`ohp_rx_num_mod_12m_se') & (`any_rx_any_12m_se') & (`any_rx_num_mod_12m_se')  \\ " _n
	"Primary care visits & `ohp_doc_any_12m_b' & `ohp_doc_num_mod_12m_b' & `any_doc_any_12m_b' & `any_doc_num_mod_12m_b'  \\ " _n
	" & (`ohp_doc_any_12m_se') & (`ohp_doc_num_mod_12m_se') & (`any_doc_any_12m_se') & (`any_doc_num_mod_12m_se')  \\ " _n
	"ER visits last 6mo & `ohp_er_any_12m_b' & `ohp_er_num_mod_12m_b' & `any_er_any_12m_b' & `any_er_num_mod_12m_b'  \\ " _n
	" & (`ohp_er_any_12m_se') & (`ohp_er_num_mod_12m_se') & (`any_er_any_12m_se') & (`any_er_num_mod_12m_se')  \\ " _n
	"Inpatient visits, last 6mo & `ohp_hosp_any_12m_b' & `ohp_hosp_num_mod_12m_b' & `any_hosp_any_12m_b' & `any_hosp_num_mod_12m_b'  \\ " _n
	" & (`ohp_hosp_any_12m_se') & (`ohp_hosp_num_mod_12m_se') & (`any_hosp_any_12m_se') & (`any_hosp_num_mod_12m_se')  \\ " _n
	"\midrule " _n
	"Observations & `ohp_rx_any_12m_n' & `ohp_rx_num_mod_12m_n'  & `any_rx_any_12m_n' & `any_rx_num_mod_12m_n' \\" _n
	"\bottomrule" _n
	"\end{tabularx} " _n
	"\label{tab:addlabel}" _n
	"\end{table}" _n
	;
	#delimit cr
	file close fh            

}

********************************************************************************
*						Switch E: First Stage (replicate T3 Cols 5&6)		   *
********************************************************************************

*********************
*DO LAST 3 QUESTIONS*
*********************

if `p2e'{
	use "${data}/data_for_analysis.dta", clear
	local LHS_vars	ohp_std_ever_survey ins_any_12m 

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
"\begin{table}[htbp!]" _n
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

}

********************************************************************************
*						Switch F: Compliers					 				   *
********************************************************************************

if `p2f'{
	use "${data}/data_for_analysis.dta", clear
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

	cap erase "${output}/TF_Compliers.tex" 
	file open fh using "${output}/TF_Compliers.tex"  , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp!]" _n
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


	
}

********************************************************************************
*						Switch G: 2SLS for extensive margin 				   *
********************************************************************************

if `p2g'{
	use "${data}/data_for_analysis.dta", clear
	
	local LHS_vars 	rx_any_12m doc_any_12m er_any_12m hosp_any_12m
		
	local RHS_vars	ohp_std_ever_survey ins_any_12m  


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



	cap erase "${output}/TG_2SLS.tex"  
	file open fh using "${output}/TG_2SLS.tex" , write replace 
	#delimit ;
	file write fh
	"\begin{table}[htbp!]" _n
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
}

