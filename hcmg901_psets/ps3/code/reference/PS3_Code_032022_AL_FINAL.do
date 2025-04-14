
/*******************************************************************************
								BASIC SET UP
*******************************************************************************/		
clear all 
set more off 
cap log close 
version 16 

*set globals
if c(username)=="ashle" {
	global drive "C:\Users\ashle\Dropbox\Wharton\2_Courses\HCMG 901\3_Assignments\PS3"
} 

* set styles
set scheme s1mono
graph set window fontface "Times New Roman"

*set other locals
local lags "lag0 lag12 lag34 lag56 lag78 lag910 lag1112 lag1314 lag1516 lag1718 lag19plus"

*download required packages 
cap ssc install bacondecomp
/*******************************************************************************
							Q2: SUMMARY STATS 
*******************************************************************************/
use "$drive/DivorceData(QJE)/Til Death MASTER DATA streamlined.dta", clear
rename unilateral uni 
rename suiciderate_elast_jag suic
foreach var in uni suic {
	forvalues i=1/2 {
		sum `var' if sex == `i'
		local `var'_`i'_mean = string(r(mean), "%4.2f")
		local `var'_`i'_sd = string(r(sd), "%4.2f")
		local `var'_`i'_obs = string(r(N), "%4.0f")
	}
}
count if uni != . 
local uni_obs = string(r(N), "%4.0f")

/*Export to Latex */ 
cap erase "$drive/Q2_SumStats.tex" 
file open fh using "$drive/Q2_SumStats.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Summary Statistics}}" _n
"\begin{tabular}{p{15em}cccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Mean}} & \multicolumn{1}{c}{\textbf{SD}} & \multicolumn{1}{c}{\textbf{N}}  \\  " _n
"\midrule " _n
"Suicide Rate & & & \\ " _n
"\indent \textit{Women} & `suic_2_mean' & `suic_2_sd' & `suic_2_obs' \\ " _n
"\indent \textit{Men} & `suic_1_mean' & `suic_1_sd' & `suic_1_obs' \\ " _n
"Unilateral Divorce & `uni_2_mean' & `uni_2_sd' & `uni_obs'  \\ " _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh 
/*******************************************************************************
						Q3: TABLE 1, COLS 1F, 1M 
*******************************************************************************/
use "$drive/DivorceData(QJE)/Til Death MASTER DATA streamlined.dta" if (year >= 1964 & year <= 1996), clear
encode st, gen(st_num)
gen diff = year - divlaw
gen lag12 = inrange(diff, 1,2)
gen lag34 = inrange(diff, 3,4)
gen lag56 = inrange(diff, 5,6)
gen lag78 = inrange(diff, 7,8)
gen lag910 = inrange(diff, 9,10)
gen lag1112 = inrange(diff, 11,12)
gen lag1314 = inrange(diff, 13,14)
gen lag1516 = inrange(diff, 15,16)
gen lag1718 = inrange(diff, 17,18)
gen lag19plus = diff >= 19 & diff != . 

forvalues i=1/2 {
	regress suiciderate_elast_jag `lags' i.st_num i.year if sex==`i', robust
		foreach var in `lags' {
			gen `var'_b = round(_b[`var']*100, 0.01) 
			sum `var'_b 
			local `var'_`i'_b = string(r(mean), "%4.1f")
			gen `var'_se = round(_se[`var']*100, 0.01)
			sum `var'_se
			local `var'_`i'_se = string(r(mean), "%4.1f")
			cap drop `var'_b `var'_se
		}
	local obs_`i' = string(e(N), "%9.0fc")
}

/*Export to Latex */ 
cap erase "$drive/Q3_Table1.tex" 
file open fh using "$drive/Q3_Table1.tex" , write replace 
#delimit ;
file write fh
"\begin{table}[htbp]" _n
"\centering" _n
"\caption{\textbf{Replication of Table 1}}" _n
"\begin{tabular}{p{15em}ccc}" _n
"\toprule" _n
"\multicolumn{1}{r}{} & \multicolumn{1}{c}{\textbf{Female suicides}} & \multicolumn{1}{c}{\textbf{Male suicides}}  \\  " _n
"Column no. & (1f) & (1m) \\" _n
"\midrule " _n
"Year of Change & `lag0_2_b'\% & `lag0_1_b'\%   \\ " _n
" & (`lag0_2_se') & (`lag0_1_se')  \\ " _n
"1-2 years later & `lag12_2_b'\% & `lag12_1_b'\%  \\ " _n
" & (`lag12_2_se') & (`lag12_1_se')  \\ " _n
"3-4 years later & `lag34_2_b'\% & `lag34_1_b'\%   \\ " _n
" & (`lag34_2_se') & (`lag34_1_se') \\ " _n
"5-6 years later & `lag56_2_b'\% & `lag56_1_b'\% \\ " _n
" & (`lag56_2_se') & (`lag56_1_se')   \\ " _n
"7-8 years later & `lag78_2_b'\% & `lag78_1_b'\%  \\ " _n
" & (`lag78_2_se') & (`lag78_1_se')  \\ " _n
"9-10 years later & `lag910_2_b'\% & `lag910_1_b'\%  \\ " _n
" & (`lag910_2_se') & (`lag910_1_se')  \\ " _n
"11-12 years later & `lag1112_2_b'\% & `lag1112_1_b'\%  \\ " _n
" & (`lag1112_2_se') & (`lag1112_1_se')   \\ " _n
"13-14 years later & `lag1314_2_b'\% & `lag1314_1_b'\%   \\ " _n
" & (`lag1314_2_se') & (`lag1314_1_se')  \\ " _n
"15-16 years later & `lag1516_2_b'\% & `lag1516_1_b'\%   \\ " _n
" & (`lag1516_2_se') & (`lag1516_1_se')  \\ " _n
"17-18 years later & `lag1718_2_b'\% & `lag1718_1_b'\%   \\ " _n
" & (`lag1718_2_se') & (`lag1718_1_se')  \\ " _n
">= 19 years later & `lag19plus_2_b'\% & `lag19plus_1_b'\%   \\ " _n
" & (`lag19plus_2_se') & (`lag19plus_1_se')  \\ " _n
"\midrule " _n
"Observations & `obs_2' & `obs_1' & \\" _n
"\bottomrule" _n
"\end{tabular} " _n
"\label{tab:addlabel}" _n
"\end{table}" _n
;
#delimit cr
file close fh 
/*******************************************************************************
						Q4: Figure 5, GB2021 
*******************************************************************************/
use "$drive/divorce_example.dta" if year>=1964, clear
forvalues i = 1/2{
preserve 
	keep if sex == `i'
	gen post = year>=_nfd
	replace post = 1 if nfd=="PRE"
	replace asmrs = asmrs*10

	reg asmrs i.stfips i.year post, robust
	local DD: di %03.2f _b[post]
	local DDSE: di %03.2f _se[post]
	local DD1 = `DD' - 1

	reg asmrs i.stfips i.year _Texp*, robust // note: _Texp* is dummy event time 
	regsave _Texp*, ci

	drop N r2
	gen event_time = _n
	replace event_time = event_time - 9
	gen sex = `i'
	label define sex_lab 1 "Men" 2 "Women"
	label values sex sex_lab
	local gr_lab: label (sex) `i' 
	
	#delimit ; 
	twoway (scatter coef ci_lower ci_upper event_time,
	lpattern(solid dash dash dot dot solid solid) 
			lcolor(gray gray gray red blue) 
			lwidth(thick medium medium medium medium thick thick)
			msymbol(i i i i i i i i) msize(medlarge medlarge)
			mcolor(gray black gray gray red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black) lpattern(solid))
			yline(0, lcolor(black)) 
			xlabel(-8 -4 -1 3 7 11 15, labsize(medium))
			ylabel(, nogrid labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Years Relative to Divorce Reform", size(medium))
			ytitle("Suicides per 1m `gr_lab'", size(medium))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero))
			yline(`DD', lcolor(red) lwidth(thick)) 
			text(`DD1' -2.55 "DD Coefficient = `DD' (s.e. = `DDSE')")
			)
			;
	#delimit cr;
	graph export "$drive/Q4_Fig5_`i'.pdf", replace as(pdf)
restore 	
}
/*******************************************************************************
						Q6: Figure 6, GB2021 
*******************************************************************************/
use "$drive/divorce_example.dta" if year>=1964 & sex == 2, clear
gen post = year>=_nfd
replace post = 1 if nfd=="PRE"
replace asmrs = asmrs*10
keep asmrs year stfips post  
xtset stfips year
#delimit ;
bacondecomp asmrs post, ddline(lcolor(red)) gropt(ylabel(-30(10)30) 
note("Overall DD Estimate: -3.08"
 "Timing Groups: 2.42 (_b), 0.38 (weight)"
 "Always v. Timing: -7.04 (_b), 0.38 (weight)" 
 "Never v. Timing: -5.33 (_b), 0.24 (weight)"))
 ;
 #delimit cr; 
graph export "$drive/Q6_Fig6.pdf", replace as(pdf)