********************************************************************************
*								  Problem Set 3								   *
*								 Stephanie Grove							   *
* 
********************************************************************************
********************************************************************************


clear all 
set more off 
mac drop _all
cap log close 
version 17
set seed 99


if c(username)=="Steph" {
	global drive "/Users/Steph/Dropbox (Penn)/coursework/HCMG_901_personal/PS3"
	global output "/Users/Steph/Dropbox (Penn)/Apps/Overleaf/HCMG- PS3 Grove/figures_and_tables"
} 
else if c(username)=="sgrove" {
	global drive "C:/Users/sgrove/Dropbox (Penn)/coursework/HCMG_901_personal/PS3"
	global output "C:/Users/sgrove/Dropbox (Penn)/Apps/Overleaf/HCMG- PS3 Grove/figures_and_tables/"

}

global data "${drive}/divorce_example.dta"


cap ssc install bacondecomp
cap ssc install estout
cap ssc install reghdfe

********************************************************************************
*								  Set Switches 							       *
********************************************************************************
* set switch to 1 to execute, 0 for off

* q2: homework Q2, create a descriptive statics table with n, mean, //
* sd of suicide rates for men and women and the policy indicator (key explanatory)
local q2 = 0
* q3: homework Q3, replicate table (no controls 1 cols 1f and 1m) of SW2006
local q3 = 0
* q4a: homework Q4a, replicate fig 5 of GB2021, 
local q4 = 0
* q4b: homework Q4b, create equivalent of Q2a for male suicides
* q6: homework Q6, replicate fig 6 in GB2021 using bacondecomp
local q6 = 1

local p2q2=1

use "$data", clear

*seems like date of law change (copied from Goodman Bacon's code because we don't have a codebook)
gen post = year>=_nfd
replace post = 1 if nfd=="PRE"

*replace asmrs = ln(asmrs*10)*100
replace asmrs = asmrs*10

label define sexlabel 1 "male" 2 "female"

label values sex sexlabel

label variable _nfd "Treat Year"

label variable post "Post Treat"


********************************************************************************
* 								   Homework Q2								   *
* create a descriptive statics table with n, mean, sd of suicide rates for men * 
* and women and the policy indicator (key explanatory)						   *
********************************************************************************


/*
	estpost summarize price mpg rep78 foreign
	esttab ., cells("mean sd count") noobs
	
	estpost tabstat price mpg rep78, listwise statistics(mean sd)
	esttab ., cells("price mpg rep78")
	
	estpost tabstat price mpg rep78, listwise statistics(mean sd) columns(statistics)
	
	esttab ., cells("mean(fmt(a3)) sd")
	
		reg `var'  if treatment == 0 & sample_12m_resp==1 [pw = weight_12m]
	local `var'_avg = string(_b[_cons], "%4.3f")
	local `var'_cse = string(e(rmse), "%4.3f")
	
	*/
use "$data", clear

isid year stfips sex

gen post = (year >= _nfd)
replace post = 1 if nfd == "PRE"

keep year stfips sex asmrs post
reshape wide asmrs, i(year stfips post) j(sex)
isid year stfips

local desc_vars post asmrs1 asmrs2
local stats count sd mean

if `q2'{
	
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

label var post   "Post Unilateral Legal"
label var asmrs1 "Suicide Rate Men"
label var asmrs2 "Suicide Rate Women"

format post asmrs? %13.2fc
tostring *, replace force usedisplayformat

foreach var of varlist * {

                replace `var' = subinstr(`var', ".00", "", .)

}


texsave * using "${output}/Q2.tex", varlabels frag replace location("H") ///
     title("Descriptive Statistics")

	
}



********************************************************************************
* 								   Homework Q3								   *
* 			replicate table (no controls 1 cols 1f and 1m) of SW2006		   *
********************************************************************************

if `q3'{
	
	use "$data", clear
	drop if year<1964
	isid year stfips sex
	
	gen years_since = year- _nfd
	replace years_since = 19 if years_since>19
	recode years_since (2=1) (4=3) (6=5) (8=7) (10=9) (12=11) (14=13) (16=15) (18=17)
	replace years_since = 100 if years_since<0

	tab years_since if sex==1

	
	gen post = (year >= _nfd)
	replace post = 1 if nfd == "PRE"

	label define yrssincelabel 0 "Year of Change" 1 "1-2 Years Later"

	label values sex sexlabel

	*for genders
	eststo clear
	forvalues i=1/2 {
		cap drop asmrs_elast_`i'
		sum asmrs if sex==`i', meanonly
		local mean_`i' `r(mean)'
		gen asmrs_elast_`i' = 100* asmrs / `mean_`i''
		if `i' == 1{
			label variable asmrs_elast_`i' "Male Suicide Elast"
			}
		if `i' == 2{
			label variable asmrs_elast_`i' "Female Suicide Elast"
		}
		eststo: reg asmrs_elast_`i' ib100.years_since i.stfips i.year if sex==`i', robust
		*reghdfe asmrs_elast_`i' ib100.years_since if sex==`i', absorb( i.stfips i.year) vce(robust)
	}


	
	esttab est2 est1 using "${output}/Q3.tex" , nostar keep(*.years_since) b(3) se(3) coeflab(0.years_since "Year of Change" 1.years_since "1-2 Years Later" 3.years_since "3-4 Years Later" 5.years_since "5-6 Years Later" 7.years_since "7-8 Years Later" 9.years_since "9-10 Years Later" 11.years_since "11-12 Years Later" 13.years_since "13-14 Years Later" 15.years_since "15-16 Years Later" 17.years_since "17-18 Years Later" 19.years_since "19+ Years Later" 100.years_since "preceding policy change") drop(100.years_since) varwidth(25) noobs replace mtitles("Female" "Male") title("Regression Table of Years Since Policy Change on Suicide Elasticity")stats(F p, fmt(a3 3) labels("F Statistic" "(p value)")) 
}


********************************************************************************
* 								   Homework Q4a								   *
* 							replicate fig 5 of GB2021		 				   *
********************************************************************************


	
	
if `q4'{
use "$data", clear
keep if year>=1964

	forvalues sex = 1/2 {
		preserve
		keep if sex==`sex'

		gen post = year>=_nfd
		replace post = 1 if nfd=="PRE"
		*replace asmrs = ln(asmrs*10)*100
		replace asmrs = asmrs*10

		xi: reg asmrs i.stfips i.year post, robust

		local DDL = _b[post]
		local DD : display %03.2f _b[post]
		local DDSE : display %03.2f _se[post]
		local DD1 = `DD' - 1

		xi: reg asmrs i.stfips i.year _Texp*, robust
		outreg2 using "${output}/sw_replication.xls", replace keep(_T*) noparen noaster addstat(DD, `DD', DDSE, `DDSE')

		/***Get ES Coefs***/
		xmluse "${output}/sw_replication.xls", clear cells(A3:B56) first
		replace VARIABLES = subinstr(VARIABLES,"_Texp_","",.)	
		quietly destring _all, replace ignore(",")
		compress
		drop in 1/3
		*replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
		quietly destring _all, replace ignore(",")
		compress

		ren VARIABLES exp
		gen b = exp<.
		replace exp = exp-10
		local obs = _N
		forval k = 2/`obs'{
			local j = `k'-1
			replace exp = exp[`j'] in `k' if exp[`k']==.
		}

		local obs =_N+1
		set obs `obs'
		for var _all: replace X = 0 in `obs'
		replace b = 1 in `obs'
		replace exp = -1 in `obs'
		keep exp asmrs b 
		reshape wide asmrs, i(exp) j(b)

		cap drop *lb* *ub*
		gen lb = asmrs1 - 1.96*asmrs0 if exp~=-19
		gen ub = asmrs1 + 1.96*asmrs0 if exp~=-19

		local gender1 male
		local gender2 female
		#delimit ;
		twoway (scatter asmrs1 ub lb exp , 
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
				ytitle("Suicides per 1m `gender`sex''", size(medium))
				title("Event study of `gender`sex'' suicide- coef estimates (+/- 1SE) ")
				graphregion(fcolor(white) color(white) icolor(white) margin(zero))
				yline(`DDL', lcolor(red) lwidth(thick)) text(`DD1' -2.55 "DD Coefficient = `DD' (s.e. = `DDSE')")
				)
				;

		#delimit cr;		
		graph export "${output}/f5_SW_replication_`gender`sex''.png", replace as(png) 
		restore
	}
}




********************************************************************************
* 								   Homework Q6								   *
* 					replicate fig 6 in GB2021 using bacondecomp		 		   *
********************************************************************************


if `q6'{
	
		
*table 2: all pairs
use "$data" if year>=1964 & sex==2, clear

* Bacon Decomp using his package

gen post = year>=_nfd
replace post = 1 if nfd=="PRE"
replace asmrs = asmrs*10
keep asmrs year stfips post  
xtset stfips year
#delimit ;
bacondecomp asmrs post, ddline(lcolor(red)) gropt(ylabel(-30(10)30)  ///
note("Late v Early wgt & B: 0.016 & 5.97" "Early v Late wgt & B: 0.0067 & 0.023" "Always treated v timing wgt & B: 0.023 & -5.07" "Never Treated vs timing wgt & B: 0.014 & -3.94")) stub("gb") ddetail 
 ;
 #delimit cr; 
graph export "${output}/Q6_Fig6.pdf", replace as(pdf)

preserve
*collapse (mean) mean_wgt=gbS meanB=gbB , by(gbcgroup)
collapse (sum) sum_wgt=gbS (mean) meanB=gbB , by(gbcgroup)

restore
}

/*
* Bacon Decomp using his code

use "$data" if year>=1964 & sex==2, clear

*drop if stfips==6
gen post = year>=_nfd
replace post = 1 if nfd=="PRE"
replace asmrs = asmrs*10

collapse (mean) post asmrs (count) stfips, by(nfd year)
save "nfd", replace


expand 14
bysort year nfd: gen ind = _n
cap drop nfd2 
gen nfd2 = ""
local i = 1
foreach stub in 1969       1970       1971       1972       1973       1974       1975       1976       1977       1980       1984       1985        NRS        PRE{
		replace nfd2 = "`stub'" if ind==`i'
		local i = `i'+1
}
ren nfd x
ren asmrs x2
ren post x3
ren stfips x4 
ren nfd2 nfd
merge m:1 year nfd using nfd
ren asmrs asmrs2
ren nfd nfd2
ren post post2
ren stfips stfips2
ren x nfd
ren x2 asmrs
ren x3 post
ren x4 stfips 
drop if nfd==nfd2
drop if real(nfd2)==.

*for each pair, which one is treat and which is control
destring nfd, gen(_nfd) force
destring nfd2, gen(_nfd2) force

egen mid = rowtotal(post*)
replace mid = (mid==1)*(_nfd<. & _nfd2<.)


*we just need a single pre/post thing then to collapse by...trying to make asmrs2 be the "treatement" outcome in all comparisons
*we have 1. t/c terms, 2. nfd<nfd2, 3. nfd>nfd2
egen Dbar = mean(post), by(nfd)
egen Dbar2 = mean(post2), by(nfd2)
gen D = post2 
*if "treatment" group is later, drop "control" group pre-period
drop if ~post & _nfd<_nfd2 & _nfd<.
*if "treatment" group is earlier, drop "control" group post-period
drop if post & _nfd2<_nfd& _nfd<.
collapse (mean) asmrs2 asmrs Dbar Dbar2 stfips stfips2, by(nfd nfd2 D)

gen mu = (1-max(Dbar,Dbar2))/(1-abs(Dbar-Dbar2))
replace mu = 1-mu if Dbar2<Dbar
gen s = (stfips/49)*(stfips2/49)*abs(Dbar-Dbar2)*(1-abs(Dbar-Dbar2))
gen wt = s*mu
egen t = total(wt/2)
replace wt = wt/t
replace s = s/t
drop t
reshape wide asmrs asmrs2, i(nfd nfd2) j(D)
gen dT = asmrs21-asmrs20
gen dC = asmrs1-asmrs0
gen DD = dT-dC


egen wLE = total(wt*(real(nfd)<. & real(nfd)<real(nfd2)))
sum wLE
local wLE : display %03.2f r(mean)
sum DD if (real(nfd)<. & real(nfd)<real(nfd2)) [aw=wt]
local DDLE : display %03.2f r(mean)

egen wEL = total(wt*(real(nfd)<. & real(nfd)>real(nfd2)))
sum wEL
local wEL : display %03.2f r(mean)
sum DD if (real(nfd)<. & real(nfd)>real(nfd2)) [aw=wt]
local DDEL : display %03.2f r(mean)

egen wPRE = total(wt*(nfd=="PRE"))
sum wPRE
local wPRE : display %03.2f r(mean)
sum DD if (nfd=="PRE") [aw=wt]
local DDPRE : display %03.2f r(mean)

egen wNRS = total(wt*(nfd=="NRS"))
sum wNRS
local wNRS : display %03.2f r(mean)
sum DD if (nfd=="NRS") [aw=wt]
local DDNRS : display %03.2f r(mean)

sum DD [aw=wt]
local DD = r(mean)
local DDN : display %03.2f r(mean)
local DD1 = `DD'+2.5

twoway ///
scatter DD wt if nfd== "PRE", msym(oh) msize(large) mcolor(black) || scatter DD wt if nfd=="NRS", msym(t) msize(large) mcolor(gray) || ///
scatter DD wt if real(nfd)<. & real(nfd)<real(nfd2), msym(x) msize(large) mcolor(black) || scatter DD wt if real(nfd)<. & real(nfd)>real(nfd2), msym(x) msize(large) mcolor(gray) ///
legend(off) ytitle("2x2 DD Estimate") xtitle("Weight") yline(`DD', lwidth(thick)) ylabel(-30(10)30, nogrid) ///
graphregion(fcolor(white) color(white) icolor(white) margin(small)) plotregion(margin(medsmall)) ///
title("", color(black)) ///
text(`DD1' .09 "{it:DD Estimate = `DDN'}") ///
|| pcarrowi 24 .028 21 .010, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) text(24 .06 "Later Group Treatment vs. Earlier Group Control" "Weight = `wLE'; DD = `DDLE'") ///
|| pcarrowi 24 .028 27 .010, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
|| pcarrowi 24 .028 12 .010, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
///
|| pcarrowi -25 .028 -16 .002, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) text(-26 .06 "Earlier Group Treatment vs. Later Group Control" "Weight = `wEL'; DD = `DDEL'") ///
|| pcarrowi -25 .028 -20 .003, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
///
|| pcarrowi 9 .041 -3.9 .04, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) text(11 .06 "Treatment vs. Non-Reform States" "Weight = `wNRS'; DD = `DDNRS'") ///
|| pcarrowi 9 .041 -4 .067, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
|| pcarrowi 9 .041 -3.9 .02, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
///
|| pcarrowi -15 .105 -9 .108, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) text(-16 .08 "Treatment vs. Pre-1964 Reform States" "Weight = `wPRE'; DD = `DDPRE'") ///
|| pcarrowi -13.2 .064 -10.5 .064, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) ///
|| pcarrowi -15 .056 -11 .042, lcolor(black) mcolor(black) lwidth(thin) mlwidth(thin) 

graph export "${output}/f6_SW_decomp.png", replace as(png)		



*/


if `p2q2'{
	
	
import delimited "${drive}/ES_unbinned.csv", clear
gen periodtemp = substr(term1, 10, 13)
gen strlen= length(periodtemp)
replace periodtemp = substr(periodtemp, 1, strlen-1) if strlen>2
replace periodtemp = substr(periodtemp, 2, length(periodtemp)) if strlen>2
destring periodtemp, gen(period)
drop periodtemp strlen term1 v1
sort period
}