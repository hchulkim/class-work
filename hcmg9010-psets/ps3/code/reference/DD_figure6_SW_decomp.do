cd "C:\Users\atulgup\Dropbox (Penn)\teaching\hcmg901\2021-spring\Problem_Sets\ps3"
set more off
set seed 4302015

*table 2: all pairs
use divorce_example if year>=1964 & sex==2, clear
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
graph export "f6_SW_decomp.png", replace as(png)		

