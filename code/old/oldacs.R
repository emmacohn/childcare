* program to get family income and labor force participation from ACS data
set more off
clear all

global code code/
  global data data/
  
  *cpi
sysuse cpi_annual, clear
keep year cpiurs

sum cpiurs if year == 2017
local cpi_2017 = `r(mean)'
tempfile cpi
save `cpi'

*infant care data for affordability standard
import delim ${data}infant_care.csv, clear
gen afford_stnrd = infant_care / 0.07
tempfile affordability
save `affordability'

!gunzip -k ${data}usa_00013.dta.gz
use ${data}usa_00013.dta, clear
replace labforce = . if labforce == 0
gen lfpr = labforce == 2

replace lfpr = . if labforce == .
replace ftotinc = . if ftotinc == 9999998 | ftotinc == 9999999
merge m:1 year using `cpi'
drop if _merge == 2
drop _merge
gen real_faminc = ftotinc * `cpi_2017' / cpiurs

tempfile acs
save `acs'
erase ${data}usa_00013.dta

use `acs', clear
*median family income for families with children under 7
keep if yngch < 6
drop if ftotinc <= 0
sort year serial famunit pernum

bysort year serial famunit: keep if _n == 1
rename statefip state_fips
merge m:1 state_fips using `affordability'

*share of families that can afford child care
gen afford_share = afford_stnrd <= real_faminc
tab state_abr afford_share
tempfile acs_famlies
save `acs_famlies'

gcollapse (mean) afford_share (first) state_fips [pw=perwt], by(state_abr)
tempfile afford_share
save `afford_share'

use `acs_famlies', clear
binipolate real_faminc [pw=perwt], binsize(100) p(50) by(state_fips)
merge 1:1 state_fips using `afford_share'
drop _merge
export excel using ${data}median_faminc_u6.xlsx, replace firstrow(var)

*distribution for how many families can afford 7% affordability standard

*Labor force participation overall
use `acs', clear

gen pop = 1
gcollapse (mean) lfpr (sum) lf_level=lfpr pop [pw=perwt/5], by(statefip)
gen group = "Overall"
tempfile part1
save `part1'

*"maternal" lfpr for women with children under 7
use `acs', clear
gen pop = 1
keep if sex == 2
keep if yngch < 7
gcollapse (mean) lfpr (sum) lf_level=lfpr pop [pw=perwt/5], by(statefip)
gen group = "Mothers u7 kids"
tempfile part2
save `part2'

append using `part1'

export excel using ${data}lfpr.xlsx, replace firstrow(var)
