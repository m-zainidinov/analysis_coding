********************************************************************************

* * * * *            EFFECT OF EDUCATION ON ECONOMIC GROWTH            * * * * *

********************************************************************************

version 14

****			DATA CLEANING
********************************************************************************

* Set working directory
cd "D:/Mukhammed/CEU/Data Analysis 4/term_project"

global raw "data/raw"
global clean "data/clean"


**	Convert each excel file into dta format

* Data on GDP per capita downloaded from World Development Indicators: https://databank.worldbank.org/source/world-development-indicators
do code/convert_xlsx_to_dta.do gdppc "GDP per capita PPP (constant 2017 international $)"

* Data on years of schooling downloaded from Our World in Data: https://ourworldindata.org/grapher/mean-years-of-schooling-1
do code/convert_xlsx_to_dta.do edu "Average number of years of total schooling across all education levels (for the population aged 25+)"

* Data on income inequality retrieved from World Inequality Database: https://wid.world/data/
do code/convert_xlsx_to_dta.do ineq "Bottom 50% share"


**	Merge recently converted datasets
use "$raw/gdppc.dta", clear

merge 1:1 country_code year using "$raw/edu.dta", nogenerate
merge 1:1 country_code year using "$raw/ineq.dta", nogenerate

gen lngdppc = ln(gdppc) // Take logs
lab var lngdppc "ln GDP per capita PPP (constant 2017 international $)"

save "$raw/merged_edu-gdp.dta", replace


**	Relationship between variables in 2017
use "$raw/merged_edu-gdp.dta", replace

keep if year == 2017

corr lngdppc edu
count if !missing(lngdppc) & !missing(edu) // To estimate number of observations in correlation
local N `r(N)'
scatter lngdppc edu || lfit lngdppc edu, ///
 legend(off) ytitle("ln(GDP per capita PPP)") xtitle("Average years of schooling") ///
 ylab(, grid) xlab(, grid) title("N = `N'")
 graph export "output/scatter_edu-gdp_2017.png", replace

corr lngdppc ineq
count if !missing(lngdppc) & !missing(ineq) // To estimate number of observations in correlation
local N `r(N)'
scatter lngdppc ineq || lfit lngdppc ineq, ///
 legend(off) ytitle("ln(GDP per capita PPP)") xtitle("Bottom 50% share") ///
 ylab(, grid) xlab(, grid) title("N = `N'")
 graph export "output/scatter_ineq-gdp_2017.png", replace

corr edu ineq
count if !missing(edu) & !missing(ineq) // To estimate number of observations in correlation
local N `r(N)' 
scatter edu ineq || lfit edu ineq, ///
 legend(off) ytitle("Average years of schooling") xtitle("Bottom 50% share") ///
 ylab(, grid) xlab(, grid) title("N = `N'")
 graph export "output/scatter_ineq-edu_2017.png", replace


**	Clean
use "$raw/merged_edu-gdp.dta", clear

drop if  year < 1990 | year > 2017 // yearly data on education covers 1990-2017

* Our data covers 28 years from 1990 to 2017

tab year // There is a country for which data covers only the year 1990, we have to drop it
egen year_n = count(year), by(country_code) // To define how many observations in each country
drop if year_n < 28 // Drop countries with observations less than 28
drop year_n // We do not need this variable

* Drop countries where there is no signle observation in one of the following variables
bysort country_code (year) : drop if missing(gdppc[28]) | missing(edu[28]) | missing(ineq[28])

* Drop countries where there are more than or 10 missing values in one of the following variables
bysort country_code (year) : drop if missing(gdppc[10]) | missing(edu[10])

mdesc
/*
Missing values:	gdppc: 	   53
				edu: 	   20
				ineq:	    0

Number of countries:	  136
Number of years:		   28
Number of observations:	 3808
*/

count if !missing(lngdppc) & !missing(edu) & !missing(ineq)
* Number of non-missing observations is 3735

* Encode string variable by generating a new variable to set panel data
encode country_code, gen(c)
order c

save "$clean/clean_edu-gdp.dta", replace


****			ANALYZE THE EFFECT OF EDUCATION ON ECONOMIC GROWTH
********************************************************************************
use "$clean/clean_edu-gdp.dta", clear

xtset c year

* FE regression
xtreg lngdppc edu, fe cluster(c)
 outreg2 using "output/reg_fe.xls", se bdec(4) 2aster nonotes ///
 ctitle("lngdppc") label replace

* FE regression with a potential confounder
xtreg lngdppc edu ineq, fe cluster(c)
 outreg2 using "output/reg_fe.xls", se bdec(4) 2aster nonotes ///
 ctitle("lngdppc, confounder") label append
 
* FE regression with year dummies
xtreg lngdppc edu i.year, fe cluster(c)
 outreg2 using "output/reg_fe.xls", se bdec(4) 2aster nonotes ///
 ctitle("lngdppc, aggreg trend") label keep(edu) append 

* Fixed effect regression with year dummies and potential confounder
xtreg lngdppc edu ineq i.year, fe cluster(c)
 outreg2 using "output/reg_fe.xls", se bdec(4) 2aster nonotes ///
 ctitle("lngdppc, confounder, aggreg trend") label keep(edu ineq) append

 
***		First difference model

* First differencing
gen d_lngdppc = d.lngdppc
gen d_edu = d.edu
gen d_ineq = d.ineq

lab var d_lngdppc "d.lngdppc"
lab var d_edu "d.edu"
lab var d_ineq "d.ineq"

* FD regression
reg d_lngdppc d_edu, cluster(c)
 outreg2 using "output/reg_fd.xls", se bdec(4) 2aster ///
 ctitle("lngdppc") label nonotes replace
more

* FD regression with 5 lags
reg d_lngdppc L(0/5).d_edu, cluster(c)
 outreg2 using "output/reg_fd.xls", se bdec(4) 2aster ///
 ctitle("lngdppc, 5 lags") label nonotes append
more

* FD regression with cumulative 5 lags
gen d2_edu = d.d_edu
reg d_lngdppc L5.d_edu L(0/4).d2_edu, cluster(c)
 outreg2 using "output/reg_fd.xls", se bdec(4) 2aster ///
 ctitle("lngdppc, cum 5 lags") label nonotes drop(L(0/4).d2_edu) append

* FD regression with cumulative 5 lags and 5 leads
reg d_lngdppc L5.d_edu L(0/4).d2_edu F(1/5).d_edu, cluster(c)
 outreg2 using "output/reg_fd.xls", se bdec(4) 2aster ///
 ctitle("lngdppc, cum 5 lags, 5 leads") label nonotes drop(L(0/4).d2_edu) append
more

* FD regression with cumulative 5 lags and aggregate trend
reg d_lngdppc L5.d_edu L(0/4).d2_edu i.year, cluster(c)
 outreg2 using "output/reg_fd_t.xls", se bdec(4) 2aster ///
 ctitle("lngdppc, cum 5 lags, aggreg trend") label keep(L5.d_edu) nonotes replace
more

* FD regression with confounder, cumulative 5 lags, and aggregate trend
reg d_lngdppc L5.d_edu L(0/4).d2_edu L(0/5).d_ineq i.year, cluster(c)
 outreg2 using "output/reg_fd_t.xls", se bdec(4) 2aster ///
 ctitle("lngdppc, cum 5 lags, confounder, aggreg trend") nonotes label keep(L5.d_edu) append
more

* FD regression with confounder, cumulative 5 lags, aggregate trend, and country linear trend
areg d_lngdppc L5.d_edu L(0/4).d2_edu L5.d_ineq i.year, cluster(c) absorb(c)
 outreg2 using "output/reg_fd_t.xls", se bdec(4) 2aster nonotes ///
 ctitle("lngdppc, cum 5 lags, confounder, aggreg trend, country trend") label keep(L5.d_edu) append
more
