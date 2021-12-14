* The purpose is to measure the share of completed successful projects by World Bank

version 14

cd "D:/Mukhammed/CEU/Coding/assignment/ieg_rating_in_stata"

global raw "data/raw"
global clean "data/clean"

import delimited "$raw/ieg_rating.csv", clear

* Data cleaning
keep projectid projectname region countrycode countryname sectorboard lendingprojectcost netcommitment exitfy ieg_outcome
	* Keep only necessary variables for analysis

rename exitfy year
rename sectorboard sector
lab var ieg_outcome "IEG Outcome"

duplicates report projectid
duplicates drop projectid, force // Drop dublicated observations

drop if year == 0 // Year cannot be 0
gen rate = . // Generate missing values for rate
lab var rate "1-6 Scale Rating"
replace rate = 1 if ieg_outcome == "Highly Unsatisfactory"
replace rate = 2 if ieg_outcome == "Unsatisfactory"
replace rate = 3 if ieg_outcome == "Moderately Unsatisfactory"
replace rate = 4 if ieg_outcome == "Moderately Satisfactory"
replace rate = 5 if ieg_outcome == "Satisfactory"
replace rate = 6 if ieg_outcome == "Highly Satisfactory"
drop if rate == . // Drop unevaluated projects

save "$clean/ieg_rating.dta", replace

* Analyze the trends in successful profects of World Bank by region

* Seperate by region
use "$clean/ieg_rating.dta", clear

keep region year ieg_outcome rate // Keep only necessary variables for trend analysis
drop if region == "OTH" // I am interested in undefined regions
egen regionid = group(region) // Generate ID for regions
lab var regionid "Region ID"
rename region regioncode
lab var regioncode "Region Code"

gen region = "Africa" if regioncode == "AFR" //Generate a variable with full names of regions
replace region = "Africa East" if regioncode == "AFE"
replace region = "Africa West" if regioncode == "AFW"
replace region = "East Asia and Pacific" if regioncode == "EAP"
replace region = "Europe and Central Asia" if regioncode == "ECA"
replace region = "Latin America and Caribbean" if regioncode == "LCR"
replace region = "Middle East and North Africa" if regioncode == "MNA"
replace region = "South Asia" if regioncode == "SAR"
lab var region "Region"
order region

drop if year < 1972 // There are a few observations till 1972
order regionid, after(region)
sort regionid year

replace rate = 1 if rate < 4 // Aggregate highly unsatisfactory, unsaticfactory, and moderately unsatisfactory rated projects
replace rate = 2 if rate > 3 // Aggregate moderately satisfactory, saticfactory, and highly satisfactory rated projects in to MS+ projects

save "$clean/ieg_rating_by_region.dta", replace

forvalues i = 1/8 {
	use "$clean/ieg_rating_by_region.dta", clear
	keep if regionid == `i'
	sort year
	count if !missing(rate)
	local N `r(N)'
	levels regioncode, local(regioncodes)
	foreach c of local regioncodes{
		levels region, local(regions)
		foreach n of local regions{
			tab year rate, row nof matcell(freq) matrow(year)
			putexcel A1=("year") B1=("unsatisfactory") C1=("satisfactory") using output/ieg_rating_`c', replace
			putexcel A2=matrix(year) B2=matrix(freq) using output/ieg_rating_`c', modify
			import excel "output/ieg_rating_`c'.xlsx", sheet("Sheet1") firstrow clear
			gen percent = 100*satisfactory/(unsatisfactory+satisfactory)
			twoway (connected percent year, lcolor(eltblue) lwidth(medium) lpattern(solid))(lfit percent year, lwidth(medium)), legend(order(1 "% of MS+ Rated Projects" 2 "Trend Line")) ytitle(Percent) xtitle(Year) title("`n', N = `N'")
			}
		graph export output/ieg_rating_`c'.png, replace
		}
}


* Analyze the trends in successful profects of World Bank by sector

* Seperate by sector
use "$clean/ieg_rating.dta", clear

keep sector year ieg_outcome rate // Keep only necessary variables for trend analysis
egen id = group(sector) // Generate ID for regions
drop if id == . // Remove observations for unidentified sectors
drop if year < 1972 // Drop years with a few observations

* Drop sectors with few observations
drop if id == 2 | id == 3 | id == 8 | id == 9 | id == 11 | id == 13 | id > 15 & id < 19 | id > 19 & id < 22
drop id
egen id = group(sector) // We should have ID numbers in sequence
lab var id "Sector ID"
order id, after(sector)
sort id year

replace rate = 1 if rate < 4 // Aggregate highly unsatisfactory, unsaticfactory, and moderately unsatisfactory rated projects
replace rate = 2 if rate > 3 // Aggregate moderately satisfactory, saticfactory, and highly satisfactory rated projects in to MS+ projects

save "$clean/ieg_rating_by_sector.dta", replace

forvalues i = 1/16 {
	use "$clean/ieg_rating_by_sector.dta", clear
	keep if id == `i'
	sort year
	count if !missing(rate)
	local N `r(N)'
	levels sector, local(sectors)
	foreach c of local sectors{
		tab year rate, row nof matcell(freq) matrow(year)
		putexcel A1=("year") B1=("unsatisfactory") C1=("satisfactory") using output/ieg_rating_sector`i', replace
		putexcel A2=matrix(year) B2=matrix(freq) using output/ieg_rating_sector`i', modify
		import excel "output/ieg_rating_sector`i'.xlsx", sheet("Sheet1") firstrow clear
		gen percent = 100*satisfactory/(unsatisfactory+satisfactory)
		twoway (connected percent year, lcolor(eltblue) lwidth(medium) lpattern(solid))(lfit percent year, lwidth(medium)), legend(order(1 "% of MS+ Rated Projects" 2 "Trend Line")) ytitle(Percent) xtitle(Year) title("`c', N = `N'")
		graph export output/ieg_rating_sector`i'.png, replace
		}
}
