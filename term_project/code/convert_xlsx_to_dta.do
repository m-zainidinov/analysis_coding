args name varlabel

import excel "$raw/`name'.xlsx", sheet("Data") firstrow clear

keep CountryName CountryCode Time Value // Keep only needed variables

rename CountryName country_name
rename CountryCode country_code
rename Time year
rename Value `name'

lab var country_name "Country name"
lab var country_code "Country code"
lab var year "Year"
lab var `name' "`varlabel'"

save "$raw/`name'.dta", replace
