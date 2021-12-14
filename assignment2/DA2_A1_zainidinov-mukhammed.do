* ASSIGNMENT 2

* by Zainidinov Mukhammed


********************************************************************************
********************************************************************************


* PART 1

cd "C:\Users\user\Documents\Mukhammed\assignment2"
global data_in "data\raw"
global data_out "data\clean"
log using "DA2_A1_zainidinov-mukhammed.log", replace

* Data cleaning
use "$data_in\hotels-europe_features.dta", clear
tab city
keep if city=="Paris"
tab city_actual
keep if city_actual=="Paris"
tab accommodation_type
keep if accommodation_type=="Hotel" | accommodation_type=="Hostel"

save "$data_out\hotels-paris_features.dta", replace

merge 1:m hotel_id using "$data_in\hotels-europe_price.dta"
keep if _merge==3
drop _merge
tabstat price, by(month) s(n mean sd min max)
keep if month == 11 & weekend == 0 & year == 2017
tab nnights, m
tab price, m
drop if price>789
duplicates report
duplicates drop
sort city hotel_id distance stars rating year month weekend holiday
gen dupl=1 == (city==city[_n-1] & hotel_id==hotel_id[_n-1] & distance==distance[_n-1] ///
 & stars==stars[_n-1] & rating==rating[_n-1] & price==price[_n-1] & year==year[_n-1] & month==month[_n-1] & weekend==weekend[_n-1] & holiday==holiday[_n-1] )
tab dupl
drop if dupl==1
drop dupl
order hotel_id

* Variable labeling
lab var hotel_id "Hotel ID"
lab var country "Country"
lab var city_actual "City actual of hotel"
lab var rating_count "Number of user ratings"
lab var distance "Distance - from main city center"
lab var center1label "Centre 1 - name of location for distance"
lab var distance_alter "Distance - alternative - from Centre 2"
lab var center2label "Centre 2 - name of location for distance_alter"
lab var neighbourhood "Neighbourhood"
lab var city "City based on search"
lab var stars "Number of stars"
lab var ratingta "User rating average (tripadvisor)"
lab var ratingta_count "Number of user ratings (tripadvisor)"
lab var accommodation_type "Type of accommodation"
lab var rating "User rating average"
lab var price "Price in EUR"
lab var scarce_room "Flag, if a room was noted as scarce"
lab var offer "Flag, if there was an offer available"
lab var offer_cat "Type of other"
lab var year "Year (YYYY)"
lab var month "Month (MM)"
lab var weekend " Flag, if day is a weekend"
lab var holiday "Flag, if day is a holiday"
lab var nnights "Number of nights (1 or 4)"

save "$data_out\hotels-paris.dta", replace

* Data description
des price distance rating stars
tabstat price distance rating stars, s(n mean median sd min max) ///
	columns(statistics) save 

hist price, width(20) fraction fcolor(ltblue) lcolor(black) ///
	xlab(0(100)800, grid) ///
	title(Figure 1. Price Distribution of Hotels & Hostels in Paris)
graph export "hist_price.png", replace

hist distance, width(0.3) frequency fcolor(ltblue) lcolor(black) ///
	xlab(0(0.5)5, grid) ///
	title(Figure 2. Distance Distribution of Hotels & Hostels in Paris)
graph export "hist_distance.png", replace

hist rating, width(0.3) percent fcolor(ltblue) lcolor(black) ///
	xlab(0(0.5)5, grid) ///
	title(Figure 3. Rating Distribution of Hotels & Hostels in Paris)
graph export "hist_rating.png", replace

hist stars, width(1) start(0.5) fraction fcolor(ltblue) lcolor(black) ///
	xlab(1(1)5, grid) ///
	title(Figure 4. Stars Distribution of Hotels in Paris)
graph export "hist_stars.png", replace


********************************************************************************
********************************************************************************


* PART 2

* 1. Estimate a lowess nonparametric regression of price on distance

use "$data_out\hotels-paris.dta", clear
lowess price distance
lowess price distance, bwidth(0.1)
lowess price distance, bwidth(0.5)
lowess price distance, bwidth(0.99)
lowess price distance , bwidth(0.8) ///
	xlab(0(0.5)4.5, grid) yscale(range(0 800)) ylab(000(100)800, grid) ///
	xtitle("Distance to city center (miles)") ///
	ytitle("Hotel price (EUR)") title("") note("") ///
	graphregion(fcolor(white) ifcolor(none))  ///
	plotregion(fcolor(white) ifcolor(white)) ///
	title(Figure 5. Lowess Regression of Price on Distance)
graph export "lowess.png", as(png) replace


* 2. Estimate a simple linear regression of price on distance

ssc instal outreg2
reg price distance, r
outreg2 using "reg1.xls", label bdec(2) excel replace
scatter price distance , ///
 xlab(0(1)4.5, grid) ylab(000(100)800, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price(EUR)") ///
 || lfit price distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 6. Simple Linear Regression)
graph export "simple_level-level.png", as(png) replace


* 3. Estimate a linear regression of price on distance that captures potential nonlinearities (polynomials, splies).

* Log-level simple regression
gen lnprice=ln(price)
lab var lnprice "Natural logaritm of price"
reg lnprice distance, r
scatter lnprice distance , ///
 xlab(0(1)4.5, grid) ylab(3.5(0.50)7, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price (EUR) in log") ///
 || lfit lnprice distance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 7. Log-level Regression)
graph export "simple_log-level.png", as(png) replace

* Level-log simple regression
sum distance	// All values are above 0, min value is 0.1. So we can generate log for distance without adding 0.1 to distance variable
gen lndistance=ln(distance)
lab var lndistance "Natural logarithm of distance"
reg price lndistance, r
scatter price lndistance , ///
 xlab(-2.3(0.5)1.6, grid) ylab(000(100)800, grid) ///
 xtitle("Distance to city center (miles) in log") ///
 ytitle("Hotel price (EUR)") ///
 || lfit price lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 8. Level-Log Regression)
graph export "level-log.png", as(png) replace

* Log-log simple regression
reg lnprice lndistance, r
outreg2 using "reg1.xls", label bdec(2) excel append
scatter lnprice lndistance , ///
 xlab(-2.3(0.5)1.6, grid) ylab(3.5(0.50)7, grid) ///
 xtitle("Distance to city center (miles) in log") ///
 ytitle("Hotel price (EUR) in log") ///
 || lfit lnprice lndistance, lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 9. Log-Log Regression)
graph export "log-log.png", as(png) replace

* Multiple regression with quadratic function of distance
gen distance2=distance^2
lab var distance2 "Squared distance"
reg price distance distance2, r
outreg2 using "reg1.xls", label bdec(2) excel append
predict pred_price_quad
scatter price distance, ///
 xlab(0(1)4.5, grid) ylab(000(100)800, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price (EUR)") ///
 || line pred_price_quad distance, sort lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 10. Regression with quadratic polynomial)
graph export "quadratic.png", as(png) replace

* Multiple regression with quadratic and cubic functions of distance
gen distance3=distance^3
lab var distance3 "Cubed distance"
reg price distance distance2 distance3, r
predict pred_price_poly
scatter price distance, ///
 ms(O) msize(tiny) mlw(thick) mcolor(navy) mcolor(%50) ///
 xlab(0(1)4.5, grid) ylab(000(100)600, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price (EUR)") ///
 || line pred_price_poly distance, sort lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white))
graph export "polynomial.png", as(png) replace

* Spline
cap drop distance_*
cap drop pred_price_spline
mkspline distance_1 1 distance_2 3 distance_3= distance
regress price distance_*, r
predict pred_price_spline
scatter price distance , ///
 xlab(0(1)4.5, grid) ylab(000(100)800, grid) ///
 xtitle("Distance to city center (miles)") ///
 ytitle("Hotel price (EUR)") ///
 || line pred_price_spline distance, sort lw(thick) lc(dkgreen) legend(off)  ///
 graphregion(fcolor(white) ifcolor(none))  ///
 plotregion(fcolor(white) ifcolor(white)) ///
 title(Figure 11. Piecewise linear spline regression)
graph export "spline.png",replace


********************************************************************************
********************************************************************************


* PART 3

* Multiple regression of price on distance, stars, rating
reg price distance stars rating, r
outreg2 using "reg2.xls", label bdec(2) excel replace

* Multiple regression of price on distance, non-linear stars of stars and rating
gen stars2=stars^2
gen rating2=rating^2
lab var stars2 "Squared stars"
lab var rating2 "Squared rating"
reg price distance stars stars2 rating rating2, r
outreg2 using "reg2.xls", label bdec(2) excel append

* Multiple regression with interaction term
gen four_stars=.
keep if accommodation_type=="Hotel" & (stars==3 | stars==3.5 | stars==4)
replace four_stars=1 if stars==4
replace four_stars=0 if stars==3 | stars==3.5
lab def four_stars 0 "3 or 3.5 stars" 1 "4 stars"
lab val four_stars four_stars
gen ratingXfour_stars=rating*four_stars
lab var four_stars "Indicator: 1 if hotel has 4 stars, 0 if hotel has 3 or 3.5 stars"
lab var ratingXfour_stars "Interaction term rating*4stars"

reg price distance rating four_stars ratingXfour_stars, r
outreg2 using "reg2.xls", label bdec(2) excel append

log close
