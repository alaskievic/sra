********************************************************************************

clear all

* Load Dataset *
use "../.././data/output/dhs/stata_march4.dta", clear


* Scale caloric measure
replace max_caloric_low  = max_caloric_low/1000000
replace max_caloric_high = max_caloric_high/1000000
replace caloric_diff  	 = caloric_diff/1000000


* Take logs of caloric measure
gen log_caloric_diff = log(caloric_diff)
gen log_caloric_low  = log(max_caloric_low)



* Some regressions
reg share_wom_agri caloric_diff max_caloric_low LAT LON tri_weighted dist_coast_centroid, r


* Controlling for baseline low caloric yields and geographic features
reg share_wom_agri log_caloric_diff log_caloric_low  LAT LON tri_weighted dist_coast_centroid, r

* Adding historical dependence in agriculture from EA
reg share_wom_agri log_caloric_diff log_caloric_low  LAT LON tri_weighted dist_coast_centroid i.EA005, r





