*******************************************************************************************************************
*
*  File Name:      01.2_merge_and_prep_consistent
*  Author:	       Billy Matthias 
*  Email:	       bmatthias88@gmail.com
*  Purpose:	       (1) merge "00_colombia_locs1-3_merged.csv" w/ Colombia cross-sectional data (colombia-cs)
*				   (2) prep consistent data for analysis
*  Date Used:      2017-05-26
*  Data Used:      (1) "00_colombia_locs1-3_merged.csv" (2) muni_19982014_v09.1_CONSISTENT.csv
*  Data Output:    (1) 
*                  
*
*******************************************************************************************************************

****************************************************************************************************************
*                                             SETTING UP DO FILE											   *
****************************************************************************************************************

* clear old environments
clear

* to stop log close errors
cap log close

* stating version of stata
version 14

* to avoid having to click more for more results
set more off

* setting directory
cd "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata"



****************************************************************************************************************
*                                   PREP MUNICIPALITY AID DATA FOR MERGE                                       *
****************************************************************************************************************

import delimited "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201705/01_cleaning/00_colombia_locs1-3_merged.csv", clear

sort codmuni

save "00_colombia_locs1-3_merged.dta", replace



****************************************************************************************************************
*                                   			MERGE AG AID                                       			   *
****************************************************************************************************************

import delimited "elecs_aid_crop_201709_02.csv", clear
sort codmuni

merge m:1 codmuni using "00_colombia_locs1-3_merged.dta"

save "01_MERGED_locs1-3_with_colombia-cs.dta", replace



****************************************************************************************************************
*                                         MERGE 2006, 2002 ELECTIONS                                           *
****************************************************************************************************************






****************************************************************************************************************
*                                   	      PREP FOR ANALYSIS                                       		   *
****************************************************************************************************************

drop _merge entidad_te municipio

* drop Arichipelago de San Andres
drop if codmuni == 88564

* create dummy - is aid targeted to the municipality (prec. code 1, 2, or 3)
* (Note: there are no prec.code==2 for ag aid in WB Colombia proj.)
gen muni_locd = 1 if locs3_project_n >= 1 | locs1_project_n >= 1
replace muni_locd = 0 if muni_locd==.


label var locs3_project_n "number of prec.code==3 projects between 1995-2014"
label var locs1_project_n "number of prec.code==1 projects between 1995-2014"
label var muni_locd "ag aid targeted to municipality? 1=yes"

tab muni_locd  /* 119 out of 1121 are targeted w/ aid */


save "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/00_colombia-cs_with_ag_aid_201708.dta", replace
export delimited "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/00_colombia-cs_with_ag_aid_201708.csv", replace














