*******************************************************************************
*
*  File Name:      00_merge_elections_and_terr-control
*  Author:	       Billy Matthias 
*  Email:	       bmatthias88@gmail.com
*  Purpose:	       (1) merge in more election data
*                      - pres. elections 1998-2006
*                      - senate & camara de rep. 2002-2006 
*				   (2) merge terr. control measures
*                  (3) armed actor presence vars
*  Last Updated:   2017-Aug-04
*  Data Used:      (1) "muni_19982014_v06_TECHNICALLY_CORRECT.dta"
*  Data Output:    
********************************************************************************


use "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_before-201705/01_cleaning/muni_19982014_v06_TECHNICALLY_CORRECT.dta", clear																											



sort department municipality year


********************************************************************************
*                           WE'VE GOT DUPLICATES!                              *
********************************************************************************

sort dep_mun_id year
drop if dep_mun_id == .
duplicates drop dep_mun_id year, force

