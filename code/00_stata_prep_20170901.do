*******************************************************************************************************************
*
*  File Name:      02_eac_PREP_CONSISTENT
*  Author:	       Billy Matthias 
*  Email:	       bmatthias88@gmail.com
*  Purpose:	       (1) prep for analysis at the municipality level - creating relevant vars
*				   (2) taking Technically Correct data and moving to Consistent data
*  Date Used:      2016-Nov-03
*  Data Used:      (1) "eac_municipality_1998-2012_WITH MODIS_03.dta" (2) "eac_municipality_1998-2012_IVs_CONTROLS_CREATED_04.dta"
*  Data Output:    (1) "muni_cropsMODIS_2001to2012.dta" (2) "eac_municipality_1998-2012_IVs_CONTROLS_CREATED_04.dta" (3) "eac_municipality_1998-2012_RESHAPE_05.dta"
*                  (4) "eac_municipality_1998-2012_ANALYSIS_BY_MUNICIPALITY_07.dta" (5) "eac_municipality_1998-2012_ANALYSIS_BY_MUNICIPALITY_CLEAN_08.dta"
*
*******************************************************************************************************************
																							/*
NOTE ON DO FILE:
																							*/


import delimited "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/dmyr_elec_vio.csv", clear																											

replace discrete2=0 if discrete2==.
replace discrete3=0 if discrete3==.

sort department municipality year

*******************************************************************************************************************
*                            				  AID COUNT VARIABLES                               			      *
*******************************************************************************************************************

																													/*
NOTE:
	 - DV dummy variables if aid given at any point from 1998-2012
	 - discrete2d = dummy if aid (precision code 1 or 2) given to municipality b/w 1998-2012
	 - discrete3d = dummy if aid (precision code 1, 2, or 3) given to municipality b/w 1998-2012
	 - discrete2d_dept = dummy if aid (precision code 1 or 2) given to department b/w 1998-2012
	 - discrete3d_dept = dummy if aid (precision code 1, 2, or 3) given to department b/w 1998-2012
																													*/


gen discrete2d=discrete2
replace discrete2d=1 if discrete2>1 & discrete2!=.
label var discrete2d "was aid given at any point from 1998-2012? prec. code 1 & 2"

gen discrete3d=discrete3
replace discrete3d=1 if discrete3>1 & discrete3!=.
label var discrete3d "was aid given at any point from 1998-2012? prec. code 1, 2, 3"

gen discrete2d_dept=discrete2_dept
replace discrete2d_dept=1 if discrete2_dept>1 & discrete2_dept!=.
label var discrete2d_dept "was aid given to dept at any point from 1998-2012? prec. code 1 & 2"

gen discrete3d_dept=discrete3_dept
replace discrete3d_dept=1 if discrete3_dept>1 & discrete3_dept!=.
label var discrete3d_dept "was aid given to dept at any point from 1998-2012? prec. code 1, 2, 3"



*******************************************************************************************************************
*                         					  WE'VE GOT DUPLICATES!                                			      *
*******************************************************************************************************************

sort dep_mun_id year
drop if dep_mun_id == .
duplicates drop dep_mun_id year, force


*******************************************************************************************************************
*                					CREATE DV & IVs FOR COMPARING 1998 and 2012                   				  *
*******************************************************************************************************************	
																																						/* 
NOTE:
	 - this section is CALCULATING IVs --> average values for election years
	 - In this case only election data for 2010 & 2014 election. Will collect/scrape more data at municipality level
	   in the future (for 2002 and 2006)
	 - The avg/mean values for these variables only account for all department-municipality pairs where the election
	   data is available.
	 - This is to say that the mean is calculated for available election years and then carried backward to all years
	   in the data
																																		*/
	
* MARGIN OF VICTORY *

	* calculate margin of victory in %
	gen mov = (candidate1_vote_percent - candidate2_vote_percent)
	label var mov "margin of victory %"
	
	* mean margin of victory
	bys dep_mun_id: egen mean_mov=mean(mov)
	label var mean_mov "mean margin of victory"


* VOTER TURNOUT *
																																/* 
    NOTE: 
	     - Population total (pobl_tot) variable sometimes contains dept wide population instead of municipality total.
		 - Creating a new population variable that uses the 2010 population total - ok since only have election data for 2010 & 2014.
		 - Fix this when we have more election data, i.e. scrape official voter turnout var from Col gov website. 				*/
		   
	bys dep_mun_id: gen pop_total2010 = pobl_tot if year == 2010
	sort department municipality year
	by department municipality: carryforward pop_total2010, replace

	* have to also backfill the pop total for 2010
	gen int negyear = -year
	bys dep_mun_id (negyear): carryforward pop_total2010, replace back

	* creating the voter turnout variable
	egen voter_turnout_total = rowtotal(candidate1_vote_total - candidate7_vote_total)
	label var voter_turnout_total "voter turnout total #"
	
	gen voter_turnout_perc = voter_turnout_total/pop_total2010
	replace voter_turnout_perc = . if voter_turnout_perc==0
	label var voter_turnout_perc "voter turnout % using 2010 population"
	
	bys dep_mun_id: egen mean_turnout=mean(voter_turnout_perc)
    label var mean_turnout "avg voter turnout for 2010 & 2014 elections"
	  
	  
* COALITION GOVERNMENT *
	
    * coaltion_mun: dummy for coalition government presence *
	gen coalition_mun=1 if strpos(candidate1_party, "-")
	replace coalition_mun=0 if coalition_mun==. & year == 2014
	replace coalition_mun=0 if coalition_mun==. & year == 2010
	bys dep_mun_id: egen mean_coalition_mun = mean(coalition_mun)
	label var coalition_mun "was there a coalition govt in muni?"


* MUNICIPALITY PARTY MATCH *

	replace department = upper(department)
	replace municipality = upper(municipality)
	
	* Does municipality party match w/ state (department) govt party and presidential party *
	gen muni_pres_match = 1 if party_winner1 == "PARTIDO DE LA U" ///
	| party_winner2 == "PARTIDO DE LA U" ///
	| party_winner3 == "PARTIDO DE LA U" ///
	| party_winner4 == "PARTIDO DE LA U" ///
	| party_winner5 == "PARTIDO DE LA U"
	
	replace muni_pres_match = 0 if muni_pres_match == . & year == 2014
	replace muni_pres_match = 0 if muni_pres_match == . & year == 2010
	label var muni_pres_match "does the muni party match the Pres. party?"
	
	replace local_pres_match = 1 if party_winner1 == "PARTIDO DE LA U" ///
	| party_winner2 == "PARTIDO DE LA U" ///
	| party_winner3 == "PARTIDO DE LA U" ///
	| party_winner4 == "PARTIDO DE LA U" ///
	| party_winner5 == "PARTIDO DE LA U"
	
	replace muni_pres_match = 0 if muni_pres_match == . & year == 2014
	replace muni_pres_match = 0 if muni_pres_match == . & year == 2010
	label var muni_pres_match "does the muni party match the Pres. party?"
	
	
	
	
	
	bys dep_mun_id: egen mean_muni_pres_match=mean(muni_pres_match)
	label var mean_muni_pres_match "avg. of when muni matches the Pres. party" /* b/c this is only 2010 & 2014 election info the avg can only be 0, .5, or 1 for each muni */
	
	* muni party - governor party match
																														/*
		Note: definitely more efficient way to do this, but this best solution for now
			  also, the strlen() command important b/c the party_winner[i] and governor_winner_party[i] vars are
			  often blank/NA, but don't want to count those
																														*/
	gen muni_dept_match = 1 if party_winner1 == governor_winner_party1 & strlen(governor_winner_party1) > 0 ///
	| party_winner1 == governor_winner_party2 & strlen(governor_winner_party2) > 0 ///
	| party_winner1 == governor_winner_party3 & strlen(governor_winner_party3) > 0 ///
	| party_winner1 == governor_winner_party4 & strlen(governor_winner_party4) > 0
	
	replace muni_dept_match = 1 if party_winner2 == governor_winner_party1 & strlen(governor_winner_party1) > 0 ///
	| party_winner2 == governor_winner_party2 & strlen(governor_winner_party2) > 0 ///
	| party_winner2 == governor_winner_party3 & strlen(governor_winner_party3) > 0 ///
	| party_winner2 == governor_winner_party4 & strlen(governor_winner_party4) > 0
	
	replace muni_dept_match = 1 if party_winner3 == governor_winner_party1 & strlen(governor_winner_party1) > 0 ///
	| party_winner3 == governor_winner_party2 & strlen(governor_winner_party2) > 0 ///
	| party_winner3 == governor_winner_party3 & strlen(governor_winner_party3) > 0 ///
	| party_winner3 == governor_winner_party4 & strlen(governor_winner_party4) > 0
	
	replace muni_dept_match = 1 if party_winner4 == governor_winner_party1 & strlen(governor_winner_party1) > 0 ///
	| party_winner4 == governor_winner_party2 & strlen(governor_winner_party2) > 0 ///
	| party_winner4 == governor_winner_party3 & strlen(governor_winner_party3) > 0 ///
	| party_winner4 == governor_winner_party4 & strlen(governor_winner_party4) > 0
	
	replace muni_dept_match = 1 if party_winner5 == governor_winner_party1 & strlen(governor_winner_party1) > 0 ///
	| party_winner5 == governor_winner_party2 & strlen(governor_winner_party2) > 0 ///
	| party_winner5 == governor_winner_party3 & strlen(governor_winner_party3) > 0 ///
	| party_winner5 == governor_winner_party4 & strlen(governor_winner_party4) > 0
	
	
	replace muni_dept_match = 0 if muni_dept_match == . & year == 2010
	replace muni_dept_match = 0 if muni_dept_match == . & year == 2014
	
	label var muni_dept_match "does winning mayor party match winning governor party?"
	
	bys dep_mun_id: egen mean_muni_dept_match=mean(muni_dept_match)
	label var mean_muni_dept_match "avg of when mayor and governor party match for 2010 & 2014 election"  /* b/c this is only 2010 & 2014 election info the avg can only be 0, .5, or 1 for each muni */
	

* PRECIPITATION VARIABLE *

	bys dep_mun_id: egen SPIstd=sd(spi12m)
	label var SPIstd "Std dev of annual SPI12m values"

	
* AREA *
																																/*
    Note:
		 - area variable sometimes contains total area for the department instead of municipality
		 - fixing this below 
																																*/
	bys dep_mun_id: gen sq_km_mun = sq_km_muni if year == 2010
	sort department municipality year
	by department municipality: carryforward sq_km_mun, replace

	* have to also backfill the area
	bys dep_mun_id (negyear): carryforward sq_km_mun, replace back
	label var sq_km_mun "area (km2) of muni"

	
* CROPSHARE IN KM^2 *

	bys dep_mun_id: gen cropsha_km2 = cropsha/100
	label var cropsha_km2 "cropshare area (km2)"

	
* CROPSHARE AS PERCENT OF TOTAL AREA *

	bys dep_mun_id: gen cropsha_pct = cropsha_km2/sq_km_mun
	label var cropsha_pct "cropshare/total muni area"

	
* MAJOR POLITICAL PARTY VARIABLES *
																														/*
    Note:
		 - 4 or 5 major political parties
         - Partido Social de Unidad Nacional = party_soc
	     - Partido Conservador Colombiano =  party_con
	     - Partido Liberal Colombiano = party_lib
	     - Centro Democratico = party_cen
	     - Cambio Radical (not considered as major but still gets lots of votes) = party_cam
																														*/
	gen party_social = 1 if party_winner1 == "PARTIDO DE LA U" ///
	| party_winner2 == "PARTIDO DE LA U" ///
	| party_winner3 == "PARTIDO DE LA U" ///
	| party_winner4 == "PARTIDO DE LA U" ///
	| party_winner5 == "PARTIDO DE LA U"
	label var party_social "1 = Partido de la U/Partido Social de Unidad Nacional"
	
	gen party_conserv = 1 if party_winner1 == "PARTIDO CONSERVADOR COLOMBIANO" ///
	| party_winner2 == "PARTIDO CONSERVADOR COLOMBIANO" ///
	| party_winner3 == "PARTIDO CONSERVADOR COLOMBIANO" ///
	| party_winner4 == "PARTIDO CONSERVADOR COLOMBIANO" ///
	| party_winner5 == "PARTIDO CONSERVADOR COLOMBIANO"
	label var party_conserv "1 = Partido Conservador Colombiano"
		
	gen party_liberal = 1 if party_winner1 == "PARTIDO LIBERAL COLOMBIANO" ///
	| party_winner2 == "PARTIDO LIBERAL COLOMBIANO" ///
	| party_winner3 == "PARTIDO LIBERAL COLOMBIANO" ///
	| party_winner4 == "PARTIDO LIBERAL COLOMBIANO" ///
	| party_winner5 == "PARTIDO LIBERAL COLOMBIANO"
	label var party_liberal "1 = Partido Liberal Colombiano"
	
	gen party_centro = 1 if party_winner1 == "PARTIDO CENTRO DEMOCRATICO" ///
	| party_winner2 == "PARTIDO CENTRO DEMOCRATICO" ///
	| party_winner3 == "PARTIDO CENTRO DEMOCRATICO" ///
	| party_winner4 == "PARTIDO CENTRO DEMOCRATICO" ///
	| party_winner5 == "PARTIDO CENTRO DEMOCRATICO"
	label var party_centro "1 = Partido Centro Democratico"
	
	gen party_cambio = 1 if party_winner1 == "PARTIDO CAMBIO RADICAL" ///
	| party_winner2 == "PARTIDO CAMBIO RADICAL" ///
	| party_winner3 == "PARTIDO CAMBIO RADICAL" ///
	| party_winner4 == "PARTIDO CAMBIO RADICAL" ///
	| party_winner5 == "PARTIDO CAMBIO RADICAL"
	label var party_cambio "1 = Partido Cambio Radical"

    replace party_social = 0 if party_social==. & election_year==2015
    replace party_social = 0 if party_social==. & election_year==2011
	replace party_conserv = 0 if party_conserv==. & election_year==2015
    replace party_conserv = 0 if party_conserv==. & election_year==2011
	replace party_liberal = 0 if party_liberal==. & election_year==2015
    replace party_liberal = 0 if party_liberal==. & election_year==2011
	replace party_centro = 0 if party_centro==. & election_year==2015
    replace party_centro = 0 if party_centro==. & election_year==2011
	replace party_cambio = 0 if party_cambio==. & election_year==2015
    replace party_cambio = 0 if party_cambio==. & election_year==2011
	
	* gen Variable For "Other Parties"
	gen party_otherp = 1 if party_social==0 & party_conserv==0 & party_liberal==0 & party_centro==0 & party_cambio==0
	replace party_otherp = 0 if party_otherp==. & election_year==2015
    replace party_otherp = 0 if party_otherp==. & election_year==2011
	label var party_otherp "1 = non-major parties"
	
	
	* MAJOR POLITICAL PARTIES WIN ELECTION, AVGs
	bys dep_mun_id: egen party_social_mean=mean(party_social)
	label var party_social_mean "avg party_social wins in 2010 & 2015"
	
	bys dep_mun_id: egen party_conserv_mean=mean(party_conserv)
	label var party_conserv_mean "avg party_conserv_mean wins in 2010 & 2015"
	
	bys dep_mun_id: egen party_liberal_mean=mean(party_liberal)
	label var party_liberal_mean "avg party_liberal_mean wins in 2010 & 2015"
	
	bys dep_mun_id: egen party_centro_mean=mean(party_centro)
	label var party_centro_mean "avg party_centro_mean wins in 2010 & 2015"
	
	bys dep_mun_id: egen party_cambio_mean=mean(party_cambio)
	label var party_cambio_mean "avg party_cambio_mean wins in 2010 & 2015"
	
	bys dep_mun_id: egen party_otherp_mean=mean(party_otherp)
	label var party_otherp_mean "avg party_otherp_mean wins in 2010 & 2015"
	

* FIX UPSTREAM (river variable)
	gen upstream1 = upstream

	
* FIX ALTITUDE
	gen altitude1 = altitude
	label var altitude1 "avg elevation of muni"

	
* FIX ROAD
	gen road_zero = road0
	gen road_five = road5
	gen road_six = road6


* ROAD VARs AVGs *
	bys dep_mun_id: egen road0_mean=mean(road_zero)
	bys dep_mun_id: egen road5_mean=mean(road_five)
	bys dep_mun_id: egen road6_mean=mean(road_six)

	
* VARIABLE FOR MAYOR PARTY & PARLIAMENT PARTY MATCH *
																													/* 
	Note:
		- remember that whereever you see "year == 2010" this means that there is only
		  election data b/w the CLEA and local election for this year. 												*/
		  
		  
    order codmuni department municipality year pty_n party_winner1
	format %24s pty_n
	format %24s party_winner1
	
	gen local_parliament_match = 1 if pty_n == party_winner1 & strlen(party_winner1) > 0 ///
	| pty_n == party_winner2 & strlen(party_winner2) > 0 ///
	| pty_n == party_winner3 & strlen(party_winner3) > 0 ///
	| pty_n == party_winner4 & strlen(party_winner4) > 0 ///
	| pty_n == party_winner5 & strlen(party_winner5) > 0
	label var local_parliament_match "do muni and parliament parties match?"

	replace local_parliament_match = 0 if local_parliament_match ==. & year == 2010
	
	order codmuni department municipality year pty_n party_winner1 local_parliament_match


	*gen mean variable for local_parliament_match
	sort dep_mun_id year
	bys dep_mun_id: egen local_parliament_match_mean = mean(local_parliament_match)
	label var local_parliament_match_mean "avg where muni party matches parliament party (w/in dept), 2010 election"


* AVG. COCA_KM2 BY DEPT-MUNI
	bys dep_mun_id: egen coca_dept_muni_mean = mean(coca_km2)
	label var coca_dept_muni_mean "avg coca coverage (km2) by dept-municipality (1999-2014)"

	
* AVG. COCA_KM2 BY DEPT
	bys department: egen coca_dept_mean = mean(coca_km2)
	label var coca_dept_mean "avg coca coverage (km2) by dept (1999-2014)"
	
	
* VIOLENCE VARIABLES - TOTALS

	bys dep_mun_id: egen attack_victims_total = total(attack_victims)
	bys dep_mun_id: egen combatant_attacks_total = total(combatant_attacks)
	bys dep_mun_id: egen attack_on_civilians_total = total(attack_on_civilians)
	bys dep_mun_id: egen wounded_total = total(wounded)
	bys dep_mun_id: egen killed_total = total(killed)
	bys dep_mun_id: egen massacre_victims_total = total(massacre_victims)
	bys dep_mun_id: egen terrorist_victims_total = total(terrorist_victims)
	bys dep_mun_id: egen wounded_terrorist_total = total(wounded_terrorist)
	bys dep_mun_id: egen war_victims_total = total(war_victims)
	bys dep_mun_id: egen war_combatants_total = total(war_combatants)
	bys dep_mun_id: egen war_civilian_victims_total = total(war_civilian_victims)
	
	label var attack_victims_total "total victims of attacks in muni, 1998-2012"
	label var combatant_attacks_total "total combatant attacks in muni, 1998-2012"
	label var attack_on_civilians_total "total attacks on civilians in muni, 1998-2012"
	label var wounded_total "total wounded in muni, 1998-2012"
	label var killed_total "total killed in muni, 1998-2012"
	label var massacre_victims_total "total victims of massacres in muni, 1998-2012"
	label var terrorist_victims_total "total victims of terrorist attacks in muni, 1998-2012"
	label var wounded_terrorist_total "total terrorists wounded in muni, 1998-2012"
	label var war_victims_total "total victims of war in muni, 1998-2012"
	label var war_combatants_total "total war combatants in muni, 1998-2012"
	label var war_civilian_victims_total "total civilian victims in muni, 1998-2012"

	
* PLEBISCITE VARIABLES
	gen majority_yes = 1 if yes_percent > .5
	replace majority_yes = 0 if majority_yes ==.
	
	* avg margin of victory for winning vote
	gen mov_pleb = abs(yes_percent - no_percent)
	
	
** 20180901 ANALYSIS PREP --
bys codmuni: egen vio_26mean = mean(vio_26)
bys codmuni: egen vio_27mean = mean(vio_27)
bys codmuni: egen vio_28mean = mean(vio_28)
bys codmuni: egen vio_29mean = mean(vio_29)
bys codmuni: egen vio_30mean = mean(vio_30)
bys codmuni: egen vio_3mean = mean(vio_3)
bys codmuni: egen vio_31mean = mean(vio_31)
bys codmuni: egen vio_32mean = mean(vio_32)
bys codmuni: egen vio_33mean = mean(vio_33)
bys codmuni: egen vio_34mean = mean(vio_34)
bys codmuni: egen vio_35mean = mean(vio_35)
bys codmuni: egen vio_36mean = mean(vio_36)
bys codmuni: egen vio_46mean = mean(vio_46)
bys codmuni: egen vio_11mean = mean(vio_11)
bys codmuni: egen vio_14mean = mean(vio_14)
bys codmuni: egen vio_13mean = mean(vio_13)
bys codmuni: egen vio_15mean = mean(vio_15)
bys codmuni: egen geo_3mean = mean(geo_3)
bys codmuni: egen demo_2mean = mean(demo_2)
bys codmuni: egen demo_3mean = mean(demo_3)
bys codmuni: egen demo_4mean = mean(demo_4)
bys codmuni: egen pres_centro_mean = mean(pres_centro)
bys codmuni: egen pres_conserv_mean = mean(pres_conserv)
bys codmuni: egen pres_izqui_mean = mean(pres_izqui)
bys codmuni: egen pres_tv_mean = mean(pres_tv)
bys codmuni: egen pres_lib_mean = mean(pres_lib)
bys codmuni: egen pres_urib_mean = mean(pres_urib)
bys codmuni: egen pres_min_mean = mean(pres_min)



	

* KEEP PARTY AND MATCH VARIABLES FOR 2014
* - need to take the 2014 "local & dept party w/ pres. party match" variables and keep them so they don't drop out when we only "keep" 2012 variables below
* - only looking @ when municipality (local) party and pres. party match in 2014

keep if year==2001 | year==2012	| year==2014

bys dep_mun_id (negyear): carryforward local_pres_match, replace back
bys dep_mun_id (negyear): carryforward local_parliament_match, replace back
bys dep_mun_id (negyear): carryforward gov_pres_match, replace back
bys dep_mun_id (negyear): carryforward local_gov_match, replace back

replace local_pres_match = muni_pres_match if year==2014
bys dep_mun_id (negyear): carryforward local_pres_match, replace back


save "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/elecs_aid_crop_201709_01.dta", replace




******************************************************************************************************************
*               				   RESHAPE TO CREATE CROPSHA_PCT DIFFERENCE                                      *
******************************************************************************************************************

keep if year == 2001 | year == 2012
keep year dep_mun_id cropsha_pct
duplicates drop dep_mun_id year, force
reshape wide cropsha_pct, i(dep_mun_id) j(year)

by dep_mun_id: gen cropsha_pct_chg = cropsha_pct2012 - cropsha_pct2001
replace cropsha_pct_chg =. if cropsha_pct_chg==0

sort dep_mun_id

save "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_before-201705/01_cleaning/muni_19982014_v08_RESHAPE.dta", replace


use "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/elecs_aid_crop_201709_01.dta", clear
drop _merge

merge m:m dep_mun_id using "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_before-201705/01_cleaning/muni_19982014_v08_RESHAPE.dta"
drop _merge



save "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/elecs_aid_crop_201709_02.dta", replace

replace cropsha_pct_chg = cropsha_pct_chg*100
replace cropsha_pct2001 = cropsha_pct2001*100
replace cropsha_pct2012 = cropsha_pct2012*100

keep if year == 2012

export delimited "/Users/wtmatthias/Google Drive/Mike RA/elections_aid_cropshare/data_201708/01_outputdata/elecs_aid_crop_201709_02.csv", replace
