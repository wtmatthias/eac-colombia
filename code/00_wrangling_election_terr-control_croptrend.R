#-----------------------------------------------------------------------------#
#  File Name:    00_wrangling_election_terr-control_crop-trend
#  Author:       Billy Matthias 
#  Email:        bmatthias88@gmail.com
#  Purpose:      (1) merge in more election data
#                    - pres. elections 1998-2006
#                    - senate & camara de rep. 2002-2006 
#                (2) merge terr. control measures
#                (3) armed actor presence vars
#                (4) merge cropshare trend var
#                (5) wrangle and prep consistent data for analysis
#  Last Update:  2017-Aug-08
#  Data Output:     
#-----------------------------------------------------------------------------#
####    README/NOTES    ####



#-----------------------------------------------------------------------------#

# =-=-=-=-=-=-=-=-=-=- # 
####    I. SETUP    #### 
# =-=-=-=-=-=-=-=-=-=- #
rm(list=ls())

library(plyr)
library(tidyverse)
library(readstata13)
library(stringr)

## 1a) SET FILE PATHS ----
# Set working directory to where you saved project directory (folder)
# 'dir.home' the only file path you need to change!
dir.home <- file.path("/Users/wtmatthias/Google Drive/Mike RA")

dir.panel <- file.path(dir.home,
                      "elections_aid_cropshare/data_before-201705/01_cleaning")
dir.rawdata <- file.path(dir.home,
                         "elections_aid_cropshare/data_201708/00_rawdata")
dir.analysis <- file.path(dir.home,
                          "elections_aid_cropshare/data_201708")
dir.elec <- file.path(dir.rawdata, "elections_2002-2014")
dir.trend <- file.path(dir.rawdata, "modis_trend-variables_Eugenio")
dir.outdata <- file.path(dir.home,
                         "elections_aid_cropshare/data_201708/01_outputdata")



# =-=-=-=-=-=-=-=-=-=-=-=-= # 
####    II. LOAD DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-= #
setwd(dir.panel)

## 2a) COLOMBIA *PANEL* DATA ----
orig_panel <- read.dta13("muni_19982014_v06_TECHNICALLY_CORRECT.dta")

setwd(dir.rawdata)
codmuni <- read_csv("muni_codes.csv")

names <- orig_panel %>% select(codmuni, department, municipality) %>%
         distinct(department, municipality)


## 2b.1) (TEST) ELECTION DATA - .dta2.csv name change & write ####
# df1 <- data.frame(number = c(1, 2, 3, ""))
# df2 <- data.frame(number = c(4, 5, 6, ""))
# rewrite_test <- data.frame(csvfile = c("test1.csv", "test2.csv"),
#                            dfobj = c("df1", "df2"),
#                            stringsAsFactors = FALSE)
# 
# df_list <- mget(rewrite_test$dfobj, envir = globalenv()) # getting the object w/ the dataframe
# str(df_list)
# df_list[[1]]
# 
# # before running make sure wd is set to where you want to write .csvs
# getwd()
# lapply(1:length(df_list), function(x) write_csv(df_list[[x]],
#                                                 path = rewrite_test$csvfile[[x]],
#                                                na = "",
#                                                 col_names = TRUE)
# )


## 2b.2) ELECTION DATA - .dta2.csv ----


#'pres' = presidential election by muni
pres <- read.dta13("cattle_and_election_ideology/4. municipal data.dta")
# variable.names(pres)

#'sen' = senate elections by muni
# senado_02 <- read.dta13("elections_2002-2014/senado_2002.dta")
# senado_03 <- read.dta13("elections_2002-2014/senado_2003.dta")
# senado_06 <- read.dta13("elections_2002-2014/senado_2006.dta")
# senado_07 <- read.dta13("elections_2002-2014/senado_2007.dta")
# senado_10 <- read.dta13("elections_2002-2014/senado_2010.dta")
# senado_14 <- read.dta13("elections_2002-2014/senado_2014.dta")

# 'cam' = "camara de representantes," elections by muni
# camara_02 <- read.dta13("elections_2002-2014/camara_2002.dta")
# camara_03 <- read.dta13("elections_2002-2014/camara_2003.dta")
# camara_06 <- read.dta13("elections_2002-2014/camara_2006.dta")
# camara_07 <- read.dta13("elections_2002-2014/camara_2007.dta")
# camara_10 <- read.dta13("elections_2002-2014/camara_2010.dta")
# camara_14 <- read.dta13("elections_2002-2014/camara_2014.dta")


## WRITE DFs ABOVE TO .csv & READ THEM BACK IN
#    - .csv smaller files than .dta file
# setwd(dir.elec)

# create new file name w/ .csv as extension
# rewrite_dta <- list.files(dir.elec, pattern = "*.dta") # lists names from dir
# rewrite_dta_name <- as.data.frame(sapply(rewrite_dta,
#                                         gsub, pattern = ".dta",
#                                         replacement = ".csv",
#                                         USE.NAMES = FALSE),
#                                  stringsAsFactors = FALSE)
#                           # .csv for filename + convert to data.frame
# rewrite_dta_name <- rewrite_dta_name %>% select(csvfile = contains("sapply"))

# create a list of DFs
# elec <- ls(envir = globalenv(),
#            pattern = "_[[:digit:]]+$") # finds list of the election df obj
# df_list <- mget(elec, env = globalenv())
# 
# system.time(
#   lapply(1:length(df_list), function(x) write_csv(df_list[[x]],
#                                         path = paste(gsub(".dta",
#                                                            ".csv",
#                                                            rewrite_dta[[x]])),
#                                         col_names = TRUE)
#       )
#   )
# 
# system.time(
#   d_ply(1:length(df_list), function(x) write_csv(df_list[[x]],
#                                                   path = paste(gsub(".dta",
#                                                                     ".csv",
#                                                                     rewrite_dta[[x]])),
#                                                   col_names = TRUE)
#   )
# )


# remove/delete .dta and TEST objs from environment
# rm(list = elec)
# rm(list = c("df1", "df2", "elec", "rewrite_dta", "tmp",
#             "rewrite_dta_name", "rewrite_test"))



## 2b.3) ELECTION DATA - write_csv ----
setwd(dir.elec)

elec_csv <- list.files(dir.elec, pattern = "_[[:digit:]]+.csv")
            # list of the election .csv
elec_dfnames <- c("camara02", "camara03", "camara06", "camara07",
                  "camara10", "camara14", "senado02", "senado03",
                  "senado06", "senado07", "senado10", "senado14")
elec_dflist <- lapply(1:length(elec_csv), function (x) read_csv(elec_csv[[x]],
                                                            col_names = TRUE))

# name each of the DFs w/in the list
elec_dflist <- setNames(elec_dflist, elec_dfnames)
str(elec_dflist, give.attr = FALSE)


## 2b.4) ELECTION DATA - binding list of DFs ----
# require(plyr)
elec_cam <- ldply(elec_dflist[1:6])
elec_cam1 <- as_tibble(elec_camara)
elec_sen <- ldply(elec_dflist[7:12])
elec_sen1 <- as_tibble(elec_senado)
rm(list = c("elec_dflist", "elec_cam", "elec_sen"))


## 2c) MODIS TREND VAR ----
setwd(dir.trend)
trendvar <- list.files(pattern = "*_trend.dta")
# c3 = "c3 plants trend"
c3 <- read.dta13(trendvar[1])
# modis_c = "Modis crops trend"
modis_c <- read.dta13(trendvar[2])
# modis_cv = "Modis crop veg trend"
modis_cv <- read.dta13(trendvar[3])
# modis_g = "Modis grass trend"
modis_g <- read.dta13(trendvar[4])

# orig_panel %>% summarise(n_distinct(admin2Pcod)) # 1118 munis




# =-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    III. MERGE DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=- #

# str(orig_panel$department)
# str(orig_panel$municipality)
# str(codmuni$municipality)
# str(codmuni$department)
# str(orig_panel)


## 3a.1) MUNI CODES MERGE ----

# don't want missing values
empty_dept <- orig_panel %>% filter(department == "")
empty_muni <- orig_panel %>% filter(municipality == "")

# filter out missing values
orig_panel <- orig_panel %>% filter(department != "" | municipality != "")

# dmyr = 'dept' + 'muni' + 'year' (panel)
dmyr <- left_join(orig_panel, codmuni, by = c("department", "municipality"))

# check merge + only keep codmuni.y
dmyr <- dmyr %>% dplyr::select(codmuni.y, everything(), -codmuni.x) %>%
                 dplyr::rename(codmuni = codmuni.y)

# drop rows where codmuni == NA
nrow(distinct(dmyr, codmuni))
dmyr <- dmyr %>% drop_na(codmuni)



## 3a.2) REMOVE DUPLICATES ----
nrow(dmyr)
nrow(distinct(dmyr, codmuni, year))

# freq table of dups
# dups <- dmyr %>% dplyr::select(codmuni, year) %>% arrange(codmuni, year)
# dups_n_occur <- data.frame(table(paste0(dups$codmuni, dups$year)))
# dups_n_occur <- dups_n_occur %>% arrange(desc(Freq))

dmyr <- dmyr %>% distinct(codmuni, year, .keep_all = TRUE)

rm(list = c("empty_dept", "empty_muni", "names", "orig_panel"))
# rm(list = c("dups", "dups_n_occur")



## 3b) TREND VARS MERGE W/ PANEL ----

# rename each trend DFs vars
c3 <- c3 %>% dplyr::rename(sterr_c3 = sterr, tstat_c3 = tstat)
modis_c <- modis_c %>% dplyr::rename(trendcrop = trend, sterr_cr = sterr, tstat_cr = tstat)
modis_cv <- modis_cv %>% dplyr::rename(sterr_cv = sterr, tstat_cv = tstat)
modis_g <- modis_g %>% dplyr::rename(sterr_g = sterr, tstat_g = tstat)

# create codmuni from admin2Pcod 
c3 <- c3 %>%
      separate(admin2Pcod,
               into = c("co", "codmuni"),
               sep = 2,
               remove = FALSE) %>%
      select(-co) %>% 
      separate(codmuni,
               into = c("zero", "codmuni"),
               sep = "^0+",
               fill = "left") %>%
      select(-zero)

modis_c <- modis_c %>%
           separate(admin2Pcod, c("co", "codmuni"), 2, remove = FALSE) %>%
           select(-co) %>% 
           separate(codmuni, c("zero", "codmuni"), "^0+", fill = "left") %>%
           select(-zero)

modis_cv <- modis_cv %>%
            separate(admin2Pcod, c("co", "codmuni"), 2, remove = FALSE) %>%
            select(-co) %>% 
            separate(codmuni, c("zero", "codmuni"), "^0+", fill = "left") %>%
            select(-zero)

modis_g <- modis_g %>%
           separate(admin2Pcod, c("co", "codmuni"), 2, remove = FALSE) %>%
           select(-co) %>% 
           separate(codmuni, c("zero", "codmuni"), "^0+", fill = "left") %>%
           select(-zero)

# 'codmuni' needs to be of same type w/ dmyr 
c3$codmuni <- as.integer(c3$codmuni)
modis_c$codmuni <- as.integer(modis_c$codmuni)
modis_cv$codmuni <- as.integer(modis_cv$codmuni)
modis_g$codmuni <- as.integer(modis_g$codmuni)


## MERGE W/ dmyr
# dmyr_test <- left_join(dmyr, c3, by = "codmuni") %>%
#              dplyr::select(contains("c3"), everything())
dmyr_c3 <- left_join(dmyr, c3, by = "codmuni")
dmyr_crop <- left_join(dmyr_c3, modis_c, by = "codmuni")
dmyr_cropveg <- left_join(dmyr_crop, modis_cv, by = "codmuni")
dmyr_jointrend <- left_join(dmyr_cropveg, modis_g, by = "codmuni")

variable.names(dmyr_jointrend)
# rm("dmyr_test")
rm(list = c("c3", "modis_c", "modis_cv", "modis_g"))
dmyr_jointrend <- dmyr_jointrend %>% select(-contains("admin2Pcod."))



## 3c) PRES ELECTION DATA ----
variable.names(pres)
# president election vars, + terr. control + other CEDE data
# modified this slightly from the full version
pres_cede <- pres %>%
             dplyr::select(-(id_4), -(id_6), -num_range("eco_", 1:32)) %>%
             dplyr::rename(pres_tie = pol_1, pres_centro = pol_2, pres_conserv = pol_3,
                    pres_tv = pol_4, pres_izqui = pol_5, pres_lib = pol_6,
                    pres_min = pol_7, pres_urib = pol_8,
                    year = id_2, codmuni = id_3)

dmyr_pres <- left_join(dmyr_jointrend, pres_cede, by = c("codmuni", "year"))

is.tibble(dmyr_pres) # tibble nicer output in the console/easier to work with
dmyr_pres <- as.tibble(dmyr_pres) 

# dmyr_pres %>% 
#         dplyr::rename_all(
#               funs(stringr::str_replace_all(., ".x", ""))
#               ) %>%
#         dplyr::select(-contains(".y")) %>%
#         variable.names()


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    IV. IDEOLOGY CODING & MATCH VARS    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

## SAVE BINDED RAW ELECTION DATA (i.e before changes)
write_csv(elec_sen1, file.path(dir.elec, "senado_2002-2014.csv"))
write_csv(elec_cam1, file.path(dir.elec, "camara_2002-2014.csv"))


## BRING IN .CSV W/ PARTY CODE NAMES AND #S
dir.party <- file.path(dir.home,
          "elections_aid_cropshare/data_201705/00_rawdata",
          "electorales_database_datosCEDE_2002_2006")
setwd(dir.party)
party <- read_csv("codpartido_codebook.csv", locale = locale(encoding = "ISO-8859-1",
                                                             asciify = TRUE),
                  col_names = TRUE, col_types = NULL)
party <- party %>% rename(partido = party_name) 
party$partido <- toupper(party$partido)


##  IDEOLOGY CODING
# Ideology Vars in Pres. election coding ('pres')
# (these are all dummies)
# TIES:         pol_1 = pres_tie = cam_tie
# CENTRO:       pol_2 = pres_centro = cam_centro
# CONSERVADOR:  pol_3 = pres_conserv = cam_conserv
# TERCERA VIA:  pol_4 = pres_tv = cam_tv
# IZQUIERDA:    pol_5 = pres_izqui = cam_izqui
# LIBERAL:      pol_6 = pres_lib = cam_lib
# MINORITY:     pol_7 = pres_min = cam_min
# URIBISMO:     pol_8 = pres_urib = cam_urib

# Pres. ideology coding:
# 1994 - Liberal
# 1998 - Conservative
# 2002 - Uribe
# 2006 - Uribe
# 2010 - Uribe (Santos candidate)
# 2014 - Tercera via (Santos)
# NOTE: need to do Santos coding normal w/ a 2nd robust coding check w/
#       2010 as TV too



## 4a) FOR 'CAMARA' DATA ----

## NOTES:
#       - codmuni == 99 means "TOTAL"
#       - coddept ('codep' orignially) == 57 == "national total"
#       - codmuni==99 (so just the "total") is counting the dept level totals
#         for candidiates


# renaming vars to be consistent w/ other data
elec_cam1 <- elec_cam1 %>% rename(year = ano, codmuni = codmpio, coddept = codep)


# drop obs that calculate "totals" and aren't individual muni-year obs.
elec_cam1 %>% summarise(n_distinct(codmuni))
elec_cam1 %>% count(codmuni) %>% View()
elec_cam1 %>%
  arrange(coddept, codmuni, year, desc(votos), codpartido) %>%
  View()



# how many "TOTAL" rows are there?
total <- str_extract_all(elec_camara1$municipio, "TOTAL", simplify = TRUE)
total <- as_tibble(total)
total %>% summarise(n_distinct(V1))
total %>% count(V1)

# vote totals
elec_cam_tot <- elec_cam1 %>% filter(codmuni == 99)
elec_cam_tot %>% summarise(n_distinct(municipio))
elec_cam_tot %>% count(municipio) %>% View()

# national vote totals
elec_cam_ntot <- elec_cam_tot %>%
                    filter(municipio == "TOTALES NACIONALES" |
                           municipio == "NACIONAL ESPECIAL")
elec_cam_tot <- elec_cam_tot %>%
                    filter(municipio != "TOTALES NACIONALES")

elec_cam_tot %>%
  arrange(coddept, codmuni, year, desc(votos), codpartido) %>%
  View()
elec_cam_tot %>%
  arrange(codmuni, coddept, year, desc(votos), codpartido) %>%
  View()


# drop tipo_eleccion that's not

# 'ct' = cross-tab
ct_yr_etype <- table(elec_cam_totnat$year, elec_cam_totnat$tipo_eleccion)
ct_yr_etype
ct_yr_etype <- table(elec_sen1$ano, elec_sen1$tipo_eleccion)
ct_yr_etype
View(elec_sen1)

## left off at red dot...
# is 2014 'tipo_eleccion' in elec_cam1 coded wrong?
# 5 supposed to be == 3?



# calculate total votes by dept
# include codlista == 996-999 (blank, 'votos nulos', 'tarjetas no marcadas')
elec_cam2 <- elec_cam1 %>%
                  filter((municipio != "TOTALES NACIONALES" |
                          !is.na(municipio))) %>%
                  group_by(coddept, year) %>%
                  mutate(cam_votedept = )


# get rid of if is.na(codpartido) & is.na(codlista)

# quick string lookups
# will go back and automate lookup process if "easy" cases make sense
# look <- stringr::str_locate(party$partido, "PRIMERO COLOMBIA")
# View(look)
# rm(look)


## CODE ALL THE IDEOLOGIES:
# 2002, 2006, 2010 liberal party
lib <- c(1, 804, 791, 733, 769, 845, 795, 811, 982, 828, 802, 983, 726)
# 2002 centro party
centro02 <- c(46)
# 2010 centro party
centro10 <- c(42)
# 2014 centro party
centro14 <- c(645, 986, 715, 790, 760, 987, 845, 847, 811, 758, 832)
# 2010, 2014 conserv party
conserv <- c(2, 848)
# 2010 tercera via party
tv10 <- c(164)
# 2014 tercera via party
tv14 <- c(198, 762, 845, 759, 802, 848, 983, 839, 726, 748, 827, 847, 986, 765,
          804, 951, 744, 733, 855, 778, 754, 751)
# 2002 izquierda independiente
izqui02 <- c(47)
# 2006, 2010, 2014 izquierda independiente
izqui06 <- c(194, 779, 790)
# 2002 minority/other party
min02 <- c(24, 38, 39, 57, 99, 42, 36)
# 2006 minority/other party
min06 <- c(38, 43, 190, 15)
# 2010 minority/other party
min10 <- c(34, 199, 233)
# 2002, 2006 uribismo party
urib02 <- c(154)
# 2010 uribismo party
urib10 <- tv14
# 2014 uribismo party
urib14 <- c(644)

## ROBUSTNESS CHECK CODING:
# w/ 2010 as tv14 (Santos' ideology in 2014) rather
# than Santos coded as Uribismo. REMEMBER to take out urib10 for this alternate
# coding!! (also it drops out tv10)
tv_santos1014 <- tv14


## CAMARA IDEOLOGY DUMMY VARS:
cam_lib <- elec_cam1 %>% mutate()
cam_centro <- 
cam_conserv <- 
cam_tv <- 
cam_izqui <- 
cam_min <- 
cam_urib <- 

# CENTRO:       pres_centro = pol_2
# CONSERVADOR:  pres_conserv = pol_3
# TERCERA VIA:  pres_tv = pol_4
# IZQUIERDA:    pres_izqui = pol_5
# LIBERAL:      pres_lib = pol_6
# MINORITY:     pres_min = pol_7
# URIBISMO:     pres_urib


# 1) does the party that got the most votes in the muni...
# match ANY of the Camara seats won at the department level?
#     - (what about if a party was not first in voting at muni but still a
#       "top" vote getter? what does "top" mean?)
#     - (might be especially important for a top vote getter in "competitive"
#       elections -- aid could be shifted from national to muni to shift or
#       rally support for the party in charge)
#     - how to code these "near miss" cases? think about "strategic" aid
#           - e.g.1: near miss = w/in a certain range
#           - e.g.: 
#     - does strategic aid work "better" or "worse" than non-strategic aid?

# 2) does the party that got the MOST votes in the muni...
# match BOTH a Camara seat winner AND the President party/ideology
#     - code both PARTY and IDEOLOGY 'MATCH'

# 3) code the munis that are a "stronghold" in votes received from party/ideo.
#     - e.g.1: a high % of the dept vote total for the party/ideo. winner?
#     - what about coding a "could be"/"fringe" stronghold?

# 4) 


cam <- elec_cam1 %>%
            filter(!(codmuni == 99 | is.na(codmuni)))

# party winner for each muni-year
cam1 <- cam %>%
              group_by(codmuni, year) %>%
              top_n(1, votos)

cam %>%
  arrange(codmuni, year, desc(votos)) %>%
  View()




# %>%
#   (top_n(1)) %>%
#   View()

str(elec_cam1)

# does codpartido (party code) or the party name ('partido') have NAs?
elec_cam1 %>% arrange(year, codmuni) %>% group_by(codmuni) %>% mutate(is.na(codpartido)) %>% View()

View(dmyr_pres)
## 4b) FOR 'SENADO' DATA ----










## 4b) CREATE NEW "MATCH" VARS ----

##  README/NOTES:
#      
#   - only select the relevant elec yrs
#   - might help to build a function
#   - in function use assign(), but remember when using %>% you need to tell it
#     the environment. e.g.:
#                     env <- environment()
#                     "x" %>% assign(100, envir = env)
#                     x
#                     > [1] 100
#   example:
#           function(data, yr, muni_win, pres_win)
#           group_by(yr) %>% (imatch = 1 if_else(muni_win == pres_win & 0 otherwise))
# 

## 4d) MERGE NEW ELECTION MATCH VARS



## 5) TIME UNDER MUNI CONTROL ----

# create the var using Ale's Stata code

## 6) RECODE DUMMY VARS AS SPECIFIED IN MTG NOTES

## 6) CHECK FOR CONSISTENT DATA

# drop necessary vars after checking and running some descrip stats on it

# what changes were all being made in that Stata .do file?

## 6) EXPORT PANEL & CS DATA VERSIONS



## HYDE DATA (c3 & c4 crops) ----
# we have the trend vars for this
# 'c3km2' and 'c4km2' --> this is HYDE data (1992-1998 for both c3 & c4)
# summary(dmyr_pres$c3km2)
# summary(dmyr_pres$c4km2)
# dmyr_pres %>% group_by(codmuni, year) %>% select(codmuni, year, c3km2, c4km2) %>% arrange(codmuni, year) %>% summary()






