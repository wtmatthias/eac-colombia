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
library(magrittr)
library(haven)

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


## 2b.1) (TEST) ELECTION DATA - .dta2.csv name change & write ----
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
elec_cam1 <- as_tibble(elec_cam)
elec_sen <- ldply(elec_dflist[7:12])
elec_sen1 <- as_tibble(elec_sen)
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


## 2d) CROP AND AGRICULTURE CS DATA ----
load(file = file.path(
  dir.rawdata,
  "uniandes_panel_agriculture/crops_cs_colombia.RData")
)




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



## 3c) PRES ELECTION DATA MERGE ----
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




# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    IV. CAMARA DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #

## SAVE BINDED RAW ELECTION DATA (i.e before changes)
# write_csv(elec_sen1, file.path(dir.elec, "senado_2002-2014.csv"))
# write_csv(elec_cam1, file.path(dir.elec, "camara_2002-2014.csv"))



## BRING IN .CSV W/ PARTY CODE NAMES AND #s
dir.party <- file.path(dir.home,
          "elections_aid_cropshare/data_201705/00_rawdata",
          "electorales_database_datosCEDE_2002_2006")
setwd(dir.party)
party <- read_csv("codpartido_codebook.csv", locale = locale(encoding = "ISO-8859-1",
                                                             asciify = TRUE),
                  col_names = TRUE, col_types = NULL)
party <- party %>% rename(partido = party_name) 
party$partido <- toupper(party$partido)



## 4a) VOTE TOTALS ----

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


# how many rows have "TOTAL" in them?
total <- str_extract_all(elec_cam1$municipio, "TOTAL", simplify = TRUE)
total <- as_tibble(total)
total %>% summarise(n_distinct(V1))
total %>% count(V1)


# 'vote totals' DFs
elec_cam_tot <- elec_cam1 %>% filter(codmuni == 99)
elec_cam_tot <- elec_cam_tot %>% filter(year != 2003 & year != 2007)
elec_cam_tot %>% summarise(n_distinct(municipio))
elec_cam_tot %>% count(municipio)
elec_cam_tot %>% count(coddept) %>% View()
elec_cam_tot %>% group_by(year) %>% count(municipio) %>% View()
elec_cam_tot %>% arrange(year, municipio, desc(votos)) %>% View()

elec_cam_tot %>% filter(year == 2010) %>% arrange(codmuni, coddept, desc(curules)) %>% View()

# drop non-relevant election years
# 'ct' = cross-tab
ct_yr_etype <- table(elec_cam_tot$year, elec_cam_tot$tipo_eleccion)
ct_yr_etype
ct_yr_etype <- table(elec_sen1$ano, elec_sen1$tipo_eleccion)
ct_yr_etype

elec_cam1 <- elec_cam1 %>% filter(year != 2003 & year != 2007)


# 2014 'tipo_eleccion' in elec_cam1 coded wrong
# 2 supposed to be 3
elec_cam1$tipo_eleccion <- 3


## 4b) CALCULATE TOTAL VOTES BY DEPT & MUNI ----
# include codlista == 996-999 (blank, 'tarjetas no marcadas')
# drop out the "VOTOS NULOS"

# where are the VOTOS NULOS? codlista coded inconsistently for some years
# elec_cam1 %>% filter(codlista == 996 | codlista == 997 | codlista == 998 |
#                      codlista == 998 | primer_apellido == "VOTOS NULOS" |
#                      nombre == "VOTOS NULOS") %>%
#               group_by(year, codlista) %>%
#               count(primer_apellido) %>% 
#               View()


## DROP/FILTER OUT "VOTOS NULOS"
str(elec_cam1$year)
str(elec_cam1$codlista)
elec_cam1$codlista <- as.numeric(elec_cam1$codlista)

# 2002 w/ codlista = 998 coded wrong (as "VOTOS NULOS")
elec_cam1 <- elec_cam1 %>% filter(!(year == 2002 & codlista == 998))

# show rows w/ 'VOTOS NULOS' or 997
# elec_cam1 %>% filter(nombre == "VOTOS NULOS" |
#                      primer_apellido == "VOTOS NULOS" |
#                      codlista == 997)

# recode: codlista==997 is coded for "TARJETAS NO MARCADAS" for 2002
# give 'TARJETAS NO MARCADAS' the correct codlista
elec_cam2 <- elec_cam1 %>% 
                mutate(codlista =
                          if_else(year == 2002 & codlista == 997,
                                  998,
                                  codlista))

# reassign codlista == NA to 997, when the observation is "VOTOS NULOS"
elec_cam3 <- elec_cam2 %>% mutate(codlista =
                          if_else(
                            ((nombre == "VOTOS NULOS" |
                              primer_apellido == "VOTOS NULOS") &
                              is.na(codlista)),
                              997,
                              codlista))


# filter out VOTOS NULOS
elec_cam3 %>% count(codlista==997) # NA values in codlista!
elec_cam4 <- elec_cam3 %>% filter(codlista != 997 | is.na(codlista))

elec_cam4 %>% filter(is.na(codlista)) %>% View()
votenames <- elec_cam4 %>%
                count(nombre) %>%
                mutate(containvote = stringr::str_detect(nombre, "VOTOS"))
votenames_unique <- votenames$nombre[votenames$containvote==TRUE &
                                     !is.na(votenames$containvote)]

# code "blank votes" w/ correct codlista when it's NA
blanco <- votenames_unique[stringr::str_detect(votenames_unique, "BLANCO")]
elec_cam5 <- elec_cam4 %>% mutate(codlista = 
                                    if_else(nombre %in% blanco,
                                    996,
                                    codlista))

# recode "blank votes" in 2002 so it's consistent with other years
elec_cam5 <- elec_cam5 %>% mutate(codlista = 
                                    if_else(primer_apellido %in% blanco,
                                            996,
                                            codlista))

# elec_cam5 %>% filter(codlista == 999) %>% View()

# code "blank votes" w/ correct codlista when it's NA
marcado <- votenames_unique[stringr::str_detect(votenames_unique, "MARCADOS")]
elec_cam6 <- elec_cam5 %>% mutate(codlista = 
                                      if_else(nombre %in% marcado,
                                      998,
                                      codlista))

# make sure 2002 has codlista
# elec_cam6 %>% filter(year == 2002 & codlista == 998) %>% View()
# elec_cam6 %>% filter(str_detect(primer_apellido, "TARJETAS NO MARCADAS")) %>% View()


## CREATE "DEPT - PARTY - VOTE TOTAL" VAR = 'dpvote'
# elec_cam6 <- elec_cam6 %>%
#                 filter(codmuni == 99 & (codlista != 0 | is.na(codlista))) %>% 
#                 group_by(year, coddept, codpartido) %>% 
#                 mutate(cam_dpv = if_else(
#                                          
#                                          sum(votos)))

# get rid of missing values in codmuni (these obs have no use for totals vars)
# elec_cam6 %>% ungroup() %>% mutate(codmunina = is.na(codmuni)) %>%
# count(codmunina) %>% View()
elec_cam7 <- elec_cam6 %>% filter(!is.na(codmuni))

# dept total dummy var
elec_cam7 <- elec_cam7 %>% mutate(dtot_d = if_else(codmuni==99,
                                                       1,
                                                       0))

elec_cam7$cam_dpvote <- ""
elec_cam7 <- elec_cam7 %>%
                group_by(year, coddept, dtot_d, codpartido) %>% 
                mutate(cam_dpvote = if_else(codmuni == 99,
                                            sum(votos, na.rm = TRUE),
                                            as.numeric(cam_dpvote)))

# elec_cam7 %>% arrange(year, coddept, codpartido) %>% View()

## CREATE "DEPT - VOTE TOTAL" VAR = 'dvote' & "MUNI-VOTE TOTAL" = 'mvote'
elec_cam7$cam_dvote <- ""
elec_cam7$cam_mvote <- ""
elec_cam7 <- elec_cam7 %>%
                group_by(year, coddept, dtot_d) %>% 
                mutate(cam_dvote = if_else(codmuni == 99,
                                          sum(votos),
                                          as.numeric(cam_dvote))) %>%
                ungroup() %>% 
                group_by(year, coddept, codmuni) %>%
                mutate(cam_mvote = if_else(codmuni != 99,
                                   sum(votos),
                                   as.numeric(cam_mvote)))

## CREATE "MUNI - PARTY - VOTE TOTAL" VAR = 'mpvote'

# check for missing values in codpartido
# elec_cam7 %>% filter(is.na(codpartido)) %>%
#               group_by(nombre) %>%
#               count(is.na(codpartido)) %>% View()

elec_cam7$cam_mpvote <- ""
elec_cam8 <- elec_cam7 %>%
                group_by(year, coddept, codmuni, codpartido) %>% 
                mutate(cam_mpvote = if_else(!is.na(codpartido),
                                            sum(votos),
                                            as.numeric(cam_mpvote)))
# check random muni
# elec_cam8 %>% filter(year == 2002 & codmuni == 23419) %>% arrange(codpartido) %>% View()



## 4c) CODING IDEOLOGY DUMMIES ----

# quick string lookups
# will go back and automate lookup process if "easy" cases make sense
# look <- stringr::str_locate(party$partido, "PRIMERO COLOMBIA")
# View(look)
# rm(look)
look <- stringr::str_locate(party$partido, "COLOMBIA VIVA")
View(look)


## CODE ALL THE IDEOLOGIES:
# 2002, 2006, 2010 liberal party
lib <- c(1, 804, 791, 733, 769, 845, 795, 811, 982, 828, 802, 983, 726)
# 2002 centro party
centro02 <- c(46)
# 2010 centro party
centro10 <- c(197)
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
# 2002 uribismo party
urib02 <- c(154, 73, 195, 41, 165, 730, 751, 754, 759, 762, 764, 765, 778, 791, # c(1, 804, 791, 733, 769, 845, 795, 811, 982, 828, 802, 983, 726)
            832, 849, 855, 985, 69, 158, 2, 848, 69, 158)
# 2006 uribismo
urib06 <- c(urib02, 198, 762, 845, 759, 802, 848, 983, 839, 726, 748, 827, 847,
           986, 765, 804, 951, 744, 733, 855, 778, 754, 751, 163, 155)
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
# liberal = 1

cam <- elec_cam8 %>%
                mutate(cam_lib = if_else(codpartido %in% lib,  # liberal dummy
                                         1,
                                         0))
# centro = 1
cam <- cam %>%
          mutate(cam_centro = case_when(
                                year == 2002 & codpartido %in% centro02 ~ 1,
                                year == 2010 & codpartido %in% centro10 ~ 1,
                                year == 2014 & codpartido %in% centro14 ~ 1,
                                TRUE ~ 0))
# conserv = 1
cam <- cam %>%
          mutate(cam_conserv = case_when(
                                year == 2010 & codpartido %in% conserv ~ 1,
                                year == 2014 & codpartido %in% conserv ~ 1,
                                TRUE ~ 0))
# tercera via = 1
cam <- cam %>%
         mutate(cam_tv = case_when(
                            year == 2010 & codpartido %in% tv10 ~ 1,
                            year == 2014 & codpartido %in% tv14 ~ 1,
                            TRUE ~ 0))
# izquierda = 1
cam <- cam %>%
         mutate(cam_iz = case_when(
                            year == 2002 & codpartido %in% izqui02 ~ 1,
                            year != 2002 & codpartido %in% izqui06 ~ 1,
                            TRUE ~ 0))
# other minority parties = 1
cam <- cam %>%
         mutate(cam_min = case_when(
                            year == 2002 & codpartido %in% min02 ~ 1,
                            year == 2006 & codpartido %in% min06 ~ 1,
                            year == 2010 & codpartido %in% min10 ~ 1,
                            TRUE ~ 0))
# uribismo = 1
cam <- cam %>%
         mutate(cam_urib = case_when(
                                year == 2002 & codpartido %in% urib02 ~ 1,
                                year == 2006 & codpartido %in% urib06 ~ 1,
                                year == 2010 & codpartido %in% urib10 ~ 1,
                                year == 2014 & codpartido %in% urib14 ~ 1,
                                TRUE ~ 0))

# alternative Santos coding
cam <- cam %>%
          mutate(cam_santos =
                         case_when(
                          (year==2010 | year==2014) & codpartido %in% tv14 ~ 1,
                          TRUE ~ 0))
# uribismo coding w/ alternative Santos coding
cam <- cam %>% mutate(cam_urib2 = if_else(
                                    cam_santos == 1 & cam_urib == 1,
                                    0,
                                    cam_urib))


# cam %>% filter(codpartido == 996) %>% View()

# ideology categorical
cam <- cam %>%
        mutate(cam_ideo =
                 case_when(
                   cam_centro == 1 ~ "CENTRO",
                   cam_conserv == 1 ~ "CONSERVADOR",
                   cam_tv == 1 ~ "TERCERA VIA",
                   cam_iz == 1 ~ "IZQUIERDA",
                   cam_lib == 1 ~ "LIBERAL",
                   cam_min == 1 ~ "MINORITY",
                   cam_urib == 1 ~ "URIBISMO",
                   TRUE ~ ""
                 ))


cam$cam_ideo <- as.factor(cam$cam_ideo)



## 4d) PROPORTION OF SEATS ('curules') PARTY WINS  ----

# create var to count the # of seats won
cam$curules <- as.integer(cam$curules)
cam$sumseat <- ""

cam1 <- cam %>%
          arrange(year, coddept, desc(curules)) %>%
          group_by(year, coddept) %>%
          mutate(
            sumseat = if_else(codmuni == 99 & !is.na(curules),
                              cumsum(curules),
                              as.integer(sumseat)))

# did the seat count work?
# cam1 %>%
#     filter(codmuni==99) %>%
#     arrange(year, coddept, curules) %>%
#     View()

# create var to count # of seats won by party @ dept
# 'dpseat' = dept-party seat
cam1$dpseat <- ""
cam1 <- cam1 %>%
          arrange(year, coddept, desc(curules)) %>%
          group_by(year, coddept, codpartido) %>%
          mutate(
            dpseat = if_else(codmuni == 99 & !is.na(curules),
                             cumsum(curules),
                             as.integer(dpseat)))

# did the seat count work?
# cam1 %>%
#     filter(codmuni==99) %>%
#     arrange(year, coddept, curules, codpartido) %>%
#     View()

# var that gives the seat count in codmuni == 99
# 'dstot' = dept seat total
cam1$dstot <- ""
cam1 <- cam1 %>%
          arrange(year, coddept, desc(sumseat)) %>% 
          group_by(year, coddept) %>%
          mutate(
            dstot = cummax(sumseat))

# proportion of seats won by party
# 'cam_seatpro' = seat proportion
cam1 <- cam1 %>%
          arrange(year, coddept, codmuni) %>%
          group_by(year, coddept, codpartido) %>%
          mutate(
            cam_seatpro = cummax(dpseat)/dstot)

# RANK of proportion of seats won by party
# 'cam_seatproR'
cam1 <- cam1 %>%
          arrange(year, coddept, codmuni, desc(cam_seatpro)) %>%
          group_by(year, coddept) %>% 
          mutate(
            cam_seatproR = dense_rank(desc(cam_seatpro)))



## 4e) CREATE DEPT & MUNI "IDEOLOGY VOTE TOTAL" VAR ----

# check for missing values in codpartido
# elec_cam7 %>% filter(is.na(codpartido)) %>%
#               group_by(nombre) %>%
#               count(is.na(codpartido)) %>% View()

cam1$cam_ideo[cam1$cam_ideo == ""] <- NA

## MUNI IDEOLOGY VOTE TOTAL
# "cam_mivote'
cam1$cam_mivote <- ""
cam2 <- cam1 %>%
  group_by(year, coddept, codmuni, cam_ideo) %>% 
  mutate(
    cam_mivote = sum(votos, na.rm = TRUE))
# cam2 <- cam1 %>%
#   group_by(year, coddept, codmuni, cam_ideo) %>% 
#   mutate(
#     cam_mivote = if_else(!is.na(cam_ideo),
#                          sum(votos),
#                          as.numeric(cam_mivote)))

# check random muni
# cam2 %>% filter(year == 2002 & codmuni == 23419) %>% arrange(cam_ideo) %>% View()


## DEPT IDEOLOGY VOTE TOTAL
# "cam_divote'
cam2$cam_divote <- ""
cam2 %<>%
  group_by(year, coddept, dtot_d, cam_ideo) %>% 
  mutate(
    cam_divote = if_else(codmuni == 99,
                         sum(votos, na.rm = TRUE),
                         as.numeric(cam_divote)))


## 4f) PROPORTION OF SEATS BY IDEOLOGY ----

# create var to count # of seats won by ideology @ dept
# 'diseat' = dept-ideology seat
cam2$cam_diseat <- ""
cam3 <- cam2 %>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(
    cam_diseat = if_else(codmuni == 99 & !is.na(curules),
                         cumsum(curules),
                         as.integer(cam_diseat)))

# proportion of seats won by ideology
# 'cam_seatpro_i' = seat proportion_ideology
cam3 %<>%
  arrange(year, coddept, cam_ideo, desc(cam_diseat)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(
    cam_seatpro_i = cummax(cam_diseat)/dstot)

# cam3 %>% arrange(year, coddept, codmuni, desc(cam_seatpro_i)) %>% View()

# RANK of proportion of seats won by party
# 'cam_seatproR'
cam3$cam_seatpro_i2 <- cam3$cam_seatpro_i
cam3$cam_seatpro_i[is.na(cam3$cam_ideo)] <- NA

cam3 %<>%
  arrange(year, coddept, codmuni, desc(cam_seatpro_i2)) %>%
  group_by(year, coddept) %>% 
  mutate(
    cam_seatpro_iR = dense_rank(desc(cam_seatpro_i)))


## 4g) IDEOLOGY TOTAL @ MUNI ----
# 'cam_mivote'
cam3$cam_mivote <- ""
cam4 <- cam3 %>%
  group_by(year, coddept, codmuni, cam_ideo) %>%
  mutate(
    cam_mivote = sum(votos, na.rm = TRUE))


## 4h) MUNI PARTY & IDEOLOGY VOTE PROPORTION & RANK ----

# party proportion
cam4 %<>%
  group_by(year, coddept, codmuni, codpartido) %>%
  mutate(
    cam_mppro = cam_mpvote/cam_mvote)

# ideology proportion
cam4$cam_mipro <- ""
cam4 %<>%
  group_by(year, coddept, codmuni, cam_ideo) %>%
  mutate(
    cam_mipro = if_else(!is.na(cam_mivote) | !is.na(cam_mvote),
                      cam_mivote/cam_mvote,
                      as.numeric(cam_mipro)))

# muni party proportion
cam4 %<>%
  arrange(year, coddept, codmuni, desc(cam_mppro)) %>%
  group_by(year, coddept, codmuni) %>% 
  mutate(
  cam_mppro_r = dense_rank(desc(cam_mppro)))

# muni ideology proportion
cam4$cam_mipro2 <- cam4$cam_mipro
cam4$cam_mipro[is.na(cam4$cam_ideo)] <- NA

cam4 %<>%
  arrange(year, coddept, codmuni, desc(cam_mipro2)) %>%
  group_by(year, coddept, codmuni) %>% 
  mutate(
    cam_mipro_r = dense_rank(desc(cam_mipro)))



## 4i) MATCH ----

# NOTE: need to do Santos coding normal w/ a 2nd robust coding check w/
#       2010 as TV too

## 4i.1) pres ideology & highest proportion of party seats @ dept ----

## (1.1) match of pres. ideology w/ respective seats won in dept
# 'pidseat' = "president ideology + camara seat won"
# 1 = have a seat + match w/ Pres. ideology/coalition
# 0 = have a seat + don't match Pres. ideology
cam_m1 <- cam4 %>%
    group_by(year, coddept, codmuni) %>% 
    mutate(
      cam_pidseat =
             case_when(
               (curules >= 1 & year!=2014) & cam_ideo == "URIBISMO" ~ 1,
               (curules >= 1 & year==2014) & cam_ideo == "TERCERA VIA" ~ 1,
                curules >= 1 ~ 0))

# cam_m1 %>% filter(year==2014) %>% arrange(year, coddept, codmuni) %>% View()

# fill the values for each ideology that has a seat
cam_m2 <- cam_m1 %>%
    group_by(year, cam_ideo, coddept) %>%
    arrange(year, desc(cam_ideo), coddept, desc(cam_pidseat)) %>%
    fill(cam_pidseat)

# quick look at how it filled
# cam_m2 %>% filter(is.na(cam_m_pidseat) & year==2002) %>% View()
# cam_m2 %>%
#   filter(!is.na(cam_m_pidseat) & year==2002) %>%
#     arrange(year, desc(cam_ideo), coddept, desc(cam_m_pidseat)) %>%
#       View()


## (1.2) alternate pidseat coding
# 0 also means doesn't have a seat + doesn't match ideology
# " " " " " " doesn't have a seat + does match ideology
cam_m3 <- cam_m2 %>% 
    mutate(
      cam_pidseat2 = if_else(is.na(cam_pidseat),
                             0,
                             cam_pidseat))

## (1.3) MATCH ideology + highest proportion of seats won
# pres. ideology have seats AND have highest proportion of seats at dept?
cam_m4 <- cam_m3 %>%
    mutate(
      cam_pidseatR1 = if_else(cam_seatpro_iR == 1 & cam_pidseat == 1,
                              1,
                              0))
# alternate coding: same, but seat proportion is Rank 1 OR Rank 2
cam_m4 <- cam_m4 %>%
    mutate(
      cam_pidseatR1R2 = if_else(
              ((cam_seatpro_iR == 1 | cam_seatpro_iR == 2) & cam_pidseat == 1),
              1,
              0))


## (1.4) 1.3 + does the muni vote for this ideology in the highest proportion?

# need to fill values from dept coding down through each muni
# cam_m4 %>%
#   arrange(year, desc(cam_ideo), coddept, desc(cam_pidseatR1)) %>% View()

cam_m5 <- cam_m4 %>%
            group_by(year, cam_ideo, coddept) %>%
            arrange(year, desc(cam_ideo), coddept, desc(cam_pidseatR1)) %>%
            fill(cam_seatpro_i, cam_seatpro_iR, cam_pidseatR1, cam_pidseatR1R2)

# match var: 'pidmuniR1' = 'pres. ideology' (pid) + 'highest rank seats won' +
#                          'muni votes' (muni) + '1st vote prop is pid' (R1)
cam_m5 <- cam_m5 %>%
            mutate(cam_pidseat_muniR1 = if_else(
              cam_pidseatR1 == 1 & cam_mipro_r == 1,
              1,
              0))
# alternate coding 1: include if ideology vote proportion is Rank 2 as well
cam_m5 <- cam_m5 %>%
    mutate(
      cam_pidseat_muniR1R2 = if_else(
              (cam_pidseatR1 == 1 & (cam_mipro_r == 1 | cam_mipro_r == 2)),
              1,
              0))
# alternate coding 2: alt. coding 1 + including seat share Rank 1 or 2
# useful? seems too distant from elec. power effect
cam_m5 <- cam_m5 %>%
    mutate(cam_pidseatR1R2_muniR1R2 = if_else(
                  (cam_pidseatR1R2 == 1 & (cam_mipro_r == 1 | cam_mipro_r == 2)),
                   1,
                   0))



## 4j) BOGOTA DC CODING ----

# Bogota 2002 doesn't include codmuni==99 Totals, so new vars/coding don't work
bog02 <- cam_m5 %>%
          filter(year == 2002 & coddept == 11)

# take bog02 out of cam data - easier to fix this way
cam_m6 <- cam_m5 %>%
            filter(!(year == 2002 & coddept == 11))

bog02$sumseat <- ""
bog02 %<>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept) %>%
  mutate(sumseat = if_else(codmuni == 11001 & !is.na(curules),
                           cumsum(curules),
                           as.integer(sumseat)))

bog02$dpseat <- ""
bog02 %<>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, codpartido) %>%
  mutate(dpseat = if_else(codmuni == 11001 & !is.na(curules),
                          cumsum(curules),
                          as.integer(dpseat)))

# 'dstot' = dept seat total
bog02$dstot <- ""
bog02 %<>%
  arrange(year, coddept, desc(sumseat)) %>% 
  group_by(year, coddept) %>%
  mutate(dstot = cummax(sumseat))

# 'cam_seatpro' = seat proportion
bog02 %<>%
  arrange(year, coddept, codmuni) %>%
  group_by(year, coddept, codpartido) %>%
  mutate(cam_seatpro = cummax(dpseat)/dstot)

# 'cam_seatproR'
bog02 %<>%
  arrange(year, coddept, codmuni, desc(cam_seatpro)) %>%
  group_by(year, coddept) %>% 
  mutate(cam_seatproR = dense_rank(desc(cam_seatpro)))

bog02$cam_divote <- ""
bog02 %<>%
  group_by(year, coddept, cam_ideo) %>% 
  mutate(cam_divote = if_else(codmuni == 11001,
                              sum(votos),
                              as.numeric(cam_divote)))

# 'diseat' = dept-ideology seat
bog02$cam_diseat <- ""
bog02 %<>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(cam_diseat = if_else(codmuni == 11001 & !is.na(curules),
                          cumsum(curules),
                          as.integer(cam_diseat)))

# 'cam_seatpro_i' = seat proportion_ideology
bog02 %<>%
  arrange(year, coddept, cam_ideo, desc(cam_diseat)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(cam_seatpro_i = cummax(cam_diseat)/dstot)

# 'cam_seatpro_iR'
bog02$cam_seatpro_i2 <- bog02$cam_seatpro_i
bog02$cam_seatpro[is.na(bog02$cam_ideo)] <- NA


bog02 %<>%
  arrange(year, coddept, codmuni, desc(cam_seatpro_i)) %>%
  group_by(year, coddept) %>% 
  mutate(cam_seatpro_iR = dense_rank(desc(cam_seatpro_i)))

bog02 <- bog02 %>%
  group_by(year, coddept, codmuni) %>% 
  mutate(cam_pidseat =
           case_when(
             (curules == 1 & year==2002) & cam_ideo == "URIBISMO" ~ 1,
              curules == 1 ~ 0))

# fill the values for each ideology that has a seat
bog02 <- bog02 %>%
  group_by(year, cam_ideo) %>%
  arrange(year, desc(cam_ideo), coddept, desc(cam_pidseat)) %>%
  fill(cam_pidseat)

## alternate pidseat coding
bog02 <- bog02 %>% 
  mutate(cam_pidseat2 = if_else(is.na(cam_pidseat),
                                0,
                                cam_pidseat))

## 1.3) MATCH ideology + highest proportion of seats won
bog02 <- bog02 %>%
  mutate(cam_pidseatR1 = if_else(
                          cam_seatpro_iR == 1 & cam_pidseat == 1,
                          1,
                          0))
# alternate coding: same, but seat proportion is Rank 1 OR Rank 2
bog02 <- bog02 %>%
  mutate(cam_pidseatR1R2 = if_else(
              ((cam_seatpro_iR == 1 | cam_seatpro_iR == 2) & cam_pidseat == 1),
                1,
                0))

## (1.4) 1.3 + does the muni vote for this ideology in the highest proportion?
bog02 <- bog02 %>%
  group_by(year, cam_ideo, coddept) %>%
  arrange(year, desc(cam_ideo), coddept, desc(cam_pidseatR1)) %>%
  fill(cam_seatpro_i, cam_seatpro_iR, cam_pidseatR1, cam_pidseatR1R2)

# match var: 'pidmuniR1' = 'pres. ideology' (pid) + 'highest rank seats won' +
#                          'muni votes' (muni) + '1st vote prop is pid' (R1)
bog02 <- bog02 %>%
  mutate(cam_pidseat_muniR1 = if_else(
                                  cam_pidseatR1 == 1 & cam_mipro_r == 1,
                                  1,
                                  0))

# alternate coding 1: include if ideology vote proportion is Rank 2 as well
bog02 <- bog02 %>%
  mutate(cam_pidseat_muniR1R2 = if_else(
                  (cam_pidseatR1 == 1 & (cam_mipro_r == 1 | cam_mipro_r == 2)),
                   1,
                   0))
# alternate coding 2: alt. coding 1 + including seat share Rank 1 or 2
bog02 <- bog02 %>%
    mutate(cam_pidseatR1R2_muniR1R2 = if_else(
                (cam_pidseatR1R2 == 1 & (cam_mipro_r == 1 | cam_mipro_r == 2)),
                 1,
                 0))

cam_m7 <- bind_rows(cam_m6, bog02)



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    V. SUMMARIZE CAMARA DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

# recode municipality called Isnos b/c wrong codmuni given
cam_m7$codmuni[cam_m7$codmuni == 41350] <- 41359

## 5a) FILL DEPT TOTAL VARS THROUGH MUNI ----

# fill dept vote & dept seat total
cam_sum1 <- cam_m7 %>%
            group_by(year, coddept) %>%
            arrange(year, coddept, codmuni, desc(dstot)) %>%
            fill(cam_dvote, dstot)

# fill # of seats each ideology has won at the dept level
cam_sum1 <- cam_sum1 %>%
  group_by(year, coddept, cam_ideo) %>% 
  arrange(year, coddept, cam_ideo, cam_diseat) %>%
  fill(cam_diseat)

cam_sum1 <- cam_sum1 %>%
  mutate(
    cam_diseat = if_else(is.na(cam_diseat),
                         0,
                         as.numeric(cam_diseat)))
View(cam_sum1)
  
# cam_sum1 %>%
#   filter(year == 2002, coddept == 5) %>%
#   arrange(year, coddept, cam_ideo, cam_diseat) %>%
#   View()



## 5b) ONLY KEEP CODMUNI OBS (DROP DEPT TOTAL VALUES) ----
cam_sum2 <- cam_sum1 %>% 
              filter(dtot_d == 0) %>%
              arrange(year, codmuni, cam_ideo)

cam_sum2 <- cam_sum2 %>%
  ungroup() %>% 
  mutate(cam_ideo = if_else(cam_ideo == "OTHER",
                                      "MINORITY",
                                       as.character(cam_ideo)))

# change NA in the ideology category to "other"
cam_sum2$cam_ideo[is.na(cam_sum2$cam_ideo)] <- "OTHER"


## 5c) SUMMARIZE (COLLAPSE) DATA TO CODMUNI-YR ----
# NOTE:
#      - only keep the ideology totals for now
#      - as discussed indiv parties don't make sense for muni-yr analysis

## 5c.1) year - muni - ideology df ----
cam_sum3 <- cam_sum2 %>%
  select(-curules, -partido, -cam_dpvote, -dtot_d, -sumseat, -dpseat,
         -cam_seatpro, -cam_seatproR, -cam_divote, -cam_mppro, -cam_mppro_r,
         -primer_apellido, -primer_apellido_suplente, -segundo_apellido,
         -segundo_apellido_suplente, -nombre, -nombre_suplente, -votos,
         -cam_mpvote, -faccion, -codlista) %>%
  group_by(year, coddept, codmuni, cam_ideo) %>%
  summarise_all(max)

## set missing values to 0 for:
# 1) "seat proportion"/"vote share" won by ideology
# 2) "seat proportion"/"vote share" rankings
cam_sum3 <- cam_sum3 %>%
  mutate(
    cam_seatpro_i = if_else(is.na(cam_seatpro_i),
                            0,
                            as.numeric(cam_seatpro_i))) %>% 
  mutate(
    cam_seatpro_iR = if_else(is.na(cam_seatpro_iR),
                             0,
                             as.numeric(cam_seatpro_iR))) %>% 
  mutate(
    cam_mipro = if_else(is.na(cam_mipro),
                             0,
                             as.numeric(cam_mipro))) %>% 
  mutate(
    cam_mipro_r = if_else(is.na(cam_mipro_r),
                             0,
                             as.numeric(cam_mipro_r)))

## 5c.2) reshape ideology categories to contain muni vote share ----
cam_sum3$cam_ideo2 <- cam_sum3$cam_ideo

# reshape ideology var so it contains vote proportion for ideology @ muni
cam_sum4 <- cam_sum3 %>%
  group_by(year, codmuni) %>% 
  spread(cam_ideo, cam_mipro, convert = TRUE, fill = -99)
                            # fill -99 so max() summary function works properly
                            # when obs collapsed further

# rename reshaped columns
cam_sum4 <- cam_sum4 %>%
  rename(c_votepro_centro = CENTRO,
         c_votepro_conserv = CONSERVADOR,
         c_votepro_iz = IZQUIERDA,
         c_votepro_lib = LIBERAL,
         c_votepro_min = MINORITY,
         c_votepro_other = OTHER,
         c_votepro_tv = `TERCERA VIA`,
         c_votepro_urib = URIBISMO)

# reshape seat proportion by ideology
cam_sum4 <- cam_sum4 %>%
  group_by(year, codmuni) %>% 
  spread(cam_ideo2, cam_seatpro_i, convert = TRUE, fill = -99)

# rename reshaped columns
cam_sum4 <- cam_sum4 %>%
  rename(c_seatpro_centro = CENTRO,
         c_seatpro_conserv = CONSERVADOR,
         c_seatpro_iz = IZQUIERDA,
         c_seatpro_lib = LIBERAL,
         c_seatpro_min = MINORITY,
         c_seatpro_other = OTHER,
         c_seatpro_tv = `TERCERA VIA`,
         c_seatpro_urib = URIBISMO)


## 5c.3) mutually exclusive match dummies ----
# c_match_munidept
# c_match_presdept
# c_match_munideptpres
# c_match_none

# Replace missing values so this only counts if pres. ideology
# matches ideology that holds the highest seat proportion at dept.
# Note--0 can mean multiple things.
cam_sum4$cam_pidseatR1[is.na(cam_sum4$cam_pidseatR1)] <- 0

cam_sum5 <- cam_sum4_temp %>%
  mutate(
    c_match_munidept = if_else((cam_seatpro_iR == 1 &
                                cam_mipro_r == 1 &
                                cam_pidseat_muniR1 == 0),
                               1,
                               0)) %>%
  mutate(
    c_match_presdept = if_else((cam_pidseatR1 == 1 &
                                  cam_pidseat_muniR1 == 0),
                               1,
                               0)) %>%
  mutate(
    c_match_munideptpres = if_else(cam_pidseat_muniR1 == 1,
                                   1,
                                   0)) %>%
  mutate(
    c_match_none = if_else((c_match_munidept == 0 &
                             c_match_presdept == 0 &
                             c_match_munideptpres == 0),
                           1,
                           0))

# replace NAs w/ 0
# Note -- NAs not meaningful here. Product of some NAs in how the mutually
#         exclusive vars calculated.
cam_sum5$c_match_presdept[is.na(cam_sum5$c_match_presdept)] <- 0
cam_sum5$c_match_munideptpres[is.na(cam_sum5$c_match_munideptpres)] <- 0
cam_sum5$c_match_none[is.na(cam_sum5$c_match_none)] <- 0

# some munis don't have every ideology accounted for in voting
# cam_sum5 %>%
#   filter(year == 2010 & coddept == 5) %>%
#   # arrange(desc(c_match_munideptpres), desc(c_match_presdept), desc(c_match_munidept), desc(c_match_none)) %>%
#   group_by(codmuni) %>%
#   count(codmuni)



## 5c.4) create distinct codmuni-yr df  ----
# cam_sum5 %>%
#   filter(c_match_munideptpres == 1 | c_match_presdept == 1 |
#                       c_match_munidept ==1 | c_match_none == 1) %>%
#   arrange(year, codmuni, desc(c_match_munideptpres), desc(c_match_presdept), desc(c_match_munidept), desc(c_match_none)) %>%
#   View()

# collapse df so that the match vars are "mututally exclusive" for each
# codmuni-year
# i.e. only 1 of 4 "c_match_xxx" can be a "1"
yes_match <- cam_sum5 %>%
  filter(c_match_munideptpres == 1 | c_match_presdept == 1 |
           c_match_munidept ==1 | c_match_none == 1) %>%
  group_by(year, codmuni) %>%
  summarise_at(vars(c("coddept", ".id", "tipo_eleccion", "municipio",
                                "cam_dvote", "cam_mvote", "dstot",
                                "c_match_munidept", "c_match_presdept",
                                "c_match_munideptpres"),
                              contains("c_votepro_"),
                              contains("c_seatpro_"),
                              contains("c_match_")),
               funs(max, min))

# keep only "c_match_none_min" of "min" summarized vars
# keep all the vars summarized by "max"
yes_match1 <- yes_match %>%
  select(elec_id = .id_max, year, codmuni, ends_with("_max"),
         c_match_none = c_match_none_min,
         -c_match_none_max)

# take "_max" out of var names
yes_match1 %<>% setNames(gsub("_max", "", names(.)))

yes_match1 %<>% select(elec_id, codmuni, year,
                       municipio,
                       starts_with("c_match"),
                       everything())

cam_muniyr <- yes_match1


## 5c.5 fix coding error in the c_match_ vars ----
# some of the c_match_ end up w/ double counting in the match b/c of coalitions
# that are formed

cam_muniyr1 <- cam_muniyr %>% 
  mutate(
    c_match_munideptpres = if_else(
                            c_match_munidept == 1 & c_match_presdept == 1,
                            1,
                            c_match_munideptpres)
  ) %>% 
  mutate(c_match_munidept = if_else(
                              c_match_munidept == 1 & c_match_munideptpres == 1,
                              0,
                              c_match_munidept)
  ) %>% 
  mutate(c_match_presdept = if_else(
                              c_match_presdept == 1 & c_match_munideptpres == 1,
                              0,
                              c_match_presdept)
  )
    

## 5c.5) save muni-year panel ----
setwd(dir.outdata)
write_csv(cam_muniyr1, "camara_muni_year_2002-2014.csv")
save(cam_muniyr1, file = "camara_muni_year_2002-2014.RData")


## 5d) CREATE MUNI CROSS SECTION DATA ----

# replace -99 values back w/ NA
cam_muniyr1 <- na_if(cam_muniyr1, -99)

cam_cs <- cam_muniyr1 %>%
  group_by(codmuni) %>%
  summarise(elec_id = first(elec_id),
            municipio = first(municipio),
            coddept = first(coddept),
            tipo_eleccion = first(tipo_eleccion),
            c_dvote_avg = mean(cam_dvote, na.rm = TRUE),
            c_mvote_avg = mean(cam_mvote, na.rm = TRUE),
            c_deptseats_14 = last(dstot),
            c_match_munidept = mean(c_match_munidept, na.rm = TRUE),
            c_match_presdept = mean(c_match_presdept, na.rm = TRUE),
            c_match_munideptpres = mean(c_match_munideptpres),
            c_match_none = mean(c_match_none, na.rm = TRUE),
            c_votepro_centro = mean(c_votepro_centro, na.rm = TRUE),
            c_votepro_conserv = mean(c_votepro_conserv, na.rm = TRUE),
            c_votepro_iz = mean(c_votepro_iz, na.rm = TRUE),
            c_votepro_lib = mean(c_votepro_lib, na.rm = TRUE),
            c_votepro_min = mean(c_votepro_min, na.rm = TRUE),
            c_votepro_other = mean(c_votepro_other, na.rm = TRUE),
            c_votepro_tv = mean(c_votepro_tv, na.rm = TRUE),
            c_votepro_urib = mean(c_votepro_urib, na.rm = TRUE),
            c_seatpro_centro = mean(c_seatpro_centro, na.rm = TRUE),
            c_seatpro_conserv = mean(c_seatpro_conserv, na.rm = TRUE),
            c_seatpro_iz = mean(c_seatpro_iz, na.rm = TRUE),
            c_seatpro_lib = mean(c_seatpro_lib, na.rm = TRUE),
            c_seatpro_min = mean(c_seatpro_min, na.rm = TRUE),
            c_seatpro_other = mean(c_seatpro_other, na.rm = TRUE),
            c_seatpro_tv = mean(c_seatpro_tv, na.rm = TRUE),
            c_seatpro_urib = mean(c_seatpro_urib, na.rm = TRUE)
            )

# changes values set to NaN to 0 (b/c of 0/0)
cam_cs$c_votepro_centro <- coalesce(cam_cs$c_votepro_centro, 0)
cam_cs$c_votepro_conserv <- coalesce(cam_cs$c_votepro_conserv, 0)
cam_cs$c_votepro_iz <- coalesce(cam_cs$c_votepro_iz, 0)
cam_cs$c_votepro_lib <- coalesce(cam_cs$c_votepro_lib, 0)
cam_cs$c_votepro_min <- coalesce(cam_cs$c_votepro_min, 0)
cam_cs$c_votepro_other <- coalesce(cam_cs$c_votepro_other, 0)
cam_cs$c_votepro_tv <- coalesce(cam_cs$c_votepro_tv, 0)
cam_cs$c_votepro_urib <- coalesce(cam_cs$c_votepro_urib, 0)

cam_cs$c_seatpro_centro <- coalesce(cam_cs$c_seatpro_centro, 0)
cam_cs$c_seatpro_conserv <- coalesce(cam_cs$c_seatpro_conserv, 0)
cam_cs$c_seatpro_iz <- coalesce(cam_cs$c_seatpro_iz, 0)
cam_cs$c_seatpro_lib <- coalesce(cam_cs$c_seatpro_lib, 0)
cam_cs$c_seatpro_min <- coalesce(cam_cs$c_seatpro_min, 0)
cam_cs$c_seatpro_other <- coalesce(cam_cs$c_seatpro_other, 0)
cam_cs$c_seatpro_tv <- coalesce(cam_cs$c_seatpro_tv, 0)
cam_cs$c_seatpro_urib <- coalesce(cam_cs$c_seatpro_urib, 0)

# check NaN recoding
# count(cam_cs1, is.nan(c_seatpro_conserv))
# count(cam_cs1, c_seatpro_conserv == 0)
# count(cam_cs, c_seatpro_conserv == 0)


## 5e) SAVE CS DATA ----
setwd(dir.outdata)
write_csv(cam_cs, "camara_muni_crosssection_2002-2014.csv")
save(cam_cs, file = "camara_muni_crosssection_2002-2014.RData")



# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    VI. SENATE DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #

# rm(list = c("cam", "cam_m1", "cam_m2", "cam_m3", "cam_m4", "elec_cam1",
#             "elec_cam2", "elec_cam3", "elec_cam4", "elec_cam5", "elec_cam6",
#             "elec_cam7", "elec_cam8", "elec_cam9", "total"))

##  IDEOLOGY CODING
# Ideology Vars in Pres. election coding ('pres')
# (these are all dummies)
# TIES:         pol_1 = pres_tie = c_tie = s_tie
# CENTRO:       pol_2 = pres_centro = c_centro = s_centro
# CONSERVADOR:  pol_3 = pres_conserv = c_conserv = s_conserv
# TERCERA VIA:  pol_4 = pres_tv = c_tv = s_tv
# IZQUIERDA:    pol_5 = pres_izqui = c_izqui = s_izqui
# LIBERAL:      pol_6 = pres_lib = c_lib = s_lib
# MINORITY:     pol_7 = pres_min = c_min = s_min
# URIBISMO:     pol_8 = pres_urib = c_urib = s_urib

# Pres. ideology coding:
# 1994 - Liberal
# 1998 - Conservative
# 2002 - Uribe
# 2006 - Uribe
# 2010 - Uribe (Santos candidate)
# 2014 - Tercera via (Santos)
# NOTE: need to do Santos coding normal w/ a 2nd robust coding check w/
#       2010 as TV too


## 6a) VOTE TOTALS ----

## NOTES:
#       - codmuni == 99 means "TOTAL"
#       - coddept ('codep' orignially) == 57 == "national total"
#       - codmuni==99 (so just the "total") is counting the dept level totals
#         for candidiates

# renaming vars to be consistent w/ other data
elec_sen1 <- elec_sen1 %>% rename(year = ano, codmuni = codmpio, coddept = codep)

# take out years 2003 & 2007
sen1 <- elec_sen1 %>% filter(year != 2003 & year!=2007)
# sen1 %>% count(year)

# the national total value, coddept == 57 only in 2002 & 2006
sen1 %>% filter(coddept==57) %>% group_by(year) %>% count(coddept)
sen1 %>% filter(is.na(coddept)) %>% View()

# make coding consistent and make sure "TOTAL NACIONAL" is coddept = 57
sen2 <- sen1 %>% mutate(coddept = if_else(is.na(coddept),
                                  57,
                                  coddept))
sen2 %>% filter(coddept==57) %>% group_by(year) %>% count(coddept)

# dept 'vote totals' DFs
# include Bogota b/c doesn't have a dept "TOTAL" code
s_dept_tot <- sen2 %>% filter(coddept == 11 | (codmuni == 99 & coddept != 57))
s_dept_tot %>% count(coddept) %>% View()

# if needed drop non-relevant election years
# 'ct' = cross-tab
# ct_yr_etype <- table(s_dept_tot$year, s_dept_tot$tipo_eleccion)
# ct_yr_etype
# ct_yr_etype <- table(sen2$year, sen2$tipo_eleccion)
# ct_yr_etype

# drop the dept total observations from sen2
sen3 <- sen2 %>% filter(!(codmuni == 99 & coddept != 57))



## 6b) CALCULATE TOTAL VOTES BY NATL & MUNI ----
# include codlista == 996 & 998 (blank, 'tarjetas no marcadas')
# drop out the "VOTOS NULOS" (codmuni 997, when coded correctly)

# where are the VOTOS NULOS? codlista coded inconsistently for some years
# sen3 %>% filter(codlista == 996 | codlista == 997 | codlista == 998 |
#                      codlista == 998 | primer_apellido == "VOTOS NULOS" |
#                      nombre == "VOTOS NULOS") %>%
#               group_by(year, codlista) %>%
#               count(nombre) %>%  #also look at count(primer_apellido)
#               View()

## 6b.1) Drop/filter Out "VOTOS NULOS" ----
str(sen3$year)
str(sen3$codlista)
sen3$codlista <- as.numeric(sen3$codlista)

# drop "VOTOS NULOS" out
# 2002 w/ codlista = 998 coded wrong (as "VOTOS NULOS")
sen3 %>% filter(year == 2002 & codlista == 998) %>% summarise(n()) # 1113 obs
sen4 <- sen3 %>% filter(!(year == 2002 & codlista == 998))

# recode: codlista==997 is coded for "TARJETAS NO MARCADAS" for 2002
# give 'TARJETAS NO MARCADAS' the correct codlista
sen4 <- sen4 %>% 
  mutate(codlista =
           if_else(year == 2002 & codlista == 997,
                   998,
                   codlista))

# reassign codlista == NA to 997, when the observation is "VOTOS NULOS"
sen5<- sen4 %>%
        mutate(codlista = if_else(((nombre == "VOTOS NULOS" |
                                    primer_apellido == "VOTOS NULOS") &
                                    is.na(codlista)),
                                    997,
                                    codlista))

# filter out VOTOS NULOS
sen5 %>% count(codlista==997) # NA values in codlista!
sen6 <- sen5 %>% filter(codlista != 997 | is.na(codlista))


## 6b.2) Other Vote Total Names w/out Correct Value ----
# other vote total names
votenames <- sen6 %>%
              count(nombre) %>%
              mutate(containvote = stringr::str_detect(nombre, "VOTOS"))
votenames_unique <- votenames$nombre[votenames$containvote==TRUE &
                                       !is.na(votenames$containvote)]

# code "blank votes" w/ correct codlista when it's NA
blanco <- votenames_unique[stringr::str_detect(votenames_unique, "BLANCO")]
sen7 <- sen6 %>%
          mutate(codlista = if_else(nombre %in% blanco,
                                    996,
                                    codlista))

# recode "blank votes" in 2002 so it's consistent with other years
sen7 <- sen7 %>%
          mutate(codlista = if_else(primer_apellido %in% blanco,
                                    996,
                                    codlista))

# code "blank votes" w/ correct codlista when it's NA
marcado <- votenames_unique[stringr::str_detect(votenames_unique, "MARCADOS")]
sen7 <- sen7 %>%
          mutate(codlista = if_else(nombre %in% marcado,
                                    998,
                                    codlista))

# make sure 2002 has codlista
# sen7 %>% filter(year == 2002 & codlista == 998) %>% View()
# sen7 %>% filter(str_detect(primer_apellido, "TARJETAS NO MARCADAS")) %>% View()


## 6b.3) "Natl-Party-VoteTotal" var = 's_npvote' ----
# "s_" meaning "senate"

# replace missing values (NA) in 'votos' w/ 0
sen7 <- sen7 %>% mutate(votos = if_else(is.na(votos),
                                        0,
                                        votos))
# # check 2006 since this occured in large # here
# sen7 %>% filter(year==2006 & coddept==57) %>% View()
sen7$curules <- as.numeric(sen7$curules)

sen7$s_npvote <- ""
sen8 <- sen7 %>%
          group_by(year, coddept, codpartido) %>% 
          mutate(s_npvote = if_else(coddept == 57,
                                      sum(votos),
                                      as.numeric(s_npvote)))

# Note: The election totals are off for 2002. But are consistent for other
#       years. There are also some small discrepancies with Col. govt official
#       vote totals (see elecciones.registraduria.gov.co)
# sen8 %>% filter(year == 2002 & codmuni == 99) %>% ungroup() %>% summarise(sum(votos))
# sen8 %>% filter(year == 2002 & codmuni != 99) %>% ungroup() %>% summarise(sum(votos))
# sen8 %>% filter(year == 2014 & codmuni != 99) %>% ungroup() %>% summarise(sum(votos))
# sen8 %>% filter(year == 2014 & codmuni == 99) %>% ungroup() %>% summarise(sum(votos))

## 6b.4) "Natl-Vote-Total" var = 'nvote' & "Muni-Vote-Total" = 'mvote' ----
sen8$s_nvote <- ""
sen8$s_mvote <- ""

sen8 %<>% mutate(ntot_d = if_else(coddept == 57,
                                  1,
                                  0))
sen8 <- sen8 %>%
  group_by(year, ntot_d) %>% 
  mutate(s_nvote = if_else(coddept == 57,
                             sum(votos),
                             as.numeric(s_nvote))) %>% # national vote var
  ungroup() %>%
  group_by(year, coddept, codmuni) %>%
  mutate(s_mvote = if_else(codmuni != 99,
                             sum(votos),
                             as.numeric(s_mvote))) # muni vote var

## 6b.5) Create "Muni-Party-Vote Total" var = 'mpvote' ----
# check for missing values in codpartido
# sen8 %>% filter(is.na(codpartido)) %>%
#          group_by(nombre) %>%
#          count(is.na(codpartido)) %>% View()

sen8$s_mpvote <- ""
sen9 <- sen8 %>%
  group_by(year, coddept, codmuni, codpartido) %>% 
  mutate(s_mpvote = if_else(!is.na(codpartido),
                              sum(votos),
                              as.numeric(s_mpvote)))
# check random muni
# sen9 %>% filter(year == 2002 & codmuni == 23419) %>% arrange(codpartido) %>% View()


## 6c) CODING IDEOLOGY DUMMIES ----

## 5c.1) Camara Ideology Dummy Vars
# liberal = 1
sen10 <- sen9 %>%
        mutate(s_lib = if_else(codpartido %in% lib,  # liberal dummy
                                 1,
                                 0))
# centro = 1
sen10 <- sen10 %>%
          mutate(s_centro = case_when(
                                year == 2002 & codpartido %in% centro02 ~ 1,
                                year == 2010 & codpartido %in% centro10 ~ 1,
                                year == 2014 & codpartido %in% centro14 ~ 1,
                                TRUE ~ 0))
# conserv = 1
sen10 <- sen10 %>%
          mutate(s_conserv = case_when(
                                     year == 2010 & codpartido %in% conserv ~ 1,
                                     year == 2014 & codpartido %in% conserv ~ 1,
                                     TRUE ~ 0))
# tercera via = 1
sen10 <- sen10 %>%
          mutate(s_tv = case_when(
                            year == 2010 & codpartido %in% tv10 ~ 1,
                            year == 2014 & codpartido %in% tv14 ~ 1,
                            TRUE ~ 0))
# izquierda = 1
sen10 <- sen10 %>%
          mutate(s_iz = case_when(
                            year == 2002 & codpartido %in% izqui02 ~ 1,
                            year != 2002 & codpartido %in% izqui06 ~ 1,
                            TRUE ~ 0))
# other minority parties = 1
sen10 <- sen10 %>%
          mutate(s_min = case_when(
                            year == 2002 & codpartido %in% min02 ~ 1,
                            year == 2006 & codpartido %in% min06 ~ 1,
                            year == 2010 & codpartido %in% min10 ~ 1,
                            TRUE ~ 0))
# uribismo = 1
sen10 <- sen10 %>%
          mutate(s_urib = case_when(
                            year == 2002 & codpartido %in% urib02 ~ 1,
                            year == 2006 & codpartido %in% urib06 ~ 1,
                            year == 2010 & codpartido %in% urib10 ~ 1,
                            year == 2014 & codpartido %in% urib14 ~ 1,
                            TRUE ~ 0))

# alternative Santos coding
sen10 <- sen10 %>%
      mutate(s_santos = case_when(
                          (year==2010 | year==2014) & codpartido %in% tv14 ~ 1,
                          TRUE ~ 0))
# uribismo coding w/ alternative Santos coding
sen10 <- sen10 %>% mutate(s_urib2 = if_else(
                                      s_santos == 1 & s_urib == 1,
                                      0,
                                      s_urib))

# cam %>% filter(codpartido == 996) %>% View()

# ideology categorical
sen10 <- sen10 %>%
  mutate(s_ideo =
           case_when(
             s_centro == 1 ~ "CENTRO",
             s_conserv == 1 ~ "CONSERVADOR",
             s_tv == 1 ~ "TERCERA VIA",
             s_iz == 1 ~ "IZQUIERDA",
             s_lib == 1 ~ "LIBERAL",
             s_min == 1 ~ "MINORITY",
             s_urib == 1 ~ "URIBISMO",
             TRUE ~ ""
           ))

sen10$s_ideo <- as.factor(sen10$s_ideo)


## 6d) PROPORTION OF SEATS ('curules') PARTY WINS  ----

# fix coding error in data -- overcounting of seats wons
sen11 <- subset(sen10, !(year == 2002 & curules == 1 & votos == 0 & !is.na(curules)))
sen11 <- subset(sen11, !(year == 2014 & curules == 1 & votos == 1 & !is.na(curules)))

# create var to count the # of seats won
sen11$sumseat <- ""
sen11 %<>%
          arrange(year, coddept, desc(curules)) %>%
          group_by(year, coddept) %>%
          mutate(
            sumseat = if_else(coddept == 57 & !is.na(curules),
                              cumsum(curules),
                              as.numeric(sumseat)))

# did the seat count work?
# sen11 %>%
#   filter(coddept==57) %>%
#   arrange(year, coddept, curules) %>%
#     View()

# create var to count # of seats won by party @ natl level
# 'npseat' = dept-party seat
sen11$s_npseat <- ""
sen11 %<>%
    arrange(year, coddept, desc(curules)) %>%
    group_by(year, coddept, codpartido) %>%
      mutate(
        s_npseat = if_else(coddept == 57 & !is.na(curules),
                           cumsum(curules),
                           as.numeric(s_npseat)))
# did the seat count work?
# sen11 %>%
#     filter(coddept==57) %>%
#     arrange(year, coddept, curules, codpartido) %>%
#     View()

# var that gives the seat count in coddept == 57
# 'nstot' = natl seat total
sen11$s_nstot <- ""
sen11 <- sen11 %>%
          arrange(year, codmuni, desc(sumseat)) %>% 
          group_by(year, codmuni) %>%
          mutate(
            s_nstot = cummax(sumseat))

# proportion of seats won by party
# 's_seatpro' = seat proportion
sen11 <- sen11 %>%
          arrange(year, codpartido) %>%
          group_by(year, coddept, codpartido) %>%
          mutate(
            s_seatpro = cummax(s_npseat)/s_nstot)

# RANK of proportion of seats won by party
# 's_seatproR'
sen11 <- sen11 %>%
          arrange(year, codmuni, desc(s_seatpro)) %>%
          group_by(year, coddept) %>% 
          mutate(
            s_seatproR = dense_rank(desc(s_seatpro)))



## 6e) NATL & MUNI "IDEOLOGY VOTE TOTAL" VAR ----

# check for missing values in codpartido
# sen11 %>% filter(is.na(codpartido)) %>%
#           group_by(nombre) %>%
#           count(is.na(codpartido)) %>% View()

# missing ideology as NA
sen12 <- sen11
sen12$s_ideo[sen12$s_ideo == ""] <- NA 

## 6e.1) Muni Ideology Vote Total ----
# "s_mivote'
sen12$s_mivote <- ""
sen13 <- sen12 %>%
  group_by(year, coddept, codmuni, s_ideo) %>% 
  mutate(
    s_mivote = sum(votos, na.rm = TRUE))

# check random muni
# sen13 %>% filter(year == 2002 & codmuni == 23419) %>% arrange(s_ideo) %>% View()


## 6e.2) Natl Ideology Vote Total ----
# "s_nivote'
sen13$s_nivote <- ""
sen13 %<>%
          group_by(year, coddept, s_ideo) %>% 
          mutate(s_nivote = if_else(coddept == 57,
                                    sum(votos, na.rm = TRUE),
                                    as.numeric(s_nivote)))



## 6f) PROPORTION OF SEATS BY IDEOLOGY ----

## var to count # of seats won by ideology @ natl
# 's_niseat' = natl-ideology seats won
sen13$s_niseat <- ""
sen14 <- sen13 %>%
          arrange(year, codmuni, desc(curules)) %>%
          group_by(year, coddept, s_ideo) %>%
          mutate(
            s_niseat = if_else(coddept == 57 & !is.na(curules),
                               cumsum(curules),
                               as.numeric(s_niseat)))

## proportion of seats won by ideology
# 's_seatpro_i' = seat proportion_ideology
sen14 %<>%
          arrange(year, codmuni, s_ideo, desc(s_niseat)) %>%
          group_by(year, coddept, s_ideo) %>%
          mutate(
            s_seatpro_i = cummax(s_niseat)/s_nstot)

sen14$s_seatpro_i2 <- sen14$s_seatpro_i
# sen14 %>% arrange(year, codmuni, desc(s_seatpro_i)) %>% View()

sen14$s_seatpro_i[is.na(sen14$s_ideo)] <- NA

# RANK of proportion of seats won by party
# 's_seatpro_iR'
sen14$s_seatpro_iR <- ""
sen14 %<>%
  arrange(year, codmuni, desc(s_seatpro_i2)) %>%
  group_by(year, coddept) %>% 
  mutate(
    s_seatpro_iR = if_else(
      coddept == 57 & curules >=1 & !is.na(s_ideo),
      dense_rank(desc(s_seatpro_i)),
      as.integer(s_seatpro_iR)))


## 6g) IDEOLOGY TOTAL @ MUNI ----
# 's_mivote'
sen14$s_mivote <- ""
sen15 <- sen14 %>%
  group_by(year, coddept, codmuni, s_ideo) %>%
  mutate(
    s_mivote = sum(votos, na.rm = TRUE))



## 6h) MUNI PARTY & IDEOLOGY VOTE PROPORTION & RANK ----

# party proportion
sen16 <- sen15 %>%
          group_by(year, coddept, codmuni) %>%
          mutate(
            s_mppro = s_mpvote/s_mvote)
# ideology proportion
sen16 %<>%
  group_by(year, coddept, codmuni) %>%
  mutate(
    s_mipro = s_mivote/s_mvote)

# muni party proportion
sen16 %<>%
  arrange(year, coddept, codmuni, desc(s_mppro)) %>%
  group_by(year, coddept, codmuni) %>% 
  mutate(
    s_mppro_r = dense_rank(desc(s_mppro)))

# muni ideology proportion
sen16$s_mipro2 <- sen16$s_mipro
sen16$s_mipro[is.na(sen16$s_ideo)] <- NA

sen16$s_mipro_r <- ""
sen16 %<>%
  arrange(year, coddept, codmuni, desc(s_mipro2)) %>%
  group_by(year, coddept, codmuni) %>% 
  mutate(
    s_mipro_r = if_else(
      codmuni != 99 & !is.na(s_ideo),
      dense_rank(desc(s_mipro)),
      as.integer(s_mipro_r)))

# ranking work?
# sen16 %>%
#   filter(coddept == 8) %>% arrange(year, coddept, codmuni, desc(s_mipro2)) %>% View()


## 6i) MATCH ----

# NOTE: need to do Santos coding normal w/ a 2nd robust coding check w/
#       2010 as TV too

## 6i.1) pres ideology & highest proportion of party seats @ natl level ----

## match pres. ideology w/ respective seats won in natl constituency
# 'pidseat' = "president ideology + senate seat won"
# 1 = have a seat + match w/ Pres. ideology/coalition
# 0 = have a seat + don't match Pres. ideology
sen_m1 <- sen16 %>%
    group_by(year, codmuni) %>% 
      mutate(s_pidseat =
                  case_when(
                    (curules >= 1 & year!=2014) & s_ideo == "URIBISMO" ~ 1,
                    (curules >= 1 & year==2014) & s_ideo == "TERCERA VIA" ~ 1,
                    curules >= 1 ~ 0))

# sen_m1 %>% filter(year==2014) %>% arrange(year, codmuni) %>% View()

# fill the values for each ideology that has a seat
sen_m2 <- sen_m1 %>%
            group_by(year, s_ideo) %>%
            arrange(year, desc(s_ideo), codmuni, desc(s_pidseat)) %>%
            fill(s_pidseat)

# quick look at how it filled
# sen_m2 %>% filter(is.na(s_pidseat) & year==2002) %>% View()
# sen_m2 %>%
#   filter(year==2002) %>% 
#     arrange(year, desc(s_ideo), codmuni, desc(s_pidseat)) %>% View()
# sen_m2 %>% filter(s_ideo == "OTHER") %>% View()

## alternate pidseat coding
# 0 also means doesn't have a seat + doesn't match ideology
# " " " " " " doesn't have a seat + does match ideology
sen_m3 <- sen_m2 %>% 
            mutate(
              s_pidseat2 = if_else(is.na(s_pidseat),
                                   0,
                                   s_pidseat))

## 6i.2) MATCH ideology + highest proportion of seats won ----
# pres. ideology have seats AND have highest proportion of seats @ natl?
sen_m4 <- sen_m3 %>%
            mutate(
              s_pidseatR1 = if_else(s_seatpro_iR == 1 & s_pidseat == 1,
                                    1,
                                    0))
# alternate coding: same, but seat proportion is Rank 1 OR Rank 2
sen_m4 <- sen_m4 %>%
  mutate(
    s_pidseatR1R2 = if_else(
              ((s_seatpro_iR == 1 | s_seatpro_iR == 2) & s_pidseat == 1),
              1,
              0))


## 6i.3) i.2 + is highest muni vote share for pres ideology? ----

# need to fill values from natl coding down through each muni
# sen_m4 %>%
#   arrange(year, desc(s_ideo), codmuni, desc(s_pidseatR1)) %>% View()

sen_m5 <- sen_m4 %>%
            group_by(year, s_ideo) %>%
            arrange(year, desc(s_ideo), codmuni, desc(s_pidseatR1)) %>%
            fill(s_seatpro_i, s_seatpro_iR, s_pidseatR1, s_pidseatR1R2)

## match var: 'pidmuniR1' = 'pres. ideology' (pid) + 'highest rank seats won' +
#                          'muni votes' (muni) + '1st vote prop is pid' (R1)
sen_m5 <- sen_m5 %>%
  mutate(
    s_pidseat_muniR1 = if_else(s_pidseatR1 == 1 & s_mipro_r == 1,
                               1,
                               0))
# alternate coding 1: include if ideology vote proportion is Rank 2 as well
sen_m5 <- sen_m5 %>%
    mutate(
      s_pidseat_muniR1R2 = if_else(
                (s_pidseatR1 == 1 & (s_mipro_r == 1 | s_mipro_r == 2)),
                1,
                0))
# alternate coding 2: alt. coding 1 + including seat share Rank 1 or 2
# useful? seems too distant from elec. power effect
sen_m5 <- sen_m5 %>%
  mutate(
    s_pidseatR1R2_muniR1R2 = if_else(
            (s_pidseatR1R2 == 1 & (s_mipro_r == 1 | s_mipro_r == 2)),
            1,
            0))

# spot checking vars coded correctly
# sen_m5 %>% filter(year == 2010) %>% arrange(year, desc(s_ideo), codmuni, desc(s_pidseatR1)) %>% View()
# sen_m5 %>% filter(year == 2010 & s_ideo == "CONSERVADOR") %>% arrange(year, desc(s_ideo), codmuni, desc(s_pidseatR1)) %>% View()
# sen_m5 %>% filter(year == 2014 & s_ideo == "TERCERA VIA") %>% arrange(year, desc(s_ideo), codmuni, desc(s_pidseatR1)) %>% View()



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
####    VII. SUMMARIZE SENATE DATA    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

## 7a) FILL DEPT TOTAL VARS THROUGH MUNI ----

# fill natl vote & natl seat total
sen_sum1 <- sen_m5 %>%
  ungroup() %>% 
  arrange(year, codmuni, desc(s_nstot)) %>%
  fill(s_nvote, s_nstot)

# fill # of seats each ideology has won (@ the natl level)
sen_sum1 %<>%
  group_by(year, s_ideo) %>% 
  arrange(year, s_ideo, desc(s_niseat)) %>%
  fill(s_niseat)

sen_sum1 %<>%
  mutate(
    s_niseat = if_else(is.na(s_niseat),
                         0,
                         as.numeric(s_niseat)))

sen_sum1 %<>%
  ungroup() %>% 
  arrange(year, s_ideo, codmuni) %>% 
  fill(s_nivote)


## 7b) ONLY KEEP CODMUNI OBS (DROP DEPT TOTAL VALUES) ----
sen_sum2 <- sen_sum1 %>% 
  filter(ntot_d == 0) %>%
  arrange(year, codmuni, s_ideo)

# change NA in the ideology category to "other"
sen_sum2$s_ideo <- as.character(sen_sum2$s_ideo)
sen_sum2$s_ideo[is.na(sen_sum2$s_ideo)] <- "OTHER"
sen_sum2$s_ideo <- as.factor(sen_sum2$s_ideo)


## 7c) SUMMARIZE (COLLAPSE) DATA TO CODMUNI-YR ----
# NOTE:
#      - only keep the ideology totals for now
#      - as discussed indiv parties don't make sense for muni-yr analysis

## 7c.1) year - muni - ideology df ----
sen_sum3 <- sen_sum2 %>%
  select(-curules, -partido, -s_npvote, -ntot_d, -sumseat, -s_npseat,
         -s_seatpro, -s_seatproR, -s_nivote, -s_mppro, -s_mppro_r,
         -primer_apellido, -primer_apellido_suplente, -segundo_apellido,
         -segundo_apellido_suplente, -nombre, -nombre_suplente, -votos,
         -s_mpvote, -faccion, -codlista) %>%
  group_by(year, coddept, codmuni, s_ideo) %>%
  summarise_all(max)


## set missing values to 0 for:
# 1) "seat proportion"/"vote share" won by ideology
# 2) "seat proportion"/"vote share" rankings
sen_sum3 %<>%
  mutate(
    s_seatpro_i = if_else(is.na(s_seatpro_i),
                            0,
                            as.numeric(s_seatpro_i))) %>% 
  mutate(
    s_seatpro_iR = if_else(is.na(s_seatpro_iR),
                             0,
                             as.numeric(s_seatpro_iR))) %>% 
  mutate(
    s_mipro = if_else(is.na(s_mipro),
                        0,
                        as.numeric(s_mipro))) %>% 
  mutate(
    s_mipro_r = if_else(is.na(s_mipro_r),
                          0,
                          as.numeric(s_mipro_r)))

## 7c.2) reshape ideology categories to contain muni vote share ----
sen_sum3$s_ideo2 <- sen_sum3$s_ideo
sen_sum3$s_ideo3 <- sen_sum3$s_ideo

# reshape ideology var so it contains vote proportion for ideology @ muni

# setwd(dir.outdata)
# save(sen_sum3, file = "senate_muni_year_2002-2014.RData")
# load(file = "senate_muni_year_2002-2014.RData")

sen_sum4 <- sen_sum3 %>%
  group_by(year, codmuni) %>% 
  spread(s_ideo, s_mipro, convert = TRUE, fill = -99)
# fill -99 so max() summary function works properly
# when obs collapsed further

# rename reshaped columns
sen_sum4 %<>%
  rename(s_votepro_centro = CENTRO,
         s_votepro_conserv = CONSERVADOR,
         s_votepro_iz = IZQUIERDA,
         s_votepro_lib = LIBERAL,
         s_votepro_min = MINORITY,
         s_votepro_other = OTHER,
         s_votepro_tv = `TERCERA VIA`,
         s_votepro_urib = URIBISMO)

# reshape seat proportion by ideology
sen_sum4 %<>%
  group_by(year, codmuni) %>% 
  spread(s_ideo2, s_seatpro_i, convert = TRUE, fill = -99)

# rename reshaped columns
sen_sum4 %<>% 
  rename(s_seatpro_centro = CENTRO,
         s_seatpro_conserv = CONSERVADOR,
         s_seatpro_iz = IZQUIERDA,
         s_seatpro_lib = LIBERAL,
         s_seatpro_min = MINORITY,
         s_seatpro_other = OTHER,
         s_seatpro_tv = `TERCERA VIA`,
         s_seatpro_urib = URIBISMO)

# reshape muni ideology vote share rankings
sen_sum4$s_miproR <- sen_sum4$s_mipro_r

sen_sum4 %<>%
  group_by(year, codmuni) %>% 
  spread(s_ideo3, s_miproR, convert = TRUE, fill = -99)
  
sen_sum4 %<>% 
  rename(s_miproR_centro = CENTRO,
         s_miproR_conserv = CONSERVADOR,
         s_miproR_iz = IZQUIERDA,
         s_miproR_lib = LIBERAL,
         s_miproR_min = MINORITY,
         s_miproR_other = OTHER,
         s_miproR_tv = `TERCERA VIA`,
         s_miproR_urib = URIBISMO)


## 7c.3) mutually exclusive match dummies ----

# Replace missing values so this only counts if pres. ideology
# matches ideology that holds the highest seat proportion at dept.
# Note--0 can mean multiple things.
sen_sum4$s_pidseatR1[is.na(sen_sum4$s_pidseatR1)] <- 0

sen_sum5 <- sen_sum4 %>%
  mutate(
    s_match_muniseat = if_else((s_seatpro_iR == 1 &
                                  s_mipro_r == 1 &
                                  s_pidseat_muniR1 == 0),
                               1,
                               0)) %>%
  mutate(
    s_match_presseat = if_else((s_pidseatR1 == 1 &
                                  s_pidseat_muniR1 == 0),
                               1,
                               0)) %>%
  mutate(
    s_match_muniseatpres = if_else(s_pidseat_muniR1 == 1,
                                   1,
                                   0)) %>%
  mutate(
    s_match_none = if_else((s_match_muniseat == 0 &
                              s_match_presseat == 0 &
                              s_match_muniseatpres == 0),
                           1,
                           0))

# replace NAs w/ 0
# Note -- NAs not meaningful here. Product of some NAs in how the mutually
#         exclusive vars calculated.
sen_sum5$s_match_presseat[is.na(sen_sum5$s_match_presseat)] <- 0
sen_sum5$s_match_muniseatpres[is.na(sen_sum5$s_match_muniseatpres)] <- 0
sen_sum5$s_match_none[is.na(sen_sum5$s_match_none)] <- 0

# some munis don't have every ideology accounted for in voting
# sen_sum5 %>%
#   filter(coddept == 5) %>%
#   arrange(desc(s_match_muniseatpres), desc(s_match_presseat), desc(s_match_muniseat), desc(s_match_none)) %>%
# group_by(codmuni) %>%
# count(codmuni)



## 7c.4) create distinct codmuni-yr df  ----
# sen_sum5 %>%
#   filter(s_match_muniseatpres == 1 | s_match_presseat == 1 |
#                       s_match_muniseat ==1 | s_match_none == 1) %>%
#   arrange(year, codmuni, desc(s_match_muniseatpres), desc(s_match_presseat), desc(s_match_muniseat), desc(s_match_none)) %>%
#   View()

# collapse df so that the match vars are "mututally exclusive" for each
# codmuni-year
# i.e. only 1 of 4 "s_match_xxx" can be a "1"
yes_match <- sen_sum5 %>%
  filter(s_match_muniseatpres == 1 | s_match_presseat == 1 |
           s_match_muniseat ==1 | s_match_none == 1) %>%
  group_by(year, codmuni) %>%
  summarise_at(vars(c("coddept", ".id", "tipo_eleccion", "municipio",
                      "s_nvote", "s_mvote", "s_nstot"),
                      # "s_match_muniseat", "s_match_presseat",
                      # "s_match_muniseatpres"),
                    contains("s_votepro_"),
                    contains("s_seatpro_"),
                    contains("s_match_"),
                    contains("s_miproR_")),
               funs(max, min))

# keep only "c_match_none_min" of "min" summarized vars
# keep all the vars summarized by "max"
yes_match1 <- yes_match %>%
  select(elec_id = .id_max, year, codmuni, ends_with("_max"),
         s_match_none = s_match_none_min,
         -s_match_none_max)

# take "_max" out of var names
yes_match1 %<>% setNames(gsub("_max", "", names(.)))

yes_match1 %<>% select(elec_id, codmuni, year,
                       municipio,
                       starts_with("s_match"),
                       everything())

sen_muniyr <- yes_match1


## 7c.5) fix coding error in the s_match_ vars ----
# some of the s_match_ end up w/ double counting in the match b/c of coalitions
# that are formed

sen_muniyr1 <- sen_muniyr %>% 
  mutate(
    s_match_muniseatpres = if_else(
                            s_match_muniseat == 1 & s_match_presseat == 1,
                            1,
                            s_match_muniseatpres)
  ) %>% 
  mutate(s_match_muniseat = if_else(
                              s_match_muniseat == 1 & s_match_muniseatpres == 1,
                              0,
                              s_match_muniseat)
  ) %>% 
  mutate(s_match_presseat = if_else(
                              s_match_presseat == 1 & s_match_muniseatpres == 1,
                              0,
                              s_match_presseat)
  )




## 7c.6) save muni-year panel ----
setwd(dir.outdata)
write_csv(sen_muniyr1, "senate_muni_year_2002-2014.csv")



## 7d) CREATE MUNI CROSS SECTION DATA ----

# replace -99 values back w/ NA
sen_muniyr1 <- na_if(sen_muniyr1, -99)

sen_cs <- sen_muniyr1 %>%
  group_by(codmuni) %>%
  summarise(elec_id = first(elec_id),
            municipio = first(municipio),
            coddept = first(coddept),
            tipo_eleccion = first(tipo_eleccion),
            s_nvote_avg = mean(s_nvote, na.rm = TRUE),
            s_mvote_avg = mean(s_mvote, na.rm = TRUE),
            s_natlseats_14 = last(s_nstot),
            s_match_muniseat = mean(s_match_muniseat, na.rm = TRUE),
            s_match_presseat = mean(s_match_presseat, na.rm = TRUE),
            s_match_muniseatpres = mean(s_match_muniseatpres),
            s_match_none = mean(s_match_none, na.rm = TRUE),
            s_votepro_centro = mean(s_votepro_centro, na.rm = TRUE),
            s_votepro_conserv = mean(s_votepro_conserv, na.rm = TRUE),
            s_votepro_iz = mean(s_votepro_iz, na.rm = TRUE),
            s_votepro_lib = mean(s_votepro_lib, na.rm = TRUE),
            s_votepro_min = mean(s_votepro_min, na.rm = TRUE),
            s_votepro_other = mean(s_votepro_other, na.rm = TRUE),
            s_votepro_tv = mean(s_votepro_tv, na.rm = TRUE),
            s_votepro_urib = mean(s_votepro_urib, na.rm = TRUE),
            s_seatpro_centro = mean(s_seatpro_centro, na.rm = TRUE),
            s_seatpro_conserv = mean(s_seatpro_conserv, na.rm = TRUE),
            s_seatpro_iz = mean(s_seatpro_iz, na.rm = TRUE),
            s_seatpro_lib = mean(s_seatpro_lib, na.rm = TRUE),
            s_seatpro_min = mean(s_seatpro_min, na.rm = TRUE),
            s_seatpro_other = mean(s_seatpro_other, na.rm = TRUE),
            s_seatpro_tv = mean(s_seatpro_tv, na.rm = TRUE),
            s_seatpro_urib = mean(s_seatpro_urib, na.rm = TRUE)
  )

# changes values set to NaN to 0 (b/c of 0/0)
sen_cs$s_votepro_centro <- coalesce(sen_cs$s_votepro_centro, 0)
sen_cs$s_votepro_conserv <- coalesce(sen_cs$s_votepro_conserv, 0)
sen_cs$s_votepro_iz <- coalesce(sen_cs$s_votepro_iz, 0)
sen_cs$s_votepro_lib <- coalesce(sen_cs$s_votepro_lib, 0)
sen_cs$s_votepro_min <- coalesce(sen_cs$s_votepro_min, 0)
sen_cs$s_votepro_other <- coalesce(sen_cs$s_votepro_other, 0)
sen_cs$s_votepro_tv <- coalesce(sen_cs$s_votepro_tv, 0)
sen_cs$s_votepro_urib <- coalesce(sen_cs$s_votepro_urib, 0)

sen_cs$s_seatpro_centro <- coalesce(sen_cs$s_seatpro_centro, 0)
sen_cs$s_seatpro_conserv <- coalesce(sen_cs$s_seatpro_conserv, 0)
sen_cs$s_seatpro_iz <- coalesce(sen_cs$s_seatpro_iz, 0)
sen_cs$s_seatpro_lib <- coalesce(sen_cs$s_seatpro_lib, 0)
sen_cs$s_seatpro_min <- coalesce(sen_cs$s_seatpro_min, 0)
sen_cs$s_seatpro_other <- coalesce(sen_cs$s_seatpro_other, 0)
sen_cs$s_seatpro_tv <- coalesce(sen_cs$s_seatpro_tv, 0)
sen_cs$s_seatpro_urib <- coalesce(sen_cs$s_seatpro_urib, 0)

# check NaN recoding
# count(cam_cs1, is.nan(c_seatpro_conserv))
# count(cam_cs1, c_seatpro_conserv == 0)
# count(cam_cs, c_seatpro_conserv == 0)


## 7e) SAVE CS DATA ----
setwd(dir.outdata)
write_csv(sen_cs, "senate_muni_crosssection_2002-2014.csv")
save(sen_cs, file = "senate_muni_crosssection_2002-2014.RData")




# =-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    VIII. PRES DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=- #

# dmyr_pres %>%
#   select(year, codmuni, department, municipality, starts_with("pol_"), starts_with("pres_"), everything()) %>%
#   arrange(year, codmuni) %>%
#   View()

## MATCH B/W PRES. PARTY IDEOLOGY & IDEOLOGY W/ HIGHEST VOTE SHARE @ MUNI
dmyr_pres <- dmyr_pres %>%
  group_by(year, codmuni) %>%
  mutate(pres_pidmuni = case_when(
                          year == 1998 & pres_conserv == 1 ~ 1,
                          year == 2002 & pres_urib == 1 ~ 1,
                          year == 2006 & pres_urib == 1 ~ 1,
                          year == 2010 & pres_urib == 1 ~ 1,
                          year == 2014 & pres_tv == 1 ~ 1,
                          TRUE ~ 0))

setwd(dir.outdata)
write_csv(dmyr_pres, "dmyr_pres.csv", na = ".")
save(dmyr_pres, file = "dmyr_pres.RData")
# load("dmyr_pres.RData")



# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-= # 
####    IX. MERGE CAM & SEN W/ DMYR_PRES    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-= #
# setwd(dir.outdata)

## 9a) LOAD DATA IF STARTING FROM HERE ----
# sen_cs <- read_csv(file = "senate_muni_crosssection_2002-2014.RData")
# load(file = "senate_muni_crosssection_2002-2014.RData")

# specify "cam_seatpro_min" (25th column) to be read as col_double()
# if you don't will throw parsing error
# cam_cs <- read_csv("camara_muni_crosssection_2002-2014.csv",
#                    col_types = "????????????????????????d???")
# load(file = "camara_muni_crosssection_2002-2014.RData")

# specify cols as below or error occurs
# dmyr_pres <- read_csv("dmyr_pres.csv",
#                       na = c(".", "", "NA"),
#                       col_types = cols(
#                         dep_mun_id = "d",
#                         eco_64 = "d",
#                         eco_65 = "d",
#                         eco_67 = "d",
#                         eco_68 = "d",
#                         eco_70 = "d",
#                         eco_76 = "d",
#                         eco_77 = "d",
#                         fid = "d",
#                         natural_parks = "d",
#                         nonstate_land = "d",
#                         tot_teachers = "d",
#                         year_founded_muni = "d"
#                       ))

# replace "2147483621" w/ NA
# an error somewhere in the previous data wrote this # instead of "NA"
# dmyr_pres <- na_if(dmyr_pres, 2147483621)
# dmyr_pres <- na_if(dmyr_pres, 2147483648)

# save(dmyr_pres, "dmyr_pres.RData")

# load(file = "dmyr_pres.RData")

## 9b) JOIN CAM, SEN, DMYR_PRES DATA
# dmyr_elec_vio <- left_join(dmyr_pres, cam_cs, by = "codmuni")
# dmyr_elec_vio1 <- left_join(dmyr_elec_vio, sen_cs, by = "codmuni")

setwd(dir.outdata)
# write_csv(dmyr_elec_vio1, "dmyr_elec_vio.csv", na = ".") # na = . for Stata
# save(dmyr_elec_vio1, file = "dmyr_elec_vio.RData")
load(file = "dmyr_elec_vio.RData")


## 9c) KEEP ONLY NEEDED VARS ----
varnamedf <- as.data.frame(variable.names(dmyr_elec_vio1))

# select vars using index b/c there are many; might need to double check
# that the right vars are being selected
co_sum <- dmyr_elec_vio1 %>%
  select(codmuni, department, municipality, year,
         upstream, road0, road5, road6, slope_min, slope_max, slope_mean,
         slope_std, SPI12m, cropsha, coca_km2, votes_no, votes_yes, votes_valid,
         pleb_vote_total, pleb_vote_percent, farc_zone, illegal_crops,
         trendc3, sterr_c3, tstat_c3, trendcrop, sterr_cr, tstat_cr,
         trendcropveg, sterr_cv, tstat_cv, trendgrass, sterr_g, tstat_g,
         admin2Pcod, id_1, id_5,
         num_range("geo_", 1:13),
         num_range("demo_", 1:17),
         num_range("eco_", 33:80),
         num_range("pol_", 9:18),
         num_range("vio_", 1:87),
         pres_tie, pres_centro, pres_conserv, pres_izqui, pres_tv, pres_lib,
         pres_min, pres_urib, pres_pidmuni,
         elec_type = tipo_eleccion.x,
         starts_with("c_"), starts_with("s_"), # cam & sen vars
         attack_victims, combatant_attacks, attack_on_civilians,
         wounded, killed, massacre_victims, terrorist_victims, war_victims,
         war_combatants, war_civilian_victims, fid, shape_area, coddepto,
         codprovincia, act_adm, gandina, gcaribe, gpacifica, gorinoquia,
         gamazonia, infant_mortality, sq_km_muni, sq_hectares_muni, altitude,
         dist_to_capital, dist_to_food_market, dist_to_bogota, distance_market,
         codmarket, gini_province, ethnic_minorities, natural_parks, religious,
         notaries, agbank, bank, savings_coop, catholic_church,
         noncatholic_church, hospital_clinic, health_center, health_point,
         electricity, usd_deflator, defl_commit_muni_year, defl_commit_total,
         trmm_min, trmm_max, trmm_mean, trmm_std, trmm_med, c3km2, c4km2
         )


## 9d) CREATE NEW VARS & RECODE OTHER VARS

co_sum %<>%
  select(codmuni, year, department, starts_with("c_"), starts_with("s_"), 
         everything()) %>%
  arrange(codmuni, year)


## AVERAGE MUNI VOTE TURNOUT - SEN & CAM ELECS
co_sum$pop_avg <- ""
co_sum %<>%
  group_by(codmuni) %>% 
  mutate(
    pop_avg = if_else(year == 2002 | year == 2006 | # pop avg in voting yrs
                        year == 2010 | year == 2014,
                      round(mean(demo_3), digits = 0),
                      as.numeric(pop_avg))
      ) %>% 
  fill(pop_avg, .direction = c("up") # fill non election years
      ) %>% 
  mutate(
    c_turnout = c_mvote_avg/pop_avg, # cam muni turnout
    s_turnout = s_mvote_avg/pop_avg) # sen muni turnout


## CROPSHARE DIFFERENCE VAR (2012-2001 value, i.e. not trend)
cropsha_cs <- co_sum %>%
  select(codmuni, year, cropsha, geo_3) %>%
  filter(year == 2001 | year == 2012) %>% 
  arrange(codmuni, year) %>%
  mutate(
    cropsha_km2 = cropsha/100, # cropshare in km^2
    cropsha_pct = cropsha_km2/geo_3 # cropshare as % of total muni area (km^2)
  ) %>%
  group_by(codmuni) %>% 
  mutate(
    cropsha_chg = cropsha_pct - lag(cropsha_pct) # pct change (2012-2001)
  ) %>% 
  spread(year, cropsha_pct, sep = "_") %>%
  rename(cropsha_pct01 = year_2001, cropsha_pct12 = year_2012)


# create cross section of the panel
cropsha_cs1 <- cropsha_cs %>%
  group_by(codmuni) %>% 
  summarise_at(c("cropsha_chg", "cropsha_pct01", "cropsha_pct12"),
               max, na.rm = TRUE)


# "pres_xxx" & "pol_xx" VAR FIXES & SUMMARIZING
pres_sum <- co_sum %>%
  select(codmuni, year,
         starts_with("pres_"),
         num_range("pol_", c(9, 11:18))
  ) %>%
  filter(year %in% c(1998, 2002, 2006, 2010, 2014)) %>%
  arrange(codmuni, year) %>%
  mutate(
    pol_12 = if_else(year %in% c(2002, 2006), # ideology n/a for these years
                     NA_real_,
                     pol_12),
    pol_13 = if_else(year %in% c(2006),
                     NA_real_,
                     pol_13),
    pol_14 = if_else(year %in% c(1998),
                     NA_real_,
                     pol_14),
    pol_15 = if_else(year %in% c(2014),
                     NA_real_,
                     pol_15),
    pol_16 = if_else(year %in% c(1998, 2002, 2006),
                     NA_real_,
                     pol_16),
    pol_17 = if_else(year %in% c(1998),
                     NA_real_,
                     pol_17)
  ) %>%
  group_by(codmuni) %>%
  summarise_all(mean, na.rm = TRUE) # collapse vars on

pres_sum %<>% select(-year)

  

## 9e) SUMMARIZE VARS ----

# mean - summarize values 
avg_sum <- co_sum %>%
  ungroup() %>% 
  select(codmuni,
         eco_40:eco_42, vio_10:vio_16,
         infant_mortality, ethnic_minorities:religious) %>%
  group_by(codmuni) %>%
  summarise_all(mean, na.rm = TRUE)

# mean & std dev - summarize values
sd_avg_sum <- co_sum %>%
  ungroup() %>% 
  select(codmuni,
         SPI12m,
         eco_46:eco_51, eco_53:eco_55, eco_61:eco_80,
         vio_26:vio_28, vio_31:vio_36,
         demo_1:demo_4
         ) %>%
  group_by(codmuni) %>%
  summarise_all(c("mean", "sd"), na.rm = TRUE)

# "total" - summarize values
total_sum <- co_sum %>% 
  ungroup() %>% 
  select(codmuni,
         vio_20:vio_25, vio_29, vio_37:vio_45, vio_4:vio_9,
         vio_48:vio_53, vio_55:vio_59, vio_61:vio_65, vio_68:vio_87,
         attack_victims:war_civilian_victims
         ) %>%
  group_by(codmuni) %>% 
  summarise_all(sum, na.rm = TRUE)

# remove the summarized values above from the aggregate 'co_sum' data frame
co_sum1 <- co_sum %>%
  select(
    -one_of(names(avg_sum)),
    -one_of(names(total_sum)),
    -num_range("vio_", c(26:28, 31:36)),
    -num_range("eco_", c(46:51, 53:55, 61:80)),
    -cropsha,
    -num_range("demo_", c(1:4, 8:9)),
    -road5, -road6,
    -starts_with("pres_"),
    -starts_with("pol_")
  )

# replace NAs w/ -9.99e+20 so 'max' summary function "works"
co_sum1[is.na(co_sum1)] <- -9.99e+20

co_sum2 <- co_sum1 %>%
  arrange(codmuni, year) %>% 
  group_by(codmuni) %>%
  summarise_all(max, na.rm = TRUE)

# replace -9.99e+20 back to NA
co_sum3 <- na_if(co_sum2, -9.99e+20)

# values calculated using "max" - use for records
values_max <- as.data.frame(names(co_sum2))



## 9f) MERGE SUMMARIZED DFS INTO ONE DF
sd_avg_sum %<>% arrange(codmuni)
avg_sum %<>% arrange(codmuni)
co_sum3 %<>% arrange(codmuni)
total_sum %<>% arrange(codmuni)
cropsha_cs1 %<>% arrange(codmuni)
pres_sum %<>% arrange(codmuni) 

co_combined <- bind_cols(co_sum3, sd_avg_sum, avg_sum, total_sum,
                         cropsha_cs1, pres_sum)

co_combined %<>%
  select(-num_range("codmuni", c(1:5)),
         -year)




# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-= # 
####    X. MERGE CROP CS DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-= #
co_combined %<>% left_join(co_combined, crops_cs, by = 'codmuni')




# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=- # 
####    XI. WRITE CROSS-SECTIONAL DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=- #
setwd(dir.outdata)
save(co_combined, file = "eac_vio_muni_cross-section.RData")
write_csv(co_combined, "eac_vio_muni_cross-section.csv")




## 5) TIME UNDER MUNI CONTROL
# create the var using Ale's Stata code


## HYDE DATA (c3 & c4 crops)
# we have the trend vars for this
# 'c3km2' and 'c4km2' --> this is HYDE data (1992-1998 for both c3 & c4)
# summary(dmyr_pres$c3km2)
# summary(dmyr_pres$c4km2)
# dmyr_pres %>% group_by(codmuni, year) %>% select(codmuni, year, c3km2, c4km2) %>% arrange(codmuni, year) %>% summary()



##write_csv for quick '20170817_analysis' w/ Mike and Alejandro
# setwd(dir.outdata)
# write_csv(dmyr_pres, "panel_w_vio_and_croptrend.csv", na = ".") 
