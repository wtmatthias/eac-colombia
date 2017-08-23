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



# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    IV. CAMARA DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #

## SAVE BINDED RAW ELECTION DATA (i.e before changes)
write_csv(elec_sen1, file.path(dir.elec, "senado_2002-2014.csv"))
write_csv(elec_cam1, file.path(dir.elec, "camara_2002-2014.csv"))

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

dpv <- elec_cam6 %>%
                filter(codmuni == 99) %>% 
                group_by(year, coddept, codpartido) %>% 
                mutate(cam_dpvote = sum(votos))

elec_cam6$cam_dpv <- ""
elec_cam6 <- elec_cam6 %>%
                group_by(year, coddept, codpartido) %>% 
                mutate(cam_dpvote = if_else(codmuni == 99,
                                         sum(votos),
                                         as.numeric(cam_dpvote)))

# get rid of missing values in codmuni (these obs have no use for totals vars)
# elec_cam6 %>% ungroup() %>% mutate(codmunina = is.na(codmuni)) %>%
              # count(codmunina) %>% View()
elec_cam7 <- elec_cam6 %>% filter(!is.na(codmuni))


## CREATE "DEPT - VOTE TOTAL" VAR = 'dvote' & "MUNI-VOTE TOTAL" = 'mvote'
elec_cam7$cam_dvote <- ""
elec_cam7$cam_mvote <- ""
elec_cam7 <- elec_cam7 %>%
                group_by(year, coddept) %>% 
                mutate(cam_dvote = if_else(codmuni != 99,
                                          sum(votos),
                                          as.numeric(cam_dvote))) %>%
                group_by(codmuni, add = TRUE) %>%
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
urib02 <- c(154, 73, 195, 41, 165, 730, 751, 754, 759, 762, 764, 765, 778, 791,
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
cam <- elec_cam9 %>%
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
                mutate(cam_conserv = if_else(codpartido %in% conserv,
                                              1,
                                              0))
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
                   cam_min == 1 ~ "OTHER",
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
          mutate(sumseat = if_else(codmuni == 99 & !is.na(curules),
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
          mutate(dpseat = if_else(codmuni == 99 & !is.na(curules),
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
          mutate(dstot = cummax(sumseat))

# proportion of seats won by party
# 'cam_seatpro' = seat proportion
cam1 <- cam1 %>%
          arrange(year, coddept, codmuni) %>%
          group_by(year, coddept, codpartido) %>%
          mutate(cam_seatpro = cummax(dpseat)/dstot)

cam1 %>% arrange(year, coddept, codmuni, desc(cam_seatpro)) %>% View()

# RANK of proportion of seats won by party
# 'cam_seatproR'
cam1 <- cam1 %>%
          arrange(year, coddept, codmuni, desc(cam_seatpro)) %>%
          group_by(year, coddept) %>% 
          mutate(cam_seatproR = dense_rank(desc(cam_seatpro)))



## 4e) CREATE DEPT & MUNI "IDEOLOGY VOTE TOTAL" VAR ----

# check for missing values in codpartido
# elec_cam7 %>% filter(is.na(codpartido)) %>%
#               group_by(nombre) %>%
#               count(is.na(codpartido)) %>% View()

# missing ideology as NA
cam2 <- cam1
cam1$cam_ideo[cam1$cam_ideo == ""] <- NA 

## MUNI IDEOLOGY VOTE TOTAL
# "cam_mivote'
cam1$cam_mivote <- ""
cam2 <- cam1 %>%
  group_by(year, coddept, codmuni, cam_ideo) %>% 
  mutate(cam_mivote = if_else(!is.na(cam_ideo),
                              sum(votos),
                              as.numeric(cam_mivote)))
# check random muni
# cam2 %>% filter(year == 2002 & codmuni == 23419) %>% arrange(cam_ideo) %>% View()


## DEPT IDEOLOGY VOTE TOTAL
# "cam_divote'
cam2$cam_divote <- ""
cam2 <- cam2 %>%
  group_by(year, coddept, cam_ideo) %>% 
  mutate(cam_divote = if_else(codmuni == 99,
                              sum(votos),
                              as.numeric(cam_divote)))



## 4f) PROPORTION OF SEATS BY IDEOLOGY ----

# create var to count # of seats won by ideology @ dept
# 'diseat' = dept-ideology seat
cam2$diseat <- ""
cam3 <- cam2 %>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(diseat = if_else(codmuni == 99 & !is.na(curules),
                          cumsum(curules),
                          as.integer(diseat)))

# proportion of seats won by ideology
# 'cam_seatpro_i' = seat proportion_ideology
cam3 <- cam3 %>%
  arrange(year, coddept, cam_ideo, desc(diseat)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(cam_seatpro_i = cummax(diseat)/dstot)

# cam3 %>% arrange(year, coddept, codmuni, desc(cam_seatpro_i)) %>% View()

# RANK of proportion of seats won by party
# 'cam_seatproR'
cam3 <- cam3 %>%
  arrange(year, coddept, codmuni, desc(cam_seatpro_i)) %>%
  group_by(year, coddept) %>% 
  mutate(cam_seatpro_iR = dense_rank(desc(cam_seatpro_i)))



## 4g) IDEOLOGY TOTAL @ MUNI ----
# 'cam_mivote'
cam3$cam_mivote <- ""
cam4 <- cam3 %>%
  group_by(year, coddept, codmuni, cam_ideo) %>%
  mutate(cam_mivote = if_else(!is.na(cam_ideo),
                              sum(votos),
                              as.numeric(cam_mivote)))



## 4h) MUNI PARTY & IDEOLOGY VOTE PROPORTION & RANK ----

# party proportion
cam4 <- cam4 %>%
          group_by(year, coddept, codmuni) %>%
          mutate(cam_mppro = cam_mpvote/cam_mvote)
# ideology proportion
cam4 <- cam4 %>%
          group_by(year, coddept, codmuni) %>%
          mutate(cam_mipro = cam_mivote/cam_mvote)

# muni party proportion
cam4 <- cam4 %>%
          arrange(year, coddept, codmuni, desc(cam_mppro)) %>%
          group_by(year, coddept, codmuni) %>% 
          mutate(cam_mppro_r = dense_rank(desc(cam_mppro)))

# muni ideology proportion
cam4 <- cam4 %>%
          arrange(year, coddept, codmuni, desc(cam_mipro)) %>%
          group_by(year, coddept, codmuni) %>% 
          mutate(cam_mipro_r = dense_rank(desc(cam_mipro)))



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
    mutate(cam_pidseat =
             case_when(
               (curules >= 1 & year!=2014) & cam_ideo == "URIBISMO" ~ 1,
               (curules >= 1 & year==2014) & cam_ideo == "TERCERA VIA" ~ 1,
               curules >= 1 ~ 0))

# cam_m1 %>% filter(year==2014) %>% arrange(year, coddept, codmuni) %>% View()

# fill the values for each ideology that has a seat
cam_m2 <- cam_m1 %>%
    group_by(year, cam_ideo, coddept) %>%
    arrange(year, desc(cam_ideo), coddept, desc(cam_m_pidseat)) %>%
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
            mutate(cam_pidseat2 = if_else(is.na(cam_pidseat),
                                           0,
                                           cam_pidseat))

## (1.3) MATCH ideology + highest proportion of seats won
# pres. ideology have seats AND does it have the highest proportion of seats?
cam_m4 <- cam_m4 %>%
            mutate(cam_pidseatR1 = if_else(
                                    cam_seatpro_iR == 1 & cam_pidseat == 1,
                                    1,
                                    0))
# alternate coding: same, but seat proportion is Rank 1 OR Rank 2
cam_m4 <- cam_m4 %>%
            mutate(cam_pidseatR1R2 = if_else(
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
    mutate(cam_pidseat_muniR1R2 = if_else(
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
cam_m6 <- bog02 %>%
            filter(!(year == 2002 & coddept == 11))

bog02$sumseat <- ""
bog02 <- bog02 %>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept) %>%
  mutate(sumseat = if_else(codmuni == 11001 & !is.na(curules),
                           cumsum(curules),
                           as.integer(sumseat)))

bog02$dpseat <- ""
bog02 <- bog02 %>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, codpartido) %>%
  mutate(dpseat = if_else(codmuni == 11001 & !is.na(curules),
                          cumsum(curules),
                          as.integer(dpseat)))

# 'dstot' = dept seat total
bog02$dstot <- ""
bog02 <- bog02 %>%
  arrange(year, coddept, desc(sumseat)) %>% 
  group_by(year, coddept) %>%
  mutate(dstot = cummax(sumseat))

# 'cam_seatpro' = seat proportion
bog02 <- bog02 %>%
  arrange(year, coddept, codmuni) %>%
  group_by(year, coddept, codpartido) %>%
  mutate(cam_seatpro = cummax(dpseat)/dstot)

# 'cam_seatproR'
bog02 <- bog02 %>%
  arrange(year, coddept, codmuni, desc(cam_seatpro)) %>%
  group_by(year, coddept) %>% 
  mutate(cam_seatproR = dense_rank(desc(cam_seatpro)))

bog02$cam_divote <- ""
bog02 <- bog02 %>%
  group_by(year, coddept, cam_ideo) %>% 
  mutate(cam_divote = if_else(codmuni == 11001,
                              sum(votos),
                              as.numeric(cam_divote)))

# 'diseat' = dept-ideology seat
bog02$diseat <- ""
bog02 <- bog02 %>%
  arrange(year, coddept, desc(curules)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(diseat = if_else(codmuni == 11001 & !is.na(curules),
                          cumsum(curules),
                          as.integer(diseat)))

# 'cam_seatpro_i' = seat proportion_ideology
bog02 <- bog02 %>%
  arrange(year, coddept, cam_ideo, desc(diseat)) %>%
  group_by(year, coddept, cam_ideo) %>%
  mutate(cam_seatpro_i = cummax(diseat)/dstot)

# 'cam_seatproR'
bog02 <- bog02 %>%
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

# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    VI. SENATE DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #











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

## PRES MATCH VAR

## CAM MATCH VAR

## SEN MATCH VAR


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



##write_csv for quick '20170817_analysis' w/ Mike and Alejandro
## setwd(dir.outdata) ----
# setwd(dir.outdata)
# write_csv(dmyr_pres, "panel_w_vio_and_croptrend.csv", na = ".")
