#-----------------------------------------------------------------------------#
#  File Name:    00_merge_elections_and_terr-control
#  Author:       Billy Matthias 
#  Email:        bmatthias88@gmail.com
#  Purpose:      (1) merge in more election data
#                    - pres. elections 1998-2006
#                    - senate & camara de rep. 2002-2006 
#                (2) merge terr. control measures
#                (3) armed actor presence vars
#  Last Update:  2017-Aug-04
#  Data Used:    (1) "muni_19982014_v06_TECHNICALLY_CORRECT.dta"
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

## 1a) SET FILE PATHS ####
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

## 1b) LOAD DATA ####
setwd(dir.panel)

# 1b.1) colombia *panel* data ----
orig_panel <- read.dta13("muni_19982014_v06_TECHNICALLY_CORRECT.dta")

setwd(dir.rawdata)
codmuni <- read_csv("muni_codes.csv")

names <- orig_panel %>% select(codmuni, department, municipality) %>%
         distinct(department, municipality)


# 1b.2 TEST .dta2.csv name change & write ----
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


# 1b.3) election data - .dta2.csv ----

#'pres' = presidential election by muni
# pres <- read.dta13("cattle_and_election_ideology/4. municipal data.dta")

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
setwd(dir.elec)

# create new file name w/ .csv as extension
rewrite_dta <- list.files(dir.elec, pattern = "*.dta") # lists names from dir
# rewrite_dta_name <- as.data.frame(sapply(rewrite_dta,
#                                         gsub, pattern = ".dta",
#                                         replacement = ".csv",
#                                         USE.NAMES = FALSE),
#                                  stringsAsFactors = FALSE)
#                           # .csv for filename + convert to data.frame
# rewrite_dta_name <- rewrite_dta_name %>% select(csvfile = contains("sapply"))

# create a list of DFs
elec <- ls(envir = globalenv(),
           pattern = "_[[:digit:]]+$") # finds list of the election df obj
df_list <- mget(elec, env = globalenv())

system.time(
  lapply(1:length(df_list), function(x) write_csv(df_list[[x]],
                                        path = paste(gsub(".dta",
                                                           ".csv",
                                                           rewrite_dta[[x]])),
                                        col_names = TRUE)
      )
  )

# system.time(
#   d_ply(1:length(df_list), function(x) write_csv(df_list[[x]],
#                                                   path = paste(gsub(".dta",
#                                                                     ".csv",
#                                                                     rewrite_dta[[x]])),
#                                                   col_names = TRUE)
#   )
# )


# remove/delete .dta and TEST objs from environment
rm(list = elec)
rm(list = c("df1", "df2", "elec", "rewrite_dta", "tmp",
            "rewrite_dta_name", "rewrite_test"))



# 1b.4 election data - write_csv ----
#    pol_1: ties
#    pol_2: Centro
#    pol_3: Conservador
#    pol_4: Tercera Via
#    pol_5: Izquierda
#    pol_6: Liberal
#    pol_7: Minority parties
#    pol_8: Uribismo
#    pol_9: electoral competence? ask Ale what does this mean?
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

# require(plyr)
elec_camara <- ldply(elec_dflist[1:6])
elec_camara1 <- as_tibble(elec_camara)
elec_senado <- ldply(elec_dflist[7:12])
elec_senado1 <- as_tibble(elec_senado)
rm(list = c("elec_dflist", "camara02", "elec_camara", "elec_senado"))



# =-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    II. MERGE DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=- #

str(orig_panel$department)
str(orig_panel$municipality)
str(codmuni$municipality)
str(codmuni$department)
str(orig_panel)


## 2a) MERGE MUNI CODES ####

# make sure there are no missing values
empty_dept <- orig_panel %>% filter(department == "")
empty_muni <- orig_panel %>% filter(municipality == "")

# filter out missing values
orig_panel <- orig_panel %>% filter(department != "" | municipality != "")

# dmyr = 'dept' + 'muni' + 'year' (panel)
dmyr <- left_join(orig_panel, codmuni, by = c("department", "municipality"))

# check merge + only keep codmuni.y
dmyr <- dmyr %>% select(codmuni.y, everything(), -codmuni.x) %>%
                     rename(codmuni = codmuni.y)

# get rid of rows where codmuni == NA
nrow(distinct(dmyr, codmuni))
dmyr <- dmyr %>% drop_na(codmuni)


## 2b) REMOVE DUPLICATES ####
nrow(dmyr)
nrow(distinct(dmyr, codmuni, year))

# freq table of dups
dups <- dmyr %>% select(codmuni, year) %>% arrange(codmuni, year)
dups_n_occur <- data.frame(table(paste0(dups$codmuni, dups$year)))
dups_n_occur <- dups_n_occur %>% arrange(desc(Freq))

dmyr <- dmyr %>%
  distinct(codmuni, year, .keep_all = TRUE)

rm(dups, empty_dept, empty_muni, names)


## 2c) MERGE ELECTION DATA ####



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- # 
####    III. WRANGLE/CLEAN/PREP CONSISTENT    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #



















names(elec_dflist$senado02)






