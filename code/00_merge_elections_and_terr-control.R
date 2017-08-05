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
# 
#  OUTLINE
#  I.   
#  II.  
#  III. 
#
# 

#-----------------------------------------------------------------------------#

# =-=-=-=-=-=-=-=-=-=- # 
####    I. SETUP    #### 
# =-=-=-=-=-=-=-=-=-=- #
rm(list=ls())

library(tidyverse)
library(readstata13)

## 1a) SET FILE PATHS ####
# Set working directory to where you saved project directory (folder)
# 'dir.home' the only file path you need to change!
dir.home <- file.path("/Users/wtmatthias/Google Drive/Mike RA")

dir.panel <- file.path(dir.home,
                      "elections_aid_cropshare/data_before-201705/01_cleaning")
dir.rawdata <- file.path(dir.home,
                         "elections_aid_cropshare/data_201708/")
dir.analysis <- file.path(dir.home,
                          "elections_aid_cropshare/data_201708")

## 1b) LOAD DATA ####
setwd(dir.panel)

# Colombia *panel* data 
orig_panel <- read.dta13("muni_19982014_v06_TECHNICALLY_CORRECT.dta")

setwd(dir.rawdata)
codmuni <- read_csv("muni_codes.csv")

names <- orig_panel %>% select(codmuni, department, municipality) %>%
         distinct(department, municipality)

# Election Data


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

























