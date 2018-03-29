#-----------------------------------------------------------------------------#
#  File Name:    01_merge_ag_aid.R
#  Author:       Billy Matthias 
#  Email:        bmatthias88@gmail.com
#  Purpose:      (1) merge in World Bank agricultural aid
#  Last Update:  2017-Sept-14
#  Data Output:    
#-----------------------------------------------------------------------------#


# =-=-=-=-=-=-=-=-=-=- # 
####    I. SETUP    #### 
# =-=-=-=-=-=-=-=-=-=- #
rm(list=ls())

library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)

## 1a) SET FILE PATHS ----
# Set working directory to where you saved project directory (folder)
# 'dir.home' the only file path you need to change!
dir.home <- file.path("/Users/wtmatthias/Google Drive/Mike RA")

dir.rawdata <- file.path(dir.home,
                        "elections_aid_cropshare/data_201708/00_rawdata")
dir.agaid <- file.path(dir.home,
                    "elections_aid_cropshare/data_201708/00_rawdata/wb_ag_aid")
dir.outdata <- file.path(dir.home,
                         "elections_aid_cropshare/data_201708/01_outputdata")

# co_combined (from "00_wrangling_xxxxxx.R")
#     - i.e. muni cross sectional w/ pres, senate, camara elections &
#     - violence/conflict vars, + other CEDE and control vars
load(file = file.path(dir.outdata, "eac_vio_muni_cross-section.RData"))

agaid <- read_csv(file = file.path(dir.agaid, "colombia_locs1-3_merged.csv"))


# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    II. MERGE AGAID    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #
agaid %<>%
  arrange(codmuni) %>%
  select(OBJECTID, codmuni, locs3_project_n, locs1_project_n)

aid_merged <- left_join(co_combined, agaid, by = "codmuni")



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    III. CREATE AID DUMMY    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# more than 1 aid project per location in prec. code 1 & 3?
# aid_merged %>% select(locs3_project_n) %>% count(locs3_project_n)
# aid_merged %>% select(locs1_project_n) %>% count(locs1_project_n)

## CREATE VAR - is aid targeted to the municipality (prec. code 1, 2, or 3)
# Note: there are no prec.code==2 for ag aid in WB Colombia proj
aid_merged %<>%
  mutate(muni_locd = if_else(locs3_project_n >= 1 | locs1_project_n >=1,
                             1,
                             0
                             )
         )

# check that var is tidy/consistent
# anyNA(aid_merged$muni_locd)
# aid_merged %>% count(muni_locd)



# =-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####    IV. EXPORT DATA    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-= #
save(aid_merged, file = file.path(
                  dir.outdata,
                  "eac_vio_muni_cross-section.RData"
                  )
     )

write_csv(aid_merged, path = file.path(
                        dir.outdata,
                        "eac_vio_muni_cross-section.csv"
                        )
          )

## .csv for STATA (NAs are ".")
# change NaN to NA
aid_merged2 <- aid_merged %>% map_df(function(x){ifelse(is.nan(x), NA, x)})
write_csv(aid_merged2,
          path = file.path(dir.outdata, "eac_vio_muni_cross-section_STATA.csv"),
          na = "."
)