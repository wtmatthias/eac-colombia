rm(list=ls())
# 
library(plyr)
library(tidyverse)
library(readstata13)
library(stringr)
library(magrittr)
library(haven)
library(interplot)


##  SET FILE PATHS  ####
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

## LOAD DATA 
setwd(dir.outdata)
agaid <- read.dta13(file = "00_colombia-cs_with_ag_aid_201708.dta", convert.factors = FALSE)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#####  ROBUST STANDARD ERRORS  #####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
library(RCurl)

# import the function
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)


# =-=-=-=-=-=-=-=-=-=-=-=-=- #
#####  MODELS & FIGURES  #####
# =-=-=-=-=-=-=-=-=-=-=-=-=- #

## TARGETED FOR AID: 201705 analysis
# logit.aid <- glm(muni_locd ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
#                    road_zero + illegal_crops + mean_turnout + mean_mov + mean_muni_dept_match +
#                    local_parliament_match_mean, family = binomial(link='logit'), data = agaid)
# 


## TARGETED FOR AID W/ NEW ELECTIONS ----


# =-=-=-=-=-=-=-=-=-=-=-=-= #
####  CROPSHARE % CHG    ####
# =-=-=-=-=-=-=-=-=-=-=-=-= #
ols.cropsha <- lm(trendcrop ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                    road_zero + illegal_crops + muni_locd + mean_turnout + mean_mov + mean_muni_dept_match +
                    local_parliament_match_mean, data = agaid)
summary(ols.cropsha)
summary(ols.cropsha, robust = T)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
####  CROPSHARE TREND VARS   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

## STATE GUERILLA CONTROL ----

# TRENDCROP
# trendcrop & state-guerilla control (vio_26mean)
trendcrop.control <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean,
                    data = agaid)
# summary(trendcrop.control)
summary(trendcrop.control, robust = T)

# interaction -- trendcrop & state-guerilla control
trendcrop.control.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean + muni_locd*vio_26mean,
                    data = agaid)
# summary(trendcrop.control.i)
summary(trendcrop.control.i, robust = T)

interplot(m = trendcrop.control.i, var1 = "muni_locd", var2 = "vio_26mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. State-Guerilla Control") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)


# trendcrop & state-guerilla control (vio_36mean)
trendcrop.vio36 <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_36mean,
                    data = agaid)
# summary(trendcrop.vio36)
summary(trendcrop.vio36, robust = T)

# interaction -- trendcrop & state-guerilla control
trendcrop.vio36.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_36mean + muni_locd*vio_36mean,
                    data = agaid)
# summary(ols.trendcrop.i)
summary(trendcrop.vio36.i, robust = T)

interplot(m = trendcrop.vio36.i, var1 = "muni_locd", var2 = "vio_36mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. State-Guerilla Control") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)


# TRENDCROPVEG
# trendcropveg & state-guerilla control (vio_36mean)
trendcropveg.vio36 <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_36mean,
                    data = agaid)
# summary(trendcropveg.vio36)
summary(trendcropveg.vio36, robust = T)

# interaction -- trendcrop & state-guerilla control
trendcropveg.vio36.i <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_36mean + muni_locd*vio_36mean,
                    data = agaid)
# summary(trendcropveg.vio36.i)
summary(trendcropveg.vio36.i, robust = T)

interplot(m = trendcropveg.vio36.i, var1 = "muni_locd", var2 = "vio_36mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. State-Guerilla Control") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)


# trendcropveg & state-guerilla control (vio_26mean)
trendcropveg.vio26 <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean,
                    data = agaid)
summary(trendcropveg.vio26)
summary(trendcropveg.vio26, robust = T)

# interaction -- trendcropveg & state-guerilla control
trendcropveg.vio26.i <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean + muni_locd*vio_26mean,
                    data = agaid)
# summary(trendcropveg.vio26.i)
summary(trendcropveg.vio26.i, robust = T)

interplot(m = trendcropveg.vio26.i, var1 = "muni_locd", var2 = "vio_26mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. State-Guerilla Control") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)



# TRENDGRASS
# trendgrass & state-guerilla control
trendgrass.vio26 <- lm(trendgrass ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean,
                    data = agaid)
# summary(trendgrass.vio26)
summary(trendgrass.vio26, robust = T)

# interaction -- trendgrass & state-guerilla control
trendgrass.vio26.i <- lm(trendgrass ~ spistd + geo_3mean + upstream1 + altitude1 +
                      road_zero + muni_locd + vio_26mean + muni_locd*vio_26mean,
                    data = agaid)
# summary(trendgrass.vio26.i)
summary(trendgrass.vio26.i, robust = T)




## COCA PRESENCE AVG ----

# TRENDCROPVEG
# trendcropveg & illegal crop presence avg.
trendcropveg.coca <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                         road_zero + muni_locd + vio_13mean,
                       data = agaid)
# summary(trendcropveg.coca)
summary(trendcropveg.coca, robust = T)

# interaction -- trendcropveg & illegal crop presence avg.
trendcropveg.coca.i <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                           road_zero + muni_locd + muni_locd*vio_13mean,
                         data = agaid)
# summary(trendcropveg.coca.i)
summary(trendcropveg.coca.i, robust = T)

# Marginal FX Plot
interplot(m = trendcropveg.coca.i, var1 = "muni_locd", var2 = "vio_13mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)
# setwd(dir.outdata) # wd where figures saved
# ggsave("interact_aid_illegalcropavg.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# trendcropveg & illegal crop dummy
trendcropveg.coca.d <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                         road_zero + muni_locd + illegal_crops,
                       data = agaid)
# summary(trendcropveg.coca.d)
summary(trendcropveg.coca.d, robust = T)

# interaction -- trendcropveg & illegal crop dummy.
trendcropveg.coca.d.i <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                           road_zero + muni_locd + muni_locd*illegal_crops,
                         data = agaid)
# summary(trendcropveg.coca.d.i)
summary(trendcropveg.coca.d.i, robust = T)

# Marginal FX Plot
interplot(m = trendcropveg.coca.d.i, var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)
# setwd(dir.outdata) # wd where figures saved
# ggsave("interact_aid_illegalcropavg.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)




# TRENDCROPVEG + COCA PRESENCE + FARC PRESENCE
# trendcropveg & illegal crop presence avg & FARC PRESENCE.
trendcropveg.coca.farc <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                          road_zero + muni_locd + vio_13mean + vio_15mean,
                        data = agaid)
# summary(trendcropveg.coca.farc)
summary(trendcropveg.coca.farc, robust = T)

# interaction -- trendcropveg & illegal crop presence avg.
trendcropveg.coca.farc.i <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + 
                                 altitude1 + road_zero + vio_13mean + 
                                 vio_15mean + muni_locd + muni_locd*vio_13mean,
                            data = agaid)
# summary(trendcropveg.coca.farc.i)
summary(trendcropveg.coca.farc.i, robust = T)

# Marginal FX Plot
interplot(m = trendcropveg.coca.farc.i, var1 = "muni_locd", var2 = "vio_13mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)


## TRENDCROP
## trend crop & illegal crop presence avg
trendcrop.coca <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                         road_zero + muni_locd + vio_13mean,
                       data = agaid)
# summary(trendcrop.coca)
summary(trendcrop.coca, robust = T)

# interaction -- trend crop & illegal crop avg
trendcrop.coca.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                         road_zero + vio_13mean + muni_locd +
                         muni_locd*vio_13mean,
                        data = agaid)
# summary(trendcrop.coca.i)
summary(trendcrop.coca.i, robust = T)

# Marginal FX Plot
interplot(m = trendcrop.coca.i, var1 = "muni_locd", var2 = "vio_13mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)



## trend crop & illegal crop dummy
trendcrop.coca.d <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                         road_zero + muni_locd + illegal_crops,
                       data = agaid)
# summary(trendcrop.coca.d)
summary(trendcrop.coca.d, robust = T)

# interaction -- ## trend crop & illegal crop dummy
trendcrop.coca.d.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                           road_zero + muni_locd + illegal_crops + muni_locd*illegal_crops,
                         data = agaid)
# summary(trendcrop.coca.d.i)
summary(trendcrop.coca.d.i, robust = T)

# Marginal FX Plot
interplot(m = trendcrop.coca.d.i, var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)
# setwd(dir.outdata) # wd where figures saved
# ggsave("interact_aid_illegalcropavg.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)



# CROP + COCA PRESENCE AVG. & CAMARA ELECTIONS
## trend crop & illegal crop avg & election vars
trendcrop.coca.cam <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                           road_zero + muni_locd + vio_13mean + c_match_munidept +
                            c_match_munideptpres + c_match_presdept,
                         data = agaid)
# summary(trendcrop.coca.cam)
summary(trendcrop.coca.cam, robust = T)

# interaction -- trend crop & illegal crop avg & election vars
trendcrop.coca.cam.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 + altitude1 +
                             road_zero + muni_locd + vio_13mean + 
                             muni_locd*vio_13mean + c_match_munidept +
                             c_match_munideptpres + c_match_presdept,
                           data = agaid)
# summary(trendcrop.coca.cam.i)
summary(trendcrop.coca.cam.i, robust = T)

# Marginal FX Plot
interplot(m = trendcrop.coca.cam.i, var1 = "muni_locd", var2 = "vio_13mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)



# CROPVEG + COCA PRESENCE AVG. & CAMARA ELECTIONS
## trend cropveg & illegal crop avg & election vars
trendcropveg.coca.cam <- lm(trendcropveg ~ spistd + geo_3mean + upstream1 + altitude1 +
                           road_zero + muni_locd + vio_13mean + c_match_munidept +
                            c_match_munideptpres + c_match_presdept,
                         data = agaid)
# summary(trendcropveg.coca.cam)
summary(trendcropveg.coca.cam, robust = T)

# interaction -- trend cropveg & illegal crop avg & election vars
trendcropveg.coca.cam.i <- lm(trendcrop ~ spistd + geo_3mean + upstream1 +
                                altitude1 + road_zero + muni_locd + vio_13mean +
                              muni_locd*vio_13mean + c_match_munidept +
                                c_match_munideptpres + c_match_presdept,
                           data = agaid)
# summary(trendcropveg.coca.cam.i)
summary(trendcropveg.coca.cam.i, robust = T)

# Marginal FX Plot
interplot(m = trendcropveg.coca.cam.i, var1 = "muni_locd", var2 = "vio_13mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Avg. Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)









#vio_26mean
#vio_47: incid. of conflict
# vio_15: FARC control avg.









## OLS W/ INTERACTIONS: Aid * Illegal Crops
ols.cropsha.i_aidcrops <- lm(cropsha_pct_chg ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                               road_zero + illegal_crops + muni_locd + muni_locd*illegal_crops + mean_turnout + mean_mov + mean_muni_dept_match +
                               local_parliament_match_mean, data = aid)
summary(ols.cropsha.i_aidcrops)
summary(ols.cropsha.i_aidcrops, robust = T)

# Marginal FX Plot
interplot(m = ols.cropsha.i_aidcrops, var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Estimated Coefficient for Aid") + xlab("Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)

getwd()
setwd(dir.figures) # wd where figures saved
ggsave("interact_aid_illegalcrops.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)



## OLS W/ INTERACTIONS: Aid * Mayor/Governor Match Proportion
ols.cropsha.i_aid_munidept <- lm(cropsha_pct_chg ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                                   road_zero + illegal_crops + muni_locd + mean_turnout + mean_mov + mean_muni_dept_match +
                                   muni_locd*mean_muni_dept_match + local_parliament_match_mean, data = aid)
summary(ols.cropsha.i_aid_munidept)
ols.cropsha.i_aid_munidept.rob <- summary(ols.cropsha.i_aid_munidept, robust = T)

# Marginal FX Plot
interplot(m = ols.cropsha.i_aid_munidept , var1 = "muni_locd", var2 = "mean_muni_dept_match") +
  ylab("Estimated Coefficient for Aid") + xlab("Proportion of Elections (2010, 2014) - Mayor & Governor Party Match") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) 

setwd(dir.figures) # wd where figures saved
ggsave("interact_aid_mayor-gov-match.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)



## OLS W/ INTERACTIONS: Aid * Mayor/Parliament Match Proportion
ols.cropsha.i_aid_munipar <- lm(cropsha_pct_chg ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                                  road_zero + illegal_crops + muni_locd + mean_turnout + mean_mov + mean_muni_dept_match +
                                  local_parliament_match_mean + muni_locd*mean_muni_dept_match + muni_locd*local_parliament_match_mean + muni_locd*illegal_crops , data = aid)
summary(ols.cropsha.i_aid_munipar)
ols.cropsha.i_aid_munipar.rob <- summary(ols.cropsha.i_aid_munipar, robust = T)
ols.cropsha.i_aid_munipar.rob

interplot(m = ols.cropsha.i_aid_munipar , var1 = "muni_locd", var2 = "local_parliament_match_mean") +
  ylab("Estimated Coefficient for Aid") + xlab("Mayor & Dominant Parliment Party Match - 2010 Election") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) 

setwd(dir.figures) # wd where figures saved
ggsave("interact_aid_mayor-par-match.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)



## OLS W/ ALL INTERACTIONS INCLUDED IN MODEL
ols.cropsha.interact <- lm(cropsha_pct_chg ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                             road_zero + illegal_crops + muni_locd + mean_turnout + mean_mov + mean_muni_dept_match +
                             local_parliament_match_mean + muni_locd*mean_muni_dept_match + muni_locd*illegal_crops + muni_locd*local_parliament_match_mean, data = aid)
summary(ols.cropsha.interact)
summary(ols.cropsha.interact, robust = T)


# Aid * Illegal Crops
interplot(m = ols.cropsha.interact, var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Estimated Coefficient for Aid") + xlab("Presence of Illegal Crops") + 
  geom_hline(yintercept = 0, color="grey35", size = .3)

setwd(dir.figures)
ggsave("interact_aid_illegalcrops_ALL-INTERACTION-MODEL.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)










