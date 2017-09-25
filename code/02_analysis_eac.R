#-----------------------------------------------------------------------------#
#  File Name:    02_analysis_eac.R
#  Author:       Billy Matthias 
#  Email:        bmatthias88@gmail.com
#  Purpose:      (1) exploratory data analysis
#                (2) stats analysis
#  Last Update:  2017-Sept-14
#  Data Output:    
#-----------------------------------------------------------------------------#


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
                          ####    I. SETUP    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
rm(list=ls())

library(sandwich)
library(RCurl)
library(interplot)
library(tidyverse)
library(ggplot2)
library(modelr)
library(haven)
library(magrittr)
options(na.action = na.warn)
library(xtable)
library(knitr)
library(texreg)
library(broom)
library(htmltools)
library(stringr)

## SET FILE PATHS
# Set working directory to where you saved project directory (folder)
# 'dir.home' the only file path you need to change!
dir.home <- file.path("/Users/wtmatthias/Google Drive/Mike RA")

dir.outdata <- file.path(dir.home,
                         "elections_aid_cropshare/data_201708/01_outputdata")

dir.analysis <- file.path(dir.home,
                          "elections_aid_cropshare/data_201708")
dir.figures.tables <- file.path(
                    dir.home,
                    "elections_aid_cropshare/data_201708/02_tables_and_figures")

## LOAD DATA 
load(file = file.path(
  dir.outdata,
  "eac_vio_muni_cross-section.RData"
))


## ROBUST STANDARD ERRORS
# import the function
# url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
# eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
#      envir=.GlobalEnv)

## CHANGE NAME OF DF
eac <- aid_merged
rm(aid_merged)



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
                  ####  II. EXPLORATORY DATA ANALYSIS  ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

eac_names <- as.data.frame(variable.names(eac))
View(eac_names)

# DV AND COVARIATES:
#   DVs: - trendcrop
#        - trendcropveg
#        - trendgrass
#        - trendc3
#        - muni_locd -> aid targeted to muni
#        - cropsha_chg

#   IVs: - cropsha_pct01: 
#        - SPI12m_sd:
#        - sq_km_mun:  area km2 muni
#        - geo_3:  area km2 muni (DANE official), 1993-2014
#        - upstream:
#        - altitude:
#        - road0:
#        - illegal_crops:  illegal crop presence dummy
#        - vio_13:  avg coca pres dummy, 1993-2015
#        - farc_zone:  FARC presence dummy
#        - vio_15: avg FARC presence over 1993-2015 (might be useful to have a modal value, dummy)
#        - "i.coddepto": department fixed effects (*create dept dummies)

#        PRES. VARS
#        - pres_centro:
#        - pres_conserv:
#        - pres_izqui:
#        - pres_tv:
#        - pres_lib:
#        - pres_min:
#        - pres_urib:
#        - pol_9:  tie
#        - pol_11:  % "minority" gets most votes
#        - pol_12:  % "conservador" gets most votes
#        - pol_13:  % "centro" gets most votes
#        - pol_14:  % "izquierda" gets most votes
#        - pol_15:  % "liberal" gets most votes
#        - pol_16:  % "tv" gets most votes
#        - pol_17:  % "uribismo" gets most votes
#        - pol_18:  total of valid votes
#        - pres_turnout:  pol_18/demo_3_mean (*need to create)
#        - pres_pidmuni:  pres ideo & muni ideo w/ highest vote the same? y = 1  

#        CAMARA VARS
#        - c_turnout:  avg camara elec turnout, 2002-2014
#        - c_dvote_avg
#        - c_mvote_avg
#        - c_match_munidept
#        - c_match_presdept
#        - c_match_munideptpres
#        - c_match_none
#        - c_votepro_centro: avg vote proportion @ muni
#        - c_votepro_conserv
#        - c_votepro_iz
#        - c_votepro_lib
#        - c_votepro_min
#        - c_votepro_other
#        - c_votepro_tv
#        - c_votepro_urib
#        - c_seatpro_centro: avg proportion of seats won
#        - c_seatpro_conserv
#        - c_seatpro_iz
#        - c_seatpro_lib
#        - c_seatpro_min
#        - c_seatpro_other
#        - c_seatpro_tv
#        - c_seatpro_urib

#        SENADO VARS
#        - s_turnout:  senado elec turnout
#        - s_match_muniseat:
#        - s_match_presseat:
#        - s_match_muniseatpres:
#        - s_match_none
#        - s_votepro_centro: avg vote proportion @ muni
#        - s_votepro_conserv
#        - s_votepro_iz
#        - s_votepro_lib
#        - s_votepro_min
#        - s_votepro_other
#        - s_votepro_tv
#        - s_votepro_urib
#        - s_seatpro_centro: avg proportion of seats won
#        - s_seatpro_conserv
#        - s_seatpro_iz
#        - s_seatpro_lib
#        - s_seatpro_min
#        - s_seatpro_other
#        - s_seatpro_tv
#        - s_seatpro_urib

#        VIOLENCE VARS
#        - vio_1: length of conflict. 0-3 (w/out conflict to permanent conflict). 2000-2012
#        - vio_2: armed conflict typology. 0-5 (no conflict to strongly effected/persistent)
#        - vio_3: conflict intensity. 0-2. no conflict, low, high. 2000-2012
#        - vio_9: # displacement occurence (what's this var mean?)
#        - vio_10: police reinforcements of "seguridad democratica". 1993-2015
#        - vio_11: dummy AUC presence. 1993-2015
#        - vio_14 dummy ELN
#        - vio_15: FARC
#        - vio_26: est vs. guer. to 2009
#        - vio_27: est vs. para/neopara
#        - vio_28: para/neopara vs. gue
#        - vio_31-33: (31) pop exposure est (32) pop exposure guer (33) pop exposure par/neo
#        - vio_34-36: (34) dispute est vs. guer (35) dispute est vs. par/neopar
#                     (36) dispute par/neopar vs. guer
#        - vio_30: lincoln-petersen calculation for displaced persons
#        - vio_46: incidence of conflict 1 to 5 (low to high), 2002-2013
#        - vio_47: incidence of conflict %, 2002-2013 (same as vio_46?)
#        - vio_87: # of ppl displaced, 1999-2014


## 2a) Summary/Descriptive Stats Table -----------------------------------------

# select() vars to use in "" function below
descriptive_vars <- eac %>%
  ungroup() %>% 
  select(trendcrop, trendc3, trendcropveg, trendgrass,
         muni_locd, cropsha_pct01, SPI12m_mean, SPI12m_sd,
         upstream, altitude, road0,
         illegal_crops, vio_13,
         farc_zone, vio_15,
         starts_with("pres_"),
         num_range("pol_", 9:18),
         starts_with("c_"),
         starts_with("s_"),
         num_range("vio_", c(1:3, 9:11, 14, 26:28, 31:36, 46:47, 87)),
         vio_26_mean, vio_27_mean, vio_28_mean, vio_31_mean, vio_32_mean,
         vio_33_mean, vio_34_mean, vio_35_mean, vio_36_mean
  )

# "Inf" values in cropsha_pct01 compute descriptive stats properly
descriptive_vars %<>%
  mutate(cropsha_pct01 = if_else(is.infinite(cropsha_pct01),
                                 NA_real_,
                                 cropsha_pct01)
  )


# "overview" = as in overview or summary of vars. can't use summarize
overview <- function(descriptive_vars, stat){
 stat <- enquo(stat)
  
 overview_df <- descriptive_vars %>%
    summarise_all(stat, na.rm = TRUE) %>% 
    gather(key = "name_var", value = !!stat)
}

# df for each descriptive stat
summarized_mean <- overview(descriptive_vars, mean)
summarized_med <- overview(descriptive_vars, median)
summarized_sd <- overview(descriptive_vars, sd)
summarized_min <- overview(descriptive_vars, min)
summarized_max <- overview(descriptive_vars, max)


# "count # of non-NA observations" is not it's own summary function
summarized_n <- descriptive_vars %>%
  map_df(function(x) sum(!is.na(x))) %>%
  gather(key = "name_var", value = "n")


# summary stats table w/ var names and values across the row
summarized_vars <- bind_cols(summarized_n, summarized_mean, summarized_sd,
                             summarized_med, summarized_max, summarized_min)

# create a tidy df w/ no duplicate cols
summarized_vars %<>% select(-num_range("name_var", c(1:5)))

summarized_vars %<>% rename(Variable = name_var, Mean = mean, `Std Dev` = sd,
                            Median = median, Max = max, Min = min)

# require("xtable")
summarized_vars_html <- xtable(summarized_vars, digits = 0) %>%
  print(type = "html",
        html.table.attributes = "",
        include.rownames = FALSE,
        format.args = list(big.mark = ","))

# require(knitr)
kable(summarized_vars)

## 2b) Visualize Key Variables ------------------------------------------------

# eac %>%
#   select(codmuni, starts_with("s_seatpro_")
#   ) %>%
#   View()


## coca presence ----
# replace NA in coca_km2 w/ 0 only for illegal crop == 0 munis
eac %>% count(illegal_crops)
eac %>% count(is.na(coca_km2))
# eac %>% select(codmuni, illegal_crops, coca_km2) %>% filter(illegal_crops == 0 & is.na(coca_km2)) %>% View()

eac %<>%
  mutate(
    coca_km2 = if_else(illegal_crops == 0 & is.na(coca_km2),
                       0,
                       coca_km2
    )
  ) 

class(eac$illegal_crops)
eac %>% select(illegal_crops) %>% count()
eac$illegal_crops <- as.integer(eac$illegal_crops)

# illegal crops (dummy variable)
ggplot(eac) + geom_bar(aes(x = illegal_crops))

# vio_13 (coca presence dummy)
ggplot(eac) + geom_freqpoly(aes(x = vio_13), binwidth = 0.05) # count
ggplot(eac) + geom_histogram(aes(x = vio_13), binwidth = 0.05) # 

eac %>% select(codmuni, vio_13) %>% View()

## AID (muni_locd)
eac$muni_locd <- as.integer(eac$muni_locd)
ggplot(data = eac) + geom_bar(mapping = aes(x = muni_locd))
eac %>% select(muni_locd) %>% count()



## trendcrop ----
ggplot(eac) + geom_freqpoly(aes(x = trendcrop), binwidth = 20)

ggplot(eac) +
  geom_histogram(aes(x = trendcrop), binwidth = 20) +
  xlim(-500, 500)

# outliers
eac %>% select(codmuni, trendcrop) %>% filter(trendcrop > 500)
eac %>% select(codmuni, trendcrop) %>% filter(trendcrop < -500)
eac %>% select(codmuni, department, municipality, trendcrop) %>% filter(codmuni == 44847)

# data w/out the huge outlier (NAs also dropped)
crop_out <- eac %>% filter(trendcrop < 10000)

# w/out outlier
ggplot(data = crop_out, aes(x = trendcrop)) +
  geom_freqpoly(binwidth = 20)

ggplot(crop_out) +
  geom_histogram(aes(x = trendcrop), binwidth = 20) +
  xlim(-500, 500)



## trendcropveg ----
# outliers
eac %>%
  select(codmuni, department, municipality, trendcropveg) %>%
  filter(trendcropveg < -2000)
eac %>%
  select(codmuni, department, municipality, trendcropveg) %>%
  filter(trendcropveg > 4000)

# distribution w/out some outliers
ggplot(eac) +
  geom_histogram(aes(x = trendcropveg), binwidth = 20) +
  xlim(-2000, 4000)

# data w/out cropveg outlier
cropveg_out <- eac %>% filter(trendcropveg < 8000)



## road0 ----



## upstream ----



## match vars: pres_pidmuni ----
eac %>% ungroup() %>% select(pres_pidmuni) %>% count()
ggplot(eac) + geom_histogram(aes(x = pres_pidmuni), bins = 6)


## camara match vars ----
# c_match_munidept
eac %>% select(c_match_munidept) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_munidept), bins = 7)

# c_match_presdept
eac %>% select(c_match_presdept) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_presdept), bins = 7)

# c_match_munipresdept
eac %>% select(c_match_munideptpres) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_munideptpres), bins = 7)

# c_match_none
eac %>% select(c_match_none) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_none), bins = 6)



## senado match vars ----
# s_match_muniseat
eac %>% select(s_match_muniseat) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_muniseat), bins = 7)

# s_match_presseat
eac %>% select(s_match_presseat) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_presseat), bins = 7)

# s_match_munipresseat
eac %>% select(s_match_muniseatpres) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_muniseatpres), bins = 7)

# s_match_none
eac %>% select(s_match_none) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_none), bins = 6)


## vio_2 ----



## farc_zone & vio_15 ----



## vio_31-33: population exposure ----
# vio_31: exposure to state
ggplot(eac) + geom_histogram(aes(x = vio_31_mean), binwidth = .1)
# vio_32: exposure to guer
ggplot(eac) + geom_histogram(aes(x = vio_32_mean), binwidth = .1)
# vio_33: exposure to guer
ggplot(eac) + geom_histogram(aes(x = vio_33_mean), binwidth = .1)


## vio_34-36: dispute ----
# vio_34: state vs. guerilla conflict
ggplot(eac) + geom_histogram(aes(x = vio_34_mean), binwidth = .1)

# vio_35: state vs. paramilitary/neoparamilitary
ggplot(eac) + geom_histogram(aes(x = vio_35_mean), binwidth = .1)

# vio_36: guer vs. paramilitary/neoparamilitary
ggplot(eac) + geom_histogram(aes(x = vio_36_mean), binwidth = .1)


## vio_46: incidence of conflict ----
ggplot(eac) + geom_bar(aes(x = vio_46))


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   ROBUST STANDARD ERRORS   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
robust <- function(model.object){
  sandwich_se <- sqrt(diag(vcovHC(model.object, type = "HC1")))

  z_stat <- coef(model.object)/sandwich_se
  p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
  
  robust.mod <- summary(model.object)

  robust.mod$coefficients[, 2] <- sandwich_se
  robust.mod$coefficients[, 3] <- z_stat
  robust.mod$coefficients[, 4] <- p_values
  corrected <- robust.mod
  se.corrections <- list("robust.mod" = corrected, "sandwich_se" = sandwich_se,
                         "z_stat" = z_stat, "p_values" = p_values)
  se.corrections
}

## robust SEs through map and purrr:
# sandwich_se <- map(cropveg.aid.coca1, ~ sqrt(diag(vcovHC(., type = "HC1")
# )
# )
# )
# z_stat <- map2(cropveg.aid.coca1, sandwich_se, ~ coef(.x)/.y)
# 
# p_values <- map(z_stat, ~ pchisq(.^2, 1, lower.tail=FALSE))



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   I. CROPSHARE (cropveg): AID + COCA (avg.) +  POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
ggplot(cropveg_out, aes(muni_locd, trendcropveg, group = muni_locd)) +
  geom_boxplot()

## models ----
# 1) base model: cropshare = aid + coca
# 2) base model + aid*coca
# 3) base model + pol vars (_match_none dropped out!)
# 4) base model + aid*coca + pol vars



formulae.cropveg1 <- list(
  trendcropveg ~ muni_locd + vio_13 + SPI12m_sd + geo_3 + upstream + altitude,
  trendcropveg ~ muni_locd + vio_13 + SPI12m_sd + geo_3 + upstream + altitude +
                 muni_locd:vio_13,
  trendcropveg ~ muni_locd + vio_13 + SPI12m_sd + geo_3 + upstream + altitude +
                 c_match_presdept + c_match_munidept + c_match_munideptpres +
                 pres_pidmuni + s_match_presseat +
                 s_match_muniseatpres,
  trendcropveg ~ muni_locd + vio_13 + SPI12m_sd + geo_3 + upstream + altitude +
                 c_match_presdept + c_match_munidept + c_match_munideptpres +
                 pres_pidmuni + s_match_presseat +
                 s_match_muniseatpres +
                 muni_locd:vio_13
  )

cropveg.aid.coca1 <- map(formulae.cropveg1, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.coca1 %>% map(summary)
cropveg.aid.coca1.r <- cropveg.aid.coca1 %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.coca1.out <- map(formulae.cropveg1, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.coca1.out %>% map(summary)
cropveg.aid.coca1.out %>% map(robust)

sandwich_se <- list(cropveg.aid.coca1.r[[1]]$sandwich_se,
                    cropveg.aid.coca1.r[[2]]$sandwich_se,
                    cropveg.aid.coca1.r[[3]]$sandwich_se,
                    cropveg.aid.coca1.r[[4]]$sandwich_se)
p_values <- list(cropveg.aid.coca1.r[[1]]$p_values,
                    cropveg.aid.coca1.r[[2]]$p_values,
                    cropveg.aid.coca1.r[[3]]$p_values,
                    cropveg.aid.coca1.r[[4]]$p_values)

## output regression tables ----

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "Coca presence avg", "SPI.sd",
              "Muni area", "Upstream", "Altitude",
              "c_match_presdept", "c_match_munidept", "c_match_munideptpres",
              "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
              "Aid * Coca")
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent variable.
        'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
        exclusive categories compared against a category where there is no match
        in ideology."

# my.reg.table <- function(lm_list_object, ..., note = NULL){
#   htmlreg(lm_list_object,
#           custom.note = if (!is.null(note)) str_c("Note: ", note) else NULL,
#           digits = 2,
#           leading.zero = TRUE,
#           # better for markdown
#           doctype = FALSE,
#           html.tag = FALSE,
#           head.tag = FALSE,
#           body.tag = FALSE,
#           # passed to extract() method for "lm"
#           include.adjr = TRUE,
#           include.rsquared = FALSE,
#           include.rmse = FALSE,
#           include.nobs = TRUE)
# }

cropveg_table <- htmlreg(cropveg.aid.coca1,
                  custom.note = note,
                  digits = 2,
                  leading.zero = TRUE,
                  # better for markdown
                  doctype = FALSE,
                  html.tag = FALSE,
                  head.tag = FALSE,
                  body.tag = FALSE,
                  # passed to extract() method for "lm"
                  include.adjr = TRUE,
                  include.rsquared = FALSE,
                  include.rmse = FALSE,
                  include.nobs = TRUE,
                  custom.model.names = str_c("(", seq_along(cropveg.aid.coca1), ")"),
                  custom.coef.names = coefnames,
                  caption.above = TRUE,
                  caption = "Aid's Effect on MODIS Crop Veg Cover",
                  override.se = sandwich_se,
                  override.pvalues = p_values,
                  star.symbol = "*"
                  )

cropveg_table %>% HTML() %>% browsable()


# Word doc
htmlreg(cropveg.aid.coca1,
        file = "cropveg_aid_coca1.doc",
        custom.note = note,
        digits = 2,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = TRUE,
        custom.model.names = str_c("(", seq_along(cropveg.aid.coca1), ")"),
        custom.coef.names = coefnames,
        caption.above = TRUE,
        caption = "Aid's Effect on MODIS Crop Veg Cover",
        override.se = sandwich_se,
        override.pvalues = p_values,
        inline.css = FALSE,
        # better for Word doc
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        # passed to extract() method for "lm"
        include.adjr = TRUE,
        include.rsquared = FALSE,
        include.rmse = FALSE,
        include.nobs = TRUE
        )


# # Latex doc
# texreg(cropveg.aid.coca1,
#        booktabs = TRUE,
#        dcolumn = TRUE,
#        table = TRUE,
#        sideways = TRUE,
#         stars = c(0.01, 0.05, 0.1),
#                   custom.note = note,
#                   digits = 2,
#                   leading.zero = TRUE,
#                   # passed to extract() method for "lm"
#                   include.adjr = TRUE,
#                   include.rsquared = FALSE,
#                   include.rmse = FALSE,
#                   include.nobs = TRUE,
#                   custom.model.names = str_c("(", seq_along(cropveg.aid.coca1), ")"),
#                   custom.coef.names = coefnames,
#                   caption.above = TRUE,
#                   caption = "Aid's Effect on MODIS Crop Veg Cover",
#                   override.se = sandwich_se,
#                   override.pvalues = p_values
#                   )


# # TIDY, BROOM, GLANCE
# cropveg.aid.coca1
# glance(cropveg.aid.coca1[[4]])
# tidy(cropveg.aid.coca1[[4]])


# interaction plots ----

# aid*coca
interplot(m = cropveg.aid.coca1[[2]], var1 = "muni_locd", var2 = "vio_13") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-muni.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.coca1[[4]], var1 = "muni_locd", var2 = "vio_13") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-muni_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   II. CROPSHARE: AID + COCA (dummy) + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
# replicate above but w/ illegal crop dummy instead of avg.


## models ---------------------------------------------------------------------
# 1) base model: cropshare = aid + coca
# 2) base model + aid*coca
# 3) base model + pol vars (_match_none dropped out!)
# 4) base model + aid*coca + pol vars
formulae.cropveg2 <- list(
  trendcropveg ~ muni_locd + illegal_crops + SPI12m_sd + geo_3 + upstream + altitude,
  trendcropveg ~ muni_locd + illegal_crops + SPI12m_sd + geo_3 + upstream + altitude +
    muni_locd:illegal_crops,
  trendcropveg ~ muni_locd + illegal_crops + SPI12m_sd + geo_3 + upstream + altitude +
    c_match_presdept + c_match_munidept + c_match_munideptpres +
    pres_pidmuni + s_match_presseat +
    s_match_muniseatpres,
  trendcropveg ~ muni_locd + illegal_crops + SPI12m_sd + geo_3 + upstream + altitude +
    c_match_presdept + c_match_munidept + c_match_munideptpres +
    pres_pidmuni + s_match_presseat +
    s_match_muniseatpres +
    muni_locd:illegal_crops
)

cropveg.aid.coca2 <- map(formulae.cropveg2, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.coca2 %>% map(summary)
cropveg.aid.coca2.r <- cropveg.aid.coca2 %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.coca2.out <- map(formulae.cropveg2, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.coca2.out %>% map(summary)
cropveg.aid.coca2.out.r <- cropveg.aid.coca2.out %>% map(robust)

sandwich_se <- list(cropveg.aid.coca2.r[[1]]$sandwich_se,
                    cropveg.aid.coca2.r[[2]]$sandwich_se,
                    cropveg.aid.coca2.r[[3]]$sandwich_se,
                    cropveg.aid.coca2.r[[4]]$sandwich_se)
p_values <- list(cropveg.aid.coca2.r[[1]]$p_values,
                 cropveg.aid.coca2.r[[2]]$p_values,
                 cropveg.aid.coca2.r[[3]]$p_values,
                 cropveg.aid.coca2.r[[4]]$p_values)

sandwich_se_out <- list(cropveg.aid.coca2.out.r[[1]]$sandwich_se,
                     cropveg.aid.coca2.out.r[[2]]$sandwich_se,
                     cropveg.aid.coca2.out.r[[3]]$sandwich_se,
                     cropveg.aid.coca2.out.r[[4]]$sandwich_se)
p_values_out <- list(cropveg.aid.coca2.out.r[[1]]$p_values,
                  cropveg.aid.coca2.out.r[[2]]$p_values,
                  cropveg.aid.coca2.out.r[[3]]$p_values,
                  cropveg.aid.coca2.out.r[[4]]$p_values)


## output regression tables ----------------------------------------------------

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "Coca presence dummy", "SPI.sd",
               "Muni area", "Upstream", "Altitude",
               "c_match_presdept", "c_match_munidept", "c_match_munideptpres",
               "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
               "Aid * Coca")
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent variable.
        'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
        exclusive categories compared against a category where there is no match
        in ideology."


# cropveg_table2 <- htmlreg(cropveg.aid.coca2,
#                          custom.note = note,
#                          digits = 2,
#                          leading.zero = TRUE,
#                          # better for markdown
#                          doctype = FALSE,
#                          html.tag = FALSE,
#                          head.tag = FALSE,
#                          body.tag = FALSE,
#                          # passed to extract() method for "lm"
#                          include.adjr = TRUE,
#                          include.rsquared = FALSE,
#                          include.rmse = FALSE,
#                          include.nobs = TRUE,
#                          custom.model.names = str_c("(", seq_along(cropveg.aid.coca2), ")"),
#                          custom.coef.names = coefnames,
#                          caption.above = TRUE,
#                          caption = "Aid's Effect on MODIS Crop Veg Cover",
#                          override.se = sandwich_se,
#                          override.pvalues = p_values,
#                          star.symbol = "*"
# )
# 
# cropveg_table2 %>% HTML() %>% browsable()

# outlier table
# cropveg_table2_out <- htmlreg(cropveg.aid.coca2.out,
#                           custom.note = note,
#                           digits = 2,
#                           leading.zero = TRUE,
#                           # better for markdown
#                           doctype = FALSE,
#                           html.tag = FALSE,
#                           head.tag = FALSE,
#                           body.tag = FALSE,
#                           # passed to extract() method for "lm"
#                           include.adjr = TRUE,
#                           include.rsquared = FALSE,
#                           include.rmse = FALSE,
#                           include.nobs = TRUE,
#                           custom.model.names = str_c("(", seq_along(cropveg.aid.coca2.out), ")"),
#                           custom.coef.names = coefnames,
#                           caption.above = TRUE,
#                           caption = "Aid's Effect on MODIS Crop Veg Cover (w/out Uribia)",
#                           override.se = sandwich_se_out,
#                           override.pvalues = p_values_out,
#                           star.symbol = "*"
# )
# cropveg_table2_out %>% HTML() %>% browsable()

# Word doc
htmlreg(cropveg.aid.coca2,
        file = "cropveg_aid_coca2_table.doc",
        custom.note = note,
        digits = 2,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = TRUE,
        custom.model.names = str_c("(", seq_along(cropveg.aid.coca2), ")"),
        custom.coef.names = coefnames,
        caption.above = TRUE,
        caption = "Aid's Effect on MODIS Crop Veg Cover",
        override.se = sandwich_se,
        override.pvalues = p_values,
        inline.css = FALSE,
        # better for Word doc
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        # passed to extract() method for "lm"
        include.adjr = TRUE,
        include.rsquared = FALSE,
        include.rmse = FALSE,
        include.nobs = TRUE
)
htmlreg(cropveg.aid.coca2.out,
        file = "cropveg_aid_coca2_out_table.doc",
        custom.note = note,
        digits = 2,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = TRUE,
        custom.model.names = str_c("(", seq_along(cropveg.aid.coca2.out), ")"),
        custom.coef.names = coefnames,
        caption.above = TRUE,
        caption = "Aid's Effect on MODIS Crop Veg Cover (without Uribia)",
        override.se = sandwich_se,
        override.pvalues = p_values,
        inline.css = FALSE,
        # better for Word doc
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        # passed to extract() method for "lm"
        include.adjr = TRUE,
        include.rsquared = FALSE,
        include.rmse = FALSE,
        include.nobs = TRUE
)



## interaction plots ------------------------------------------------------------

# aid*coca
interplot(m = cropveg.aid.coca2[[2]], var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)")

setwd(dir.figures.tables)
ggsave("cropveg2_aid-muni.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.coca2[[4]], var1 = "muni_locd", var2 = "illegal_crops") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg2_aid-muni_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   III. CROPSHARE: AID + STATE-GUER CONF + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

# 1) base model: cropshare = aid + state_guer conflict
# 2) base model + aid*state-guer
# 3) base model + pol vars
# 4) base model + pol vars + aid*state_guer

formulae.cropveg3 <- list(
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
                 altitude + vio_31_mean + vio_32_mean,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
                 altitude + vio_31_mean + vio_32_mean + muni_locd:vio_34_mean,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
                 altitude + vio_31_mean + vio_32_mean + c_match_presdept +
                 c_match_munidept + c_match_munideptpres + pres_pidmuni +
                 s_match_presseat + s_match_muniseatpres,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
                 altitude + vio_31_mean + vio_32_mean + c_match_presdept +
                 c_match_munidept + c_match_munideptpres + pres_pidmuni +
                 s_match_presseat + s_match_muniseatpres + muni_locd:vio_34_mean
)

cropveg.aid.dispute <- map(formulae.cropveg3, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.dispute %>% map(summary)
cropveg.aid.dispute.r <- cropveg.aid.dispute %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.dispute.out <- map(formulae.cropveg3, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.dispute.out %>% map(summary)
cropveg.aid.dispute.out.r <- cropveg.aid.dispute.out %>% map(robust)

sandwich_se <- list(cropveg.aid.dispute.r[[1]]$sandwich_se,
                    cropveg.aid.dispute.r[[2]]$sandwich_se,
                    cropveg.aid.dispute.r[[3]]$sandwich_se,
                    cropveg.aid.dispute.r[[4]]$sandwich_se)
p_values <- list(cropveg.aid.dispute.r[[1]]$p_values,
                 cropveg.aid.dispute.r[[2]]$p_values,
                 cropveg.aid.dispute.r[[3]]$p_values,
                 cropveg.aid.dispute.r[[4]]$p_values)

sandwich_se_out <- list(cropveg.aid.dispute.out.r[[1]]$sandwich_se,
                        cropveg.aid.dispute.out.r[[2]]$sandwich_se,
                        cropveg.aid.dispute.out.r[[3]]$sandwich_se,
                        cropveg.aid.dispute.out.r[[4]]$sandwich_se)
p_values_out <- list(cropveg.aid.dispute.out.r[[1]]$p_values,
                     cropveg.aid.dispute.out.r[[2]]$p_values,
                     cropveg.aid.dispute.out.r[[3]]$p_values,
                     cropveg.aid.dispute.out.r[[4]]$p_values)


## output regression tables ----------------------------------------------------

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "State-Guer Dispute", "SPI.sd",
               "Muni area", "Upstream", "Altitude", "Pop. vs. State Exposure",
               "Pop. vs. Guer. Exposure", "c_match_presdept",
               "c_match_munidept", "c_match_munideptpres",
               "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
               "Aid * Dispute")

note <- " %stars. OLS regressions with MODIS cropveg trend as the dependent variable.
'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
exclusive categories compared against a category where there is no match
in ideology. 'State-Guerilla Dispute' is the proportion of years in which a
municipality observed violent conflict among government and guerilla forces
(e.g. FARC)."


cropveg_table3 <- htmlreg(cropveg.aid.dispute,
                          custom.note = note,
                          digits = 2,
                          leading.zero = TRUE,
                          # better for markdown
                          doctype = FALSE,
                          html.tag = FALSE,
                          head.tag = FALSE,
                          body.tag = FALSE,
                          # passed to extract() method for "lm"
                          include.adjr = TRUE,
                          include.rsquared = FALSE,
                          include.rmse = FALSE,
                          include.nobs = TRUE,
                          custom.model.names = str_c("(", seq_along(cropveg.aid.dispute), ")"),
                          custom.coef.names = coefnames,
                          caption.above = TRUE,
                          caption = "Aid's Effect on MODIS Crop Veg Cover (conditioned by state-guerilla disputes)",
                          override.se = sandwich_se,
                          override.pvalues = p_values,
                          star.symbol = "*"
)

cropveg_table3 %>% HTML() %>% browsable()

# outlier table
cropveg_table3_out <- htmlreg(cropveg.aid.dispute.out,
                              custom.note = note,
                              digits = 2,
                              leading.zero = TRUE,
                              # better for markdown
                              doctype = FALSE,
                              html.tag = FALSE,
                              head.tag = FALSE,
                              body.tag = FALSE,
                              # passed to extract() method for "lm"
                              include.adjr = TRUE,
                              include.rsquared = FALSE,
                              include.rmse = FALSE,
                              include.nobs = TRUE,
                              custom.model.names = str_c("(", seq_along(cropveg.aid.dispute.out), ")"),
                              custom.coef.names = coefnames,
                              caption.above = TRUE,
                              caption = "Aid's Effect on MODIS Crop Veg Cover (w/out Uribia)",
                              override.se = sandwich_se_out,
                              override.pvalues = p_values_out,
                              star.symbol = "*"
)
cropveg_table3_out %>% HTML() %>% browsable()


htmlreg(cropveg.aid.dispute,
        file = "cropveg_aid_dispute_table.doc",
        custom.note = note,
        digits = 2,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = TRUE,
        custom.model.names = str_c("(", seq_along(cropveg.aid.dispute), ")"),
        custom.coef.names = coefnames,
        caption.above = TRUE,
        caption = "Aid's Effect on MODIS Crop Veg Cover",
        override.se = sandwich_se,
        override.pvalues = p_values,
        inline.css = FALSE,
        # better for Word doc
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        # passed to extract() method for "lm"
        include.adjr = TRUE,
        include.rsquared = FALSE,
        include.rmse = FALSE,
        include.nobs = TRUE
)
htmlreg(cropveg.aid.coca2.out,
        file = "cropveg_aid_dispute_out_table.doc",
        custom.note = note,
        digits = 2,
        stars = c(0.01, 0.05, 0.1),
        leading.zero = TRUE,
        custom.model.names = str_c("(", seq_along(cropveg.aid.coca2.out), ")"),
        custom.coef.names = coefnames,
        caption.above = TRUE,
        caption = "Aid's Effect on MODIS Crop Veg Cover (without Uribia)",
        override.se = sandwich_se,
        override.pvalues = p_values,
        inline.css = FALSE,
        # better for Word doc
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        # passed to extract() method for "lm"
        include.adjr = TRUE,
        include.rsquared = FALSE,
        include.rmse = FALSE,
        include.nobs = TRUE
)



## interaction plots ------------------------------------------------------------

# aid*coca
interplot(m = cropveg.aid.dispute[[2]], var1 = "muni_locd", var2 = "vio_34_mean") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Proportion of State-Guerilla Disputes ()") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2012)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-dispute.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.dispute[[4]], var1 = "muni_locd", var2 = "vio_34_mean") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2012)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-dispute_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   IV. CROPSHARE: AID + CONF INCIDENCE/INTENSITY + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
# 1) base model: cropshare = aid + conflict incidence
# 2) base model + aid*conflict-incidence
# 3) base model + pol vars
# 4) base model + pol vars + aid*conflict-incidence

formulae.cropveg4 <- list(
  trendcropveg ~ muni_locd + vio_46 + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean,
  trendcropveg ~ muni_locd + vio_46 + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + muni_locd:vio_46,
  trendcropveg ~ muni_locd + vio_46 + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + c_match_presdept +
    c_match_munidept + c_match_munideptpres + pres_pidmuni +
    s_match_presseat + s_match_muniseatpres,
  trendcropveg ~ muni_locd + vio_46 + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + c_match_presdept +
    c_match_munidept + c_match_munideptpres + pres_pidmuni +
    s_match_presseat + s_match_muniseatpres + muni_locd:vio_46
)

cropveg.aid.incidence <- map(formulae.cropveg4, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.incidence %>% map(summary)
cropveg.aid.incidence.r <- cropveg.aid.incidence %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.incidence.out <- map(formulae.cropveg4, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.incidence.out %>% map(summary)
cropveg.aid.incidence.out.r <- cropveg.aid.incidence.out %>% map(robust)

sandwich_se <- list(cropveg.aid.incidence.r[[1]]$sandwich_se,
                    cropveg.aid.incidence.r[[2]]$sandwich_se,
                    cropveg.aid.incidence.r[[3]]$sandwich_se,
                    cropveg.aid.incidence.r[[4]]$sandwich_se)
p_values <- list(cropveg.aid.incidence.r[[1]]$p_values,
                 cropveg.aid.incidence.r[[2]]$p_values,
                 cropveg.aid.incidence.r[[3]]$p_values,
                 cropveg.aid.incidence.r[[4]]$p_values)

sandwich_se_out <- list(cropveg.aid.incidence.out.r[[1]]$sandwich_se,
                        cropveg.aid.incidence.out.r[[2]]$sandwich_se,
                        cropveg.aid.incidence.out.r[[3]]$sandwich_se,
                        cropveg.aid.incidence.out.r[[4]]$sandwich_se)
p_values_out <- list(cropveg.aid.incidence.out.r[[1]]$p_values,
                     cropveg.aid.incidence.out.r[[2]]$p_values,
                     cropveg.aid.incidence.out.r[[3]]$p_values,
                     cropveg.aid.incidence.out.r[[4]]$p_values)


## output regression tables ----------------------------------------------------

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "Conflict Incidence", "SPI.sd",
               "Muni area", "Upstream", "Altitude", "Pop. vs. State Exposure",
               "Pop. vs. Guer. Exposure", "c_match_presdept",
               "c_match_munidept", "c_match_munideptpres",
               "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
               "Aid * Incidence")

note <- "OLS regressions with MODIS cropveg trend as the dependent variable.
'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
exclusive categories compared against a category where there is no match
in ideology. 'Conflict Incidence' takes on values 1-5 moving from low levels of
conflict occurence to high levels of conflict occurence."


cropveg_table4 <- htmlreg(cropveg.aid.incidence,
                          custom.note = note,
                          digits = 2,
                          leading.zero = TRUE,
                          # better for markdown
                          doctype = FALSE,
                          html.tag = FALSE,
                          head.tag = FALSE,
                          body.tag = FALSE,
                          # passed to extract() method for "lm"
                          include.adjr = TRUE,
                          include.rsquared = FALSE,
                          include.rmse = FALSE,
                          include.nobs = TRUE,
                          custom.model.names = str_c("(", seq_along(cropveg.aid.incidence), ")"),
                          custom.coef.names = coefnames,
                          caption.above = TRUE,
                          caption = "Aid's Effect on MODIS Crop Veg Cover (conditioned by state-guerilla disputes)",
                          override.se = sandwich_se,
                          override.pvalues = p_values,
                          star.symbol = "*"
)

cropveg_table4 %>% HTML() %>% browsable()

# outlier table
cropveg_table4_out <- htmlreg(cropveg.aid.incidence.out,
                              custom.note = note,
                              digits = 2,
                              leading.zero = TRUE,
                              # better for markdown
                              doctype = FALSE,
                              html.tag = FALSE,
                              head.tag = FALSE,
                              body.tag = FALSE,
                              # passed to extract() method for "lm"
                              include.adjr = TRUE,
                              include.rsquared = FALSE,
                              include.rmse = FALSE,
                              include.nobs = TRUE,
                              custom.model.names = str_c("(", seq_along(cropveg.aid.incidence.out), ")"),
                              custom.coef.names = coefnames,
                              caption.above = TRUE,
                              caption = "Aid's Effect on MODIS Crop Veg Cover (w/out Uribia)",
                              override.se = sandwich_se_out,
                              override.pvalues = p_values_out,
                              star.symbol = "*"
)
cropveg_table4_out %>% HTML() %>% browsable()



## interaction plots ------------------------------------------------------------

# aid*coca
interplot(m = cropveg.aid.incidence[[2]], var1 = "muni_locd", var2 = "vio_46") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.incidence[[4]], var1 = "muni_locd", var2 = "vio_46") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   V. CROPSHARE: AID + STATE-GUER CONF + POL + COCA    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

formulae.cropveg5 <- list(
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + vio_13,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + vio_13 + muni_locd:vio_34_mean:vio_13,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + vio_13 + c_match_presdept +
    c_match_munidept + c_match_munideptpres + pres_pidmuni +
    s_match_presseat + s_match_muniseatpres + muni_locd:vio_34_mean:vio_13
)

cropveg.aid.triple <- map(formulae.cropveg5, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.triple %>% map(summary)
cropveg.aid.triple.r <- cropveg.aid.triple %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.triple.out <- map(formulae.cropveg5, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.triple.out %>% map(summary)
cropveg.aid.triple.out.r <- cropveg.aid.triple.out %>% map(robust)

sandwich_se <- list(cropveg.aid.triple.r[[1]]$sandwich_se,
                    cropveg.aid.triple.r[[2]]$sandwich_se,
                    cropveg.aid.triple.r[[3]]$sandwich_se)
p_values <- list(cropveg.aid.triple.r[[1]]$p_values,
                 cropveg.aid.triple.r[[2]]$p_values,
                 cropveg.aid.triple.r[[3]]$p_values)

sandwich_se_out <- list(cropveg.aid.triple.out.r[[1]]$sandwich_se,
                        cropveg.aid.triple.out.r[[2]]$sandwich_se,
                        cropveg.aid.triple.out.r[[3]]$sandwich_se)
p_values_out <- list(cropveg.aid.triple.out.r[[1]]$p_values,
                     cropveg.aid.triple.out.r[[2]]$p_values,
                     cropveg.aid.triple.out.r[[3]]$p_values)

triple_effect <- lm(trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 +
                      upstream + altitude + vio_31_mean + vio_32_mean + vio_13 +
                      muni_locd:vio_34_mean:vio_13,
                      data = eac)



## interaction plots -----------------------------------------------------------
ThreewayME.f(M = triple_effect,
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = "Aid's Effect on Crop Veg Cover",
             lloc = "bottomleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob = TRUE
            )

ThreewayME.f(M = triple_effect,
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = "Aid's Effect on Crop Veg Cover",
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob = TRUE
            )


## output regression tables ----------------------------------------------------

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "Conflict Incidence", "SPI.sd",
               "Muni area", "Upstream", "Altitude", "Pop. vs. State Exposure",
               "Pop. vs. Guer. Exposure", "c_match_presdept",
               "c_match_munidept", "c_match_munideptpres",
               "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
               "Aid * Incidence")

note <- "OLS regressions with MODIS cropveg trend as the dependent variable.
'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
exclusive categories compared against a category where there is no match
in ideology. 'Conflict Incidence' takes on values 1-5 moving from low levels of
conflict occurence to high levels of conflict occurence."


cropveg_table4 <- htmlreg(cropveg.aid.incidence,
                          custom.note = note,
                          digits = 2,
                          leading.zero = TRUE,
                          # better for markdown
                          doctype = FALSE,
                          html.tag = FALSE,
                          head.tag = FALSE,
                          body.tag = FALSE,
                          # passed to extract() method for "lm"
                          include.adjr = TRUE,
                          include.rsquared = FALSE,
                          include.rmse = FALSE,
                          include.nobs = TRUE,
                          custom.model.names = str_c("(", seq_along(cropveg.aid.incidence), ")"),
                          custom.coef.names = coefnames,
                          caption.above = TRUE,
                          caption = "Aid's Effect on MODIS Crop Veg Cover (conditioned by state-guerilla disputes)",
                          override.se = sandwich_se,
                          override.pvalues = p_values,
                          star.symbol = "*"
)

cropveg_table4 %>% HTML() %>% browsable()

# outlier table
cropveg_table4_out <- htmlreg(cropveg.aid.incidence.out,
                              custom.note = note,
                              digits = 2,
                              leading.zero = TRUE,
                              # better for markdown
                              doctype = FALSE,
                              html.tag = FALSE,
                              head.tag = FALSE,
                              body.tag = FALSE,
                              # passed to extract() method for "lm"
                              include.adjr = TRUE,
                              include.rsquared = FALSE,
                              include.rmse = FALSE,
                              include.nobs = TRUE,
                              custom.model.names = str_c("(", seq_along(cropveg.aid.incidence.out), ")"),
                              custom.coef.names = coefnames,
                              caption.above = TRUE,
                              caption = "Aid's Effect on MODIS Crop Veg Cover (w/out Uribia)",
                              override.se = sandwich_se_out,
                              override.pvalues = p_values_out,
                              star.symbol = "*"
)
cropveg_table4_out %>% HTML() %>% browsable()





# aid*coca
interplot(m = cropveg.aid.incidence[[2]], var1 = "muni_locd", var2 = "vio_46") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.incidence[[4]], var1 = "muni_locd", var2 = "vio_46") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   VI. CROPSHARE: AID + POP EXPOSURE + POL + COCA    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
formulae.cropveg6 <- list(
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + vio_13 + muni_locd:vio_32_mean,
  trendcropveg ~ muni_locd + vio_34_mean + SPI12m_sd + geo_3 + upstream +
    altitude + vio_31_mean + vio_32_mean + vio_13 + muni_locd:vio_31_mean)

cropveg.aid.popexpo <- map(formulae.cropveg6, ~ lm(.x, data = eac, model = TRUE))

## regression output
cropveg.aid.popexpo %>% map(summary)
cropveg.aid.popexpo.r <- cropveg.aid.popexpo %>% map(robust)

## outliers: dropped Uribia in La Guajira
cropveg.aid.popexpo.out <- map(formulae.cropveg6, ~ lm(.x, data = cropveg_out, model = TRUE))
cropveg.aid.popexpo.out %>% map(summary)
cropveg.aid.popexpo.out.r <- cropveg.aid.popexpo.out %>% map(robust)

sandwich_se <- list(cropveg.aid.popexpo.r[[1]]$sandwich_se,
                    cropveg.aid.popexpo.r[[2]]$sandwich_se
                    )
p_values <- list(cropveg.aid.popexpo.r[[1]]$p_values,
                 cropveg.aid.popexpo.r[[2]]$p_values
                 )
                 
sandwich_se_out <- list(cropveg.aid.popexpo.out.r[[1]]$sandwich_se,
                        cropveg.aid.popexpo.out.r[[2]]$sandwich_se
                        )
p_values_out <- list(cropveg.aid.popexpo.out.r[[1]]$p_values,
                     cropveg.aid.popexpo.out.r[[2]]$p_values
                     )


## output regression tables ----------------------------------------------------

# specify changes that will go into the Robust SEs table
coefnames <- c("(Intercept)", "Aid given", "State-Guer Dispute", "SPI.sd",
               "Muni area", "Upstream", "Altitude", "Pop. - State Exposure",
               "Pop. - Guer. Exposure", "Avg Coca Presence",
               "Aid * Pop-Guer", "Aid * Pop-State")

note <- "OLS regressions with MODIS cropveg trend as the dependent variable.
'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
exclusive categories compared against a category where there is no match
in ideology. 'Pop-Guer' and 'Pop-State' Exposure variables are a proportion of
the encounters that the population has with these groups during conflict."


cropveg_table6 <- htmlreg(cropveg.aid.popexpo,
                          custom.note = note,
                          digits = 2,
                          leading.zero = TRUE,
                          # better for markdown
                          doctype = FALSE,
                          html.tag = FALSE,
                          head.tag = FALSE,
                          body.tag = FALSE,
                          # passed to extract() method for "lm"
                          include.adjr = TRUE,
                          include.rsquared = FALSE,
                          include.rmse = FALSE,
                          include.nobs = TRUE,
                          custom.model.names = str_c("(", seq_along(cropveg.aid.popexpo), ")"),
                          custom.coef.names = coefnames,
                          caption.above = TRUE,
                          caption = "Aid's Effect on MODIS Crop Veg Cover (conditioned by population exposure)",
                          override.se = sandwich_se,
                          override.pvalues = p_values,
                          star.symbol = "*"
)

cropveg_table6 %>% HTML() %>% browsable()

# outlier table
cropveg_table6_out <- htmlreg(cropveg.aid.popexpo.out,
                              custom.note = note,
                              digits = 2,
                              leading.zero = TRUE,
                              # better for markdown
                              doctype = FALSE,
                              html.tag = FALSE,
                              head.tag = FALSE,
                              body.tag = FALSE,
                              # passed to extract() method for "lm"
                              include.adjr = TRUE,
                              include.rsquared = FALSE,
                              include.rmse = FALSE,
                              include.nobs = TRUE,
                              custom.model.names = str_c("(", seq_along(cropveg.aid.popexpo.out), ")"),
                              custom.coef.names = coefnames,
                              caption.above = TRUE,
                              caption = "Aid's Effect on MODIS Crop Veg Cover (w/out Uribia)",
                              override.se = sandwich_se_out,
                              override.pvalues = p_values_out,
                              star.symbol = "*"
)
cropveg_table6_out %>% HTML() %>% browsable()



## interaction plots -----------------------------------------------------------

# aid*coca
interplot(m = cropveg.aid.popexpo[[1]], var1 = "muni_locd", var2 = "vio_32_mean") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Muni Population's Exposure to Guerilla Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Population Exposure (2002-2013)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_guer.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = cropveg.aid.popexpo[[2]], var1 = "muni_locd", var2 = "vio_31_mean") +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Muni Population's Exposure to Government Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Population Exposure (2002-2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_govt.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   VII. CROPSHARE (crop cover): REPEAT ABOVE    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   AID: POL MATCH    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

# pres
# senado
# cam


## TARGETED FOR AID: 201705 analysis
# logit.aid <- glm(muni_locd ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
#                    road_zero + illegal_crops + mean_turnout + mean_mov + mean_muni_dept_match +
#                    local_parliament_match_mean, family = binomial(link='logit'), data = agaid)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   AID: POL IDEO    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   AID: POL    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 



# =-=-=-=-=-=-=-=-=-=-=-=-= #
####  CROPSHARE % CHG    ####
# =-=-=-=-=-=-=-=-=-=-=-=-= #
ols.cropsha <- lm(trendcrop ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
                    road_zero + illegal_crops + muni_locd + mean_turnout + mean_mov + mean_muni_dept_match +
                    local_parliament_match_mean, data = agaid)
summary(ols.cropsha)
summary(ols.cropsha, robust = T)









## TABLE HELP ####
## LOGIT RESULTS TO EXCEL
# setwd("/Users/wtmatthias/Google Drive/Research/colombia_pb_pleb/data/03_data_viz/")
# results_df <-summary.glm(logit11)$coefficients
# results_df <- as.data.frame(results_df)
# write_csv(results_df, "general_effects.csv")
# summary(logit11)
# 
# ## ODDS RATIOS TO EXCEL
# #  change "logit[]" to fit the logit model you need
# odds_ratios <- as.data.frame(exp(cbind(coef(logit11), confint(logit11))))
# write_csv(odds_ratios, "odds_ratios.csv")

#-------------------------------------------------------------------------------
# #### Marginal effect of income with respect to age
# # Prediction
# all.pairs <- expand.grid(seq(min(eac$vio_13), max(eac$vio_13), by=0.05),
#                          range(eac$muni_locd))
# colnames(all.pairs) <- c('vio_13', 'muni_locd')
# pred.data.me <- data.frame(muni_locd = all.pairs[2],
#                            vio_13 = all.pairs[1]
#                            )
# pred.data.me$id <- row.names(pred.data.me)
# 
# 
# # Calculate Marginal effect of Coca Presence
# pred.data.me$me <- cropveg.aid.coca1[[4]]$coefficients[['vio_13']] + pred.data.me$muni_locd * cropveg.aid.coca1[[4]]$coefficients[['muni_locd:vio_13']]

# # Calculate CI of the marginal effect
# var.coca <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['vio_13','vio_13']
# var.inter <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['muni_locd:vio_13','muni_locd:vio_13']
# cov <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['vio_13','muni_locd:vio_13']
# muni_locd <- pred.data.me$muni_locd
# pred.data.me$me.se <- sqrt(var.coca + var.inter*(muni_locd^2) + 2*muni_locd*cov)
# pred.data.me$me.upper <- pred.data.me$me + 1.96*pred.data.me$me.se
# pred.data.me$me.lower <- pred.data.me$me - 1.96*pred.data.me$me.se
# 
# # Reformat the data again
# pred.melt.me <- melt(pred.data.me, id='muni_locd',
#                      measure = c('me', 'me.upper', 'me.lower'))
# pred.melt.me$stat <- ifelse(pred.melt.me$variable == 'me',
#                             'P', 'CI')
# 
# # Plot
# ggplot(data = pred.melt.me, aes(y=muni_locd, x=vio_13, group=variable,
#                                 linetype=factor(stat))) +
#   geom_line(color='orange') +
#   scale_linetype_manual(breaks=c("CI","P"), values=c(2,1)) +
#   ylab('Marginal effect of  on Aid') +
#   guides(linetype=F)

#-------------------------------------------------------------------------------
