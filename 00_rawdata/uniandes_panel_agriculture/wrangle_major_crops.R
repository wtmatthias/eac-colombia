
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    ####    SETUP   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
library(tidyverse)

# relative path for input & output data in this script
dir.uniandes <- file.path(
  "00_rawdata/uniandes_panel_agriculture")

# load data
crops <- read_dta(file = file.path(
            dir.uniandes,
            "PANEL_AGRICULTURA_Y_TIERRA.dta")
            )

crops_varnames <- as_tibble(variable.names(crops))

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    ####    PANEL DATA VERSION OF NEEDED VARS   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #


# keep only needed variables
crops1 <- crops %>%  select(
  codmuni = codmpio,
  year = ano,
  ends_with("_cafe"),
  ends_with("_banano"),
  ends_with("_bananito"),
  ends_with("_mango"),
  ends_with("_cacao"),
  ends_with("_canaa"),
  ends_with("_palmaa"),
  contains("_arroz"),
  contains("_maiz"),
  contains("_flores"),
  ends_with("_coco"),
  ends_with("_platano"),
  ends_with("_soya"),
  ends_with("_ajonjoli")
) %>% 
  filter(year %in% c(2007:2014))

# year as.integer so it matches the large colombia panel data 
crops1$year <- as.integer(crops1$year)


## COFFEE DUMMY VAR
crops1 %<>%
  mutate(
    coffee_d = case_when(  # create for propotion of coffee pres var in cs data
                ac_cafe == 0 ~ 0, 
                ac_cafe > 0 ~ 1,
                TRUE ~ NA_real_),
    coffee_2014 = case_when( # single yr dummy var for cs data (last yr avail.)
      year == 2014 & (ac_cafe == 0 | is.na(ac_cafe)) ~ 0,
      year == 2014 & ac_cafe > 0 ~ 1,
      TRUE ~ NA_real_)
    )


# take out coffee var to merge back in after summarizing other vars
coffee_var <- crops1 %>%
  filter(year == 2014) %>%
  select(codmuni, year, coffee_2014)

## PANEL DATA
save(crops1, file = file.path(dir.uniandes, "crops_panel_colombia.RData"))



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
    ####    CS DATA OF NEEDED VARS   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #

## SUMMARIZE DATA
crops_cs <- crops1 %>%
  select(-year, -coffee_2014) %>% 
  group_by(codmuni) %>%
  summarise_all(., funs(mean), na.rm = TRUE)

# replace NaN w/ NA
crops_cs %<>% map_dfc(~ ifelse(is.nan(.x), NA_real_, .x))

# coffee dummy
crops_cs %<>%
  rename(coffee_avg = coffee_d) %>%
  mutate(
    coffee_d = case_when(
                coffee_avg == 0 ~ 0,
                ac_cafe > 0 ~ 1,
                TRUE ~ NA_real_)
  )

# there isn't much variation in coffee producing/non-producing w/in muni
crops_cs %>% select(coffee_avg, codmuni) %>% count(coffee_avg)

crops_cs <- left_join(crops_cs, coffee_var, by='codmuni')

crops_cs %<>% select(-year)


## SAVE CS DATA
save(crops_cs, file = file.path(dir.uniandes, "crops_cs_colombia.RData"))

