#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
#
#  Author:		  Billy Matthias 
#  Email:		    bmatthias88@gmail.com
#  Purpose:		  wrangle spatial & non-spatial data for Colombia maps w/ aid,
#               coca, conflict measures, and other crops
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  SETUP  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

rm(list=ls())  # removes all objects from environment

library(ggplot2)
library(sp)
library(ggmap)  # issue w/ ggproto in ggmap 15-Jan-2017. To fix--> devtools::install_github("dkahle/ggmap")
library(rgdal)
library(rgeos)
library(maptools)
library(tmap)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(hexbin)


## Set file paths --------------------------------------------------------------
# root dir for the gis data
dir.gis <- file.path("./00_rawdata/gis_colombia/wfpco_admin2")
# dir for shp files

dir.agaid <- file.path("./00_rawdata/wb_ag_aid")
dir.maps <- file.path("./02_tables_and_figures/maps")
dir.outdata <- file.path("./01_outputdata")


## Load Data -------------------------------------------------------------------

# Cross Sectional Data
load(file = file.path(dir.outdata,
                      "eac_vio_muni_cross-section.RData")
)

eac <- aid_merged
rm(aid_merged)

# Agricultural Aid - precision code 1 
aid_locs1 <- read_csv(file.path(dir.agaid,
                                "00_colombia_ag_locs1_counts.csv")
                      )
# Agricultural Aid - precision code 3
aid_locs3 <- read_csv(file.path(dir.agaid,
                                "00_colombia_ag_locs3_counts.csv")
                      )

# prec code 1 & 3 merged but w/out lat & long coordinates
aid_locs <- read_csv(file = file.path(dir.agaid, "colombia_locs1-3_merged.csv"))



## Setup .shp File and Layer Data ----------------------------------------------

## LOAD LAYER DATA
# name of the .shp file + its associated files (.dbf, .prj, etc.)
dir.shp <- file.path(dir.gis,
                     "./simplified_and_smoothed")
layer_name <- "wfpco_simplified_smoothed"

# spdf = Spatial Polygons Data Frame
spdf <- readOGR(dsn = dir.shp, layer = layer_name, stringsAsFactors = FALSE)

# look at structure of data in layer files
# vars need to be factors
str(spdf@data)
summary(spdf@data)


## PROJ4 INFO
projInfo(type = "datum")
getPROJ4VersionInfo()
getPROJ4libPath()
projNAD()


## SET CRS (PROJECTION)
new_projection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
spdf <- spTransform(spdf, new_projection)
summary(spdf)
class(spdf)
# Note: when you set to WGS84 the projection will still be FALSE; ok if only
#       plotting polygons, lines, or points on a coordinate plane (like ggmap)
is.projected(spdf)


## PLOTTING GENERAL COUNTRY OUTLINE
plot(spdf)




#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  SPATIAL POLY DF - MERGE CO. ATTRIBUTES  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

## Check for Duplicates --------------------------------------------------------
# Note: - need unique values for this merge/join


## DUPLICATES IN spdf@data?
# extract data.frame only from SPDF
df_only <- spdf@data

# check for duplicates
df_only$CODANE <- as.numeric(df_only$CODANE)
df_only %>% dplyr::select(CODANE) %>% n_distinct()
# length(unique(df_only$CODANE)) == nrow(df_only)  # TRUE = there are NO duplicates
# anyDuplicated(df_only$CODANE) # will give the row number of one instance of duplication in the data frame

## where are the duplicates? - if there are any
# n_occur a df that is a giant frequency table
#     - sort on column 'Freq' to check the merge ID for duplicates
# n_occur <- data.frame(table(df_only$CODANE))
# n_occur[n_occur$Freq > 1,]
# df_only[df_only$CODANE %in% n_occur$Var1[n_occur$Freq > 1],]  # open up the data and sort on the side to look at which merge values have duplicates. If there are duplicates you'll have to fix this


## DUPLICATES IN OUTSIDE DATA (that you will merge)
eac %>% dplyr::select(codmuni) %>% n_distinct()
# anyDuplicated(eac)
# n_occur2 <- data.frame(table(eac$codmuni))
# n_occur2[n_occur2$Freq > 1,]
# eac[eac$codmuni %in% n_occur2$Var1[n_occur2$Freq > 1],]


## Check Class of Vars Before Merge --------------------------------------------
str(spdf$CODANE)
spdf@data$CODANE <- as.numeric(spdf@data$CODANE)

str(eac$codmuni)


## Merge Spatial Polygon DF w/ Attributes -------------------------------------
spdf_eac <- sp::merge(spdf, eac,
                      by.x = "CODANE", by.y = "codmuni", all.x=TRUE)
spdf_eac@data <- spdf_eac@data %>% rename(codmuni = CODANE)


#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  SPATIAL POLY DF - CONVERT TO REGULAR DF  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

# ggplot/ggmap cannot work w/ Spatial Points Data Frames
# need to pull polygons out


## CONVERT SpatialPolygonsDataframe INTO REGULAR DATA FRAME
spdf_eac@data$id <- rownames(spdf_eac@data)  # unique ID for later join
# require rgeos library to use 'fortify'
spdf_eac_poly <- fortify(spdf_eac, region="id")  #this df only has the polygon coordinates
spdf_eac_df <- left_join(spdf_eac_poly, spdf_eac@data, by="id")  # add the attributes back in
str(spdf_eac_df)
str(spdf_eac_poly)


## TEST PLOT
ggplot(spdf_eac_df, aes(long, lat, group=group)) +
  geom_polygon()


## GET COLOMBIA GOOGLE MAP
# bbox(spdf_eac)
# long <- c(-81.72966, -66.85859)  # need bbox long & lat coords for get_map Google Maps
# lat <- c(-4.20396, 13.37115)
# calc_zoom(long, lat, spdf_eac_df)  # gives estimate of zoom level for get_map

## color map of Colombia
# map_base <- get_map(location = 'Colombia', source = 'google', zoom = 6,
#                    maptype = "terrain")
# ggmap(map_base)

## b/w map of Colombia
# map_base_bw <- get_map(location = c(lon = mean(long), lat = mean(lat)),
#                       source = 'google', zoom = 6,
#                       maptype = "terrain", color = 'bw')




#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  SPATIAL POLY DF - VARS TO IMPROVE VISUALIZATION  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

## Create categories for pos & neg cropveg trend -------------------------------
# p_value_cv = cropveg trend p-value for each muni
# trendcropveg_cat = Significant Positive, Significant Negative, No Trend
spdf_eac_df %<>%
  group_by(codmuni
  ) %>% 
  mutate(
    pvalue_cv = 2 * pt(-abs(tstat_cv), 10), 
    trendcropveg_cat = case_when(
      trendcropveg > 0 & pvalue_cv > 0.05 ~ "Positive Trend",
      trendcropveg < 0 & pvalue_cv > 0.05 ~ "Negative Trend",
      TRUE ~ "No Trend"
    )
  ) %>%
  dplyr::select(codmuni, municipality, department,
                trendcropveg, sterr_cv, tstat_cv,
                pvalue_cv, trendcropveg_cat,
                everything()
  )

spdf_eac_df$trendcropveg_cat <- as.factor(spdf_eac_df$trendcropveg_cat)


## Save cross-section data w/ polygons -----------------------------------------
save(spdf_eac_df,
     file = file.path(dir.outdata, "maps_cross-section_and_polygons.RData"))




#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  AID LOCATION POINTS  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

## Bind Prec Code 1 & 3 DFs ----------------------------------------------------

# make sure vars are the same name and in the same order
str(aid_locs1, give.attr = FALSE)
str(aid_locs3, give.attr = FALSE)


# bind rows together
aid_locs13 <- bind_rows(aid_locs1, aid_locs3)

# need separate var name for prec code 1 & 3 project counts
aid_locs13 <- aid_locs13 %>% rename(locs3_n = project_n)



## Create var - split aid amts -------------------------------------------------

# Note:
#         - This is in the original WB data but wasn't included in this
#           division of data. Easier to just create here than going back. Named
#           with same var so I can go back later.

# 'numbloc' is a var from 'WorldBank_GeocodedResearchRelease_Level1_v1.4.1'
# count # of locations per project
aid_locs13 %<>%
  add_count(project_id) %>%
  rename(numbloc = n)

aid_locs13 %<>%
  group_by(project_id) %>% 
  mutate(even_split_disbursements = total_disbursements/numbloc)


## Create var - dept codes for grouping aid ------------------------------------

# regex out the dept code from 'gazetteer_adm_code' 
aid_locs13 %<>%
  ungroup(
  ) %>%
  mutate(
    coddept = str_extract(gazetteer_adm_code,
                          "(?<=\\|)([0-9]{1}[0-9]{1})(?=\\|)")
  ) %>%
  dplyr::select(coddept, everything())



## Collapsing aid amounts on each location -------------------------------------
# usually collapses on municipality

aid_by_muni <- aid_locs13 %>% 
  group_by(coddept, place_name
  ) %>%
  summarise(
    aid_muni_total = sum(even_split_disbursements),
    first(latitude),
    first(longitude),
    first(geoname_id),
    first(transactions_start_year),
    first(transactions_end_year)
  )

# aid in millions of $USD
aid_by_muni$aid_muni_total <- aid_by_muni$aid_muni_total/1000000



## Make aid categories for plotting --------------------------------------------
# try a few different ways to break into categories

summary(aid_by_muni$aid_muni_total)

## JENKS NATURAL BREAKS ALGORITHM (used a lot for mapping)
library(classInt)
jenks_aid <- classIntervals(aid_by_muni$aid_muni_total, n = 5, style = 'jenks')
                          # n = "however many breaks you want in the data"
jenks_aid$brks

# Note:
#       - the extreme end points in 'fixed' need to be *just* smaller and *just* larger than the jenks #s output
#         if you want the smallest and largest categories included in the factor var
#       - if you do not do this the new var, 'fixedinterval', will have NA values
labels_aid <- c("0.212 - 0.386", "0.386 - 0.984",
                 "0.984 - 1.839", "1.839 - 4.508",
                 "4.508 - 5.107")
aid_by_muni$aid_intervals <- cut(aid_by_muni$aid_muni_total,
                                       breaks = jenks_aid$brks,
                                       labels = labels_aid,
                                       include.lowest = TRUE,
                                       ordered_result = TRUE)

str(aid_by_muni$aid_intervals)

# Rename lat and lon (x-y coord) vars after collapse
aid_points <- aid_by_muni %>%
  rename(long = `first(longitude)`,
         lat = `first(latitude)`,
         geoname_id = `first(geoname_id)`,
         transactions_start_year = `first(transactions_start_year)`,
         transactions_end_year = `first(transactions_end_year)`
         )



## Save aid points data --------------------------------------------------------
save(aid_points,
     file = file.path(dir.outdata, "maps_plot_aid_points.RData"))




#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  MAPS - POSITIVE CROPVEG TREND AND AID AMTS  ####
#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#

## Plot aid points w/ muni choropleths of trend --------------------------------
library(viridis)
summary(spdf_eac_df$trendcropveg_cat)

## SPECIFIC TO THIS MAP
# geom_polygon args
polygon_data <- spdf_eac_df
# polygon_fill <- trendcropveg_cat

# label args
lab_title <- "Crop Trends (2001-2012) & Aid Amounts (1998-2015) to Colombia"
lab_caption <- "*Trend categories are sig. @ p-value <= 0.05. Sources: WFPGeoNode. AidData. QGIS. ggplot2. ggmap."

# scale_fill_manual
name_scale_fill_manual <- "Trend Direction"

# geom_points args
point_data <- aid_points
# point_size <- aid_intervals

# scale_size_manual
name_scale_size_manual <- "Aid Amounts (Millions of $USD)"

## Function for a very minimal looking map w/ nice background (this may help troubleshooting... https://github.com/grssnbchr/thematic-maps-ggplot2/blob/master/index.md)
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#000004"),  # change text color to your desired; this color = an almost black 
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),  # these colors give a kind of sandy-gray color; looks pretty nice on white background
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


ggplot() +
  geom_polygon(
    data = polygon_data,
    aes(x = long, y = lat, group = id,
        fill = trendcropveg_cat,  # 'fill' munis w/ cropveg cats
        alpha = 0.8),  # alpha sets transparency of the mapped polygons
    color = 'white', size = 0.1  # white admin unit borders
  ) +
  labs(x = NULL, y = NULL,
       title = lab_title,  # map title
       caption = lab_caption  # caption on bottom of map
  ) +
  theme_map( # calling the 'theme_map' function
  ) +
  theme(
    legend.spacing = unit(3, "mm"),  # trial and error. PITA to set. Google help
    legend.position = c(0.2, .2),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.text = element_text(size = 6, color = "#000004", vjust = 0.5, hjust = 0),
    plot.title = element_text(hjust = 0.5, color = "#000004", face = 'bold'),
    legend.title = element_text(size = 8),
    plot.margin = grid::unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")  # caption text color
  ) +
  scale_fill_manual( # setting the values that will be filled (i.e. the color of the admin units)
    values = c("Negative Trend" = "#FEC98DCC",
               "Positive Trend" = "#CD4071CC",
               "No Trend" = "#3E4A89CC"),
    breaks = c("Negative Trend",
               "Positive Trend",
               "No Trend"),
    limits = c("Negative Trend",
               "Positive Trend",
               "No Trend"),
    name = name_scale_fill_manual, # set key title above
    drop = TRUE,
    labels = c("Negative", # the labels in the map legend
               "Positive",
               "No Trend"),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(60/3, units = "mm"),
      title.position = 'bottom',
      title.hjust = 0.5,
      label.hjust = 0.5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )
  ) +
  geom_point(                 
    data = point_data,
    aes(x = long, y = lat,
        size = aid_intervals  # factor/intervals for point sizes
    ),
    alpha = 0.7,
    color = "#440154"
  ) +
  scale_size_manual(   # options for geom_point size (also color, shape, etc.)
    name = name_scale_size_manual,
    values = c("0.212 - 0.386" = 1, # setting point size
               "0.386 - 0.984" = 2,
               "0.984 - 1.839" = 4,
               "1.839 - 4.508" = 6,
               "4.508 - 5.107" = 8),
    breaks = c("0.212 - 0.386",
               "0.386 - 0.984",
               "0.984 - 1.839",
               "1.839 - 4.508",
               "4.508 - 5.107"),
    limits = c("0.212 - 0.386",
               "0.386 - 0.984",
               "0.984 - 1.839",
               "1.839 - 4.508",
               "4.508 - 5.107"),
    na.translate = FALSE,
    drop = TRUE,
    guide = guide_legend(
      direction = "vertical",
      keyheight = unit(35/5, units = "mm"),
      keywidth = unit(2, units = "mm"),
      title.position = 'bottom',
      title.hjust = 0.5,
      label.hjust = 0,
      byrow = T,
      reverse = F
    )
  ) +
  guides(alpha = FALSE
  ) + # alpha = FALSE so the alpha values don't map as legend (e.g. this case, a gray box with "0.8" as a label)
  coord_equal() # keep the aspect ratio proportional

ggsave(file.path(dir.maps,
                 "cropveg-trend_aid_map.pdf"),
       plot = last_plot(), device = "pdf", width = 6, height = 7,
       units = "in", dpi = 300)









## CATEGORIES OF AID AMOUNTS FOR CHOROLPLETH
## Note:
#       - categories needed to create the 'bins' for choropleth

## NATURAL BREAKS JENKS
# jenks2 <- classIntervals(spdf_df$even_split_disbursements[spdf_df$even_split_disbursements != 0], n=4, style='jenks')
# jenks2$brks # output from jenks breaks: 0.2209453  12.4990268  92.8288778 215.7077105 583.5995286
# 
# fixed_jenks <- c(0, 0.2209453, 12.4990268, 92.8288778, 215.7077105, 583.59953) # the intervals/breaks include a "0" base cat. if the "0" values are impt by themselves
# lab.fix_jenks <- c("0", ".2 - 12.5", "12.5 - 92.8", "92.8 - 215.7", "215.7 - 583.6") # labels for the categories (factor variables)
# spdf_df$fixedint_jenks <- cut(spdf_df$even_split_disbursements,                    # labels above are what will appear in map legend
#                               breaks = fixed_jenks,
#                               labels = lab.fix_jenks,
#                               include.lowest = TRUE, right = TRUE,
#                               ordered_result = TRUE)
# str(spdf_df$fixedint_jenks)
# 
# spdf_df <- spdf_df %>% dplyr::select(long, lat, even_split_disbursements, fixedint_jenks, illegal_crops, majority_yes, everything())
# your_data <- your_data %>% dplyr::select(codmuni, even_split_disbursements, everything())
# 
# 


##  CHOROPLETH & GOOGLE MAPS - AID AMOUNTS BY MUNI
# ggmap(basemap, extent = 'device') +
#   geom_polygon(data = dept_df, aes(x = long, y = lat, group=id), alpha = .1, color='white', size = .3) +
#   theme(line = element_blank(),
#         axis.ticks.y = element_blank(), axis.text.y = element_blank(), # get rid of x ticks/text
#         axis.ticks.x = element_blank(), axis.text.x = element_blank(), # get rid of y ticks/text
#         panel.background = element_blank())
# 
# getwd()
# setwd(shp_file) # SET WD TO FILE PATH WHERE YOU WANT THE IMAGE!
# ggsave("col_map_deptlevel.pdf", plot = last_plot(), device = "pdf", width = 7, height = 7, units = "in", dpi = 300)















