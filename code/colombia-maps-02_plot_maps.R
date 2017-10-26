#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
#
#  Author:		  Billy Matthias 
#  Email:		    bmatthias88@gmail.com
#  Purpose:		  ggplot/ggmap Colombia maps to enhance '02_analysis_eac.R'
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

#=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-#
    ##  SETUP ####
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
# load data from here
dir.outdata <- file.path("./01_outputdata")
# send maps here
dir.maps <- file.path("./02_tables_and_figures/maps")


## Load Data -------------------------------------------------------------------

## cross-sectional data w/ spatial polygon coords
# Note: 
#       - not a 'Spatial Polygons Data Frame" though b/c this doesn't work w/
#         ggplot/ggmap
load(file = file.path(dir.outdata,
                      "maps_cross-section_and_polygons.RData")
)

# aid locations data w/ point coordinates for geom_point
load(file = file.path(dir.outdata,
                      "maps_plot_aid_points.RData")
)


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
