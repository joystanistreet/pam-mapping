
#--------------------------------------

# Script name: station_map.R

# Purpose: Create map of PAM stations based on stations table exported from DMApps database

# Author: Joy Stanistreet

# Date created: 2023-03-24

#--------------------------------------

# REQUIRED: SET PARAMETERS

# specify file name for output map (.png)
mapfile = "PAM_stations_2012-2025.png"

# select stations to include on map? (TRUE/FALSE)
select_stations = FALSE
## if TRUE, specify selected stations
station_select = c("CCU","COC","GBK","FCH","FCM", "FCD","EFC")

# select stations to omit from map? (TRUE/FALSE)
omit_stations = TRUE
## if TRUE, specify stations to OMIT
station_omit = c("GLNE","GLSW", "SAM", "SBVC1","SBVC2","SBVC3")

# add station labels?
station_labels = TRUE

#--------------------------------------

# Load packages

library(tidyverse)
library(here)
library(sf)
library(mapdata)
library(ggspatial)

#--------------------------------------

# Import data layers and set up for mapping

# load bathymetry
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

# path to PAM metadata folder
#metadata <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata'

# load station table
stations <- read_csv(here('input_data', 'station_summary.csv')) %>%
  transmute(station = Code,
            latitude = Latitude,
            longitude = Longitude)

# select stations, if needed
if(select_stations == TRUE){

  stations <- stations %>%
    filter(station %in% station_select)

}

# omit stations, if needed
if(omit_stations == TRUE){

  stations <- stations %>%
    filter(!station %in% station_omit)

}

#--------------------------------------

# create map

theme_set(theme_bw())

pam_map <-ggplot() +

  # add bathymetry
  geom_raster(data = bf, aes(x = x, y = y, fill = z)) +

  scale_fill_distiller(palette = "Blues", guide = 'none') +

  # add land region
  geom_sf(data = north_america,
          color = NA, fill = "grey60") +

  # add contours (200m, 500m)
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200,-500),
               size = 0.3,
               colour = "grey80") +

  # add contours (1000m, 2000m, 3000m, 4000m, 5000m)
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-1000,-2000,-3000,-4000,-5000),
               linewidth = 0.3,
               colour = "grey70") +

  # add recording sites
  geom_point(data = stations, aes(x = longitude, y = latitude),
             colour = "black", shape = 17, size = 1.5) +

  # set map limits
  coord_sf(xlim = c(-69, -55), ylim = c(40, 49), expand = FALSE) +

  # format axes
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9),
        legend.key = element_rect(fill = NA),
        plot.margin = margin(0.2,0.1,0.2,0.1,"cm"))

# add labels if needed
if(station_labels == TRUE){

  pam_map <- pam_map + geom_text(data = stations,
                                  aes(x = longitude,
                                      y = latitude,
                                      label = station),
                                      hjust = "left",
                                      nudge_x = 0.1,
                                      size = 2,
                                 color = 'darkred')
}

ggsave(here('output_figures', mapfile), pam_map, width = 6.5, height = 6, dpi = 600)
