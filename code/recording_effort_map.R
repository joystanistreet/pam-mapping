#--------------------------------------

# Script name: recording_effort_map.R

# Purpose: Create map to visualize recording effort at DFO Maritimes PAM stations

  # NOTES:
  # Recording effort (number of days) is calculated based on dataset records in the PAM metadata database
  # Recording effort is summarized and mapped by station, ignoring station revisions

# Author: Joy Stanistreet

# Date created: 2024-11-15

#--------------------------------------

# Load packages

library(here)
library(tidyverse)
library(sf)
library(ggnewscale)
library(ggspatial)

#--------------------------------------

# Set options

# projects to leave out
omit_project <- c('OPP-MEQ Coastal Monitoring', 'Kenchington PAM Landers')

#--------------------------------------

# Compile recording effort summary

# path to PAM metadata folder
metadata <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata'

# load deployment summary
depl <- read_csv(here(metadata, 'deployment_summary.csv'))

# load station table
stations <- read_csv(here(metadata, 'mooring_stations.csv')) %>%
  transmute(station = Station,
         latitude = Lat,
         longitude = Lon)

# summarize recording effort by station
effort_summary <- depl %>%
  filter(!Project %in% omit_project) %>%
  mutate(station = stringr::str_extract(Station, '(.)+(?=:)')) %>%
  transmute(deployment = Deployment,
            station,
            rec_start = as_date(`In-water_start`),
            rec_end = as_date(`In-water_end`)) %>%
  mutate(n_days = rec_end - rec_start + 1) %>%
  left_join(stations) %>%
  group_by(station) %>%
  mutate(rec_days = as.numeric(sum(n_days, na.rm = T))) %>%
  transmute(station, latitude, longitude, rec_days) %>%
  distinct() %>%
  ungroup ()

#--------------------------------------

# Import data layers and set up for mapping

# load bathymetry
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

#--------------------------------------

# Create map

theme_set(theme_bw())

m<-ggplot() +

  # add bathymetry
  geom_raster(data = bf, aes(x = x, y = y, fill = z)) +

  scale_fill_distiller(palette = "Blues", guide = 'none') +

  # add land region
  geom_sf(data = north_america,
          color = NA, fill = "grey60") +

  # add contours
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200,-500),
               linewidth = c(0.3),
               colour = "grey75") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-1000,-2000,-3000,-4000,-5000),
               linewidth = c(0.3),
               colour = "grey65") +

  new_scale_fill() +

  # add recording effort
  geom_point(data = effort_summary,
             aes(x = longitude, y = latitude, size = rec_days, fill = rec_days),
             colour = "black",
             alpha = 0.4,
             shape = 21) +

  scale_size('Recording days', range = c(2,9), breaks = c(0,500,2000,4000), limits = c(0,4000)) +

  scale_fill_gradientn('Recording days',
                       colours = c("white", "black", "black"),
                       values = c(0,0.00000001,4000), breaks = c(0,500,2000,4000),
                       limits = c(0,4000), guide = "legend") +

  scale_colour_manual(values = 'black') +

  geom_point(data = effort_summary %>%
               filter(rec_days==0),
             aes(x = longitude, y = latitude),
             size = 2, colour = 'black', fill = 'white', alpha = 1, shape = 21) +

  # set map limits
  coord_sf(xlim = c(-71, -55), ylim = c(41, 48), expand = FALSE) +

  # add scale bar
  annotation_scale(location = "br",
                   width_hint = 0.25,
                   text_cex = 0.6,
                   style = 'ticks') +

  # format axes
  ylab("") +
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 10),
        legend.key = element_rect(fill = NA))

ggsave(here('output_figures', paste0('PAM_effort_map_', Sys.Date(), '.png')), m, width = 10, height = 6, dpi = 300)
