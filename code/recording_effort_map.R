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
omit_project <- c('OPP-MEQ Coastal Monitoring')

# stations to leave out
omit_station <- c('ROBE', 'SABV', 'SBSM', 'GLNE', 'GLSW')

# output figure sized for docx (1) or pptx (2)
map_size = 2

# optional area layers to include

# OPTIONS:
  # narw_ch - north atlantic right whale critical habitat areas
  # nbw_ch - northern bottlenose whale critical habitat areas
  # mpa - marine protected areas
  # aoi - fundian channel browns bank aoi (based on currently available shapefile)
  # refuge - maritimes region marine refuges from 'other effective area-based conservation measures' shapefile
  # wea - designated wind energy areas
  # for none of above, specify as include_layers <- c('')

include_layers <- c('mpa', 'aoi', 'refuge')

#--------------------------------------

# Compile recording effort summary

# path to PAM metadata folder
metadata <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata'
#metadata <- 'C:/Users/STANISTREETJ/Documents/R-GitHub/pam-mapping/input_data'


# load deployment summary
depl <- read_csv(here(metadata, 'deployment_summary.csv'))

# load station table
stations <- read_csv(here(metadata, 'station_summary.csv')) %>%
  transmute(station = Code,
            Revision,
            latitude = Latitude,
            longitude = Longitude) %>%
  group_by(station) %>%
  top_n(1, Revision)

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
  ungroup () %>%
  filter(!station %in% omit_station)

# get current deployments
current_depl <- depl %>%
  filter(!Project %in% omit_project) %>%
  filter(Year == 2026, is.na(`In-water_end`)) %>%
  transmute(deployment = Deployment,
            station = stringr::str_extract(Station, '(.)+(?=:)'),
            latitude = Latitude,
            longitude = Longitude)


#--------------------------------------

# Import data layers and set up for mapping

# load bathymetry
bf <- readRDS('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/bathymetry/baleenwhale/bathymetry.RDS')

# path to shapefiles folder
shapefiles <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles'

# load land areas shapefile
north_america <- read_sf(here(shapefiles, 'coastline', 'north_america','north_america.shp')) %>%
  st_transform(crs = 4326)

# load optional shapefiles for use later if needed

narw_ch_sf <- read_sf(here(shapefiles, "SAR_CH", "NARW_CH", "NARW_CH.shp")) %>%
  st_transform(crs = 4326)

nbw_ch_sf <- read_sf(here(shapefiles, "SAR_CH", "NBW_CH", "NorthernBottlenoseWhale_CH.shp")) %>%
  st_transform(crs = 4326)

wea_sf <-read_sf(here(shapefiles, "Designated-WEA", "Designated_WEAs_25_07_29.shp")) %>%
  st_transform(crs = 4326)

mpa_sf <-read_sf(here(shapefiles, "ProtectedAreas", "DFO", "OA_MPAs", "EastCan_MPAs.shp")) %>%
  st_transform(crs = 4326) %>%
  st_make_valid() %>%
  group_by(NAME_E) %>%
  summarize(geometry = st_union(geometry))

aoi_sf <-read_sf(here(shapefiles, "ProtectedAreas", "DFO", "FundianAOI_OLD", "FundianChannel_BrownsBank_AOI_poly.shp")) %>%
  st_transform(crs = 4326)

refuge_sf <-read_sf(here(shapefiles, "ProtectedAreas", "DFO", "DFO_OECM_MPO_AMCEZ_SHP", "DFO_OECM_MPO_AMCEZ.shp")) %>%
  st_transform(crs = 4326) %>%
  filter(REGION_E == "Maritimes")

# set fill colours for optional layers
layer_fills <- c(
  "NARW Critical Habitat" = "darkred",
  "NBW Critical Habitat" = "#5B1166",
  "Marine Protected Area" = "darkgreen",
  "Area of Interest" = "lightgreen",
  "Wind Energy Area" = "darkblue",
  "Marine Refuge" = "#EFBF04"
)

# set up legend for current deployments

current_shape <- c("Current deployments" = 24)

#--------------------------------------

# Create map

theme_set(theme_bw())

m<-ggplot() +

  # add bathymetry
  geom_raster(data = bf, aes(x = x, y = y, fill = z)) +

  scale_fill_distiller(palette = "Blues", guide = 'none') +

  # add land region
  geom_sf(data = north_america,
          color = "grey40", fill = "grey60") +

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
               colour = "grey70") +

  new_scale_fill() +

  # add optional layers

  {
    if ('narw_ch' %in% include_layers) {
      geom_sf(data = narw_ch_sf,
              aes(fill = "NARW Critical Habitat"),
              colour="darkred", alpha=0.3, linewidth=0.1)
    }
  } +

  {
    if ('nbw_ch' %in% include_layers) {

    geom_sf(data = nbw_ch_sf,
            aes(fill = "NBW Critical Habitat"),
            colour="#5B1166", alpha=0.4, linewidth=0.1)
    }
  } +

  {
    if ('mpa' %in% include_layers) {

      geom_sf(data = mpa_sf,
              aes(fill = "Marine Protected Area"),
              colour="darkgreen", alpha=0.3, linewidth=0.1)
    }
  } +

  {
    if ('aoi' %in% include_layers) {

      geom_sf(data = aoi_sf,
              aes(fill = "Area of Interest"),
              colour="darkgreen", alpha=0.3, linewidth=0.1)
    }
  } +


  {
    if ('refuge' %in% include_layers) {

      geom_sf(data = refuge_sf,
              aes(fill = "Marine Refuge"),
              colour="#EFBF04", alpha=0.3, linewidth=0.1)
    }
  } +

  {
    if ('wea' %in% include_layers) {

      geom_sf(data = wea_sf,
              aes(fill = "Wind Energy Area"),
              colour="darkblue", alpha=0.3, linewidth=0.1)
    }
  } +

  scale_fill_manual(name = '', values = layer_fills) +

  # add recording effort
  geom_point(data = effort_summary %>%
               filter(rec_days>0),
             #aes(x = longitude, y = latitude, size = rec_days, fill = rec_days),
             aes(x = longitude, y = latitude, size = rec_days),
             fill = 'black',
             colour = 'black',
             alpha = 0.3,
             shape = 21) +

  scale_size('Recording days', range = c(1,9), breaks = c(500,1000,2000,4000), limits = c(0,4000),
             guide = guide_legend(order = 1)) +


  # scale_size('Recording days', range = c(2,9), breaks = c(0,500,2000,4000), limits = c(0,4000)) +
  #
  # scale_fill_gradientn('Recording days',
  #                      colours = c("white", "black", "black"),
  #                      values = c(0,0.00000001,4000), breaks = c(0,500,2000,4000),
  #                      limits = c(0,4000), guide = "legend") +

  scale_colour_manual(values = 'black') +

  # geom_point(data = effort_summary %>%
  #              filter(rec_days==0),
  #            aes(x = longitude, y = latitude),
  #            size = 2, colour = 'black', fill = 'white', alpha = 1, shape = 21) +

  # add current deployments
  geom_point(data = current_depl,
             aes(x = longitude, y = latitude, shape = "Current deployments"),
             size = 1.5,
             colour = 'black',
             fill = 'red',
             alpha = 1) +
             #shape = 24) +

  scale_shape_manual(name = '', values = current_shape, guide = guide_legend(order = 2)) +

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
        legend.key = element_rect(fill = NA),
        legend.spacing.y = unit(-5, "pt"))

if (map_size == 1) {

  ggsave(here('output_figures', paste0('PAM_effort_map_', Sys.Date(), '.png')), m, width = 6.5, height = 3.5, dpi = 300)

} else if (map_size == 2) {

  ggsave(here('output_figures', paste0('PAM_effort_map_', Sys.Date(), '.png')), m, width = 8, height = 4, dpi = 300)

}
