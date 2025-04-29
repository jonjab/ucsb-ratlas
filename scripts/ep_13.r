#episode 13
#create publication quality graphics

# and explaining why our NDVIs look the way they do

# clean the environment and hidden objects
rm(list=ls())

current_episode <- 13

library(terra)
library(ggplot2)
library(dplyr)
library(sf)
library(raster)
library(scales)

# make our ggtitles automagically #######
# set ggplot counter
current_ggplot <- 0
gg_labelmaker <- function(plot_num){
  gg_title <- c("Episode:", current_episode, " ggplot:", plot_num)
  plot_text <- paste(gg_title, collapse=" " )
  print(plot_text)
  current_ggplot <<- plot_num
  return(plot_text)
}
# every ggtitle should be:
# ggtitle(gg_labelmaker(current_ggplot+1))
# end automagic ggtitle           #######




# recreate what you had at the end of episode 12
# ###################
# depends on running map 12
ndvi_series_path <- list.files("output_data/ndvi", full.names = TRUE)

# build a raster stack
ndvi_series_stack <- rast(ndvi_series_path)

#   crop the stack to the NCOS area to make a 
#   more relevant map for campus.
#   and make plotting faster
ncos_extent <- vect("source_data/planet/planet/ncos_aoi.geojson")
ncos_extent <- project(ncos_extent, ndvi_series_stack)
ndvi_series_stack <- crop(ndvi_series_stack, ncos_extent)
plot(ndvi_series_stack)

# convert to a data frame
ndvi_series_df <- as.data.frame(ndvi_series_stack, xy=TRUE, na.rm=FALSE) %>% 
  pivot_longer(-(x:y), names_to = "date", values_to= "NDVI")
str(ndvi_series_df)



# Before and after
#################

# https://datacarpentry.github.io/r-raster-vector-geospatial/13-plot-time-series-rasters-in-r.html#before-and-after

# this is the before plot at the top of the episode
ggplot() +
  geom_raster(data = ndvi_series_df , aes(x = x, y = y, fill = NDVI)) +
  ggtitle(gg_labelmaker(current_ggplot+1)) +
  facet_wrap(~ date)

# iterate through gradual changes

# Adjust the Plot Theme
#######################

# I want to use captions here, so I have to use
# labs instead of ggtitle.
ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  labs("NCOS NDVI", 
          subtitle = gg_labelmaker(current_ggplot+1),
          caption = "What's going on here?") + 
  theme_void() +
  coord_sf()

# center the title
ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  labs("NCOS NDVI", 
       subtitle = gg_labelmaker(current_ggplot+1),
       caption = "What's going on here?") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_sf()

# Challenge: Make the title bold
ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  labs("NCOS NDVI", 
       subtitle = gg_labelmaker(current_ggplot+1),
       caption = "What's going on here?") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_sf()

# Adjust the color ramp
#######################

# lesson uses yellow to green like this:
library(RColorBrewer)
brewer.pal(9, "YlGn")
green_colors <- brewer.pal(9, "YlGn") %>%
  colorRampPalette()

ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  scale_fill_gradientn(name = "NDVI", colours = green_colors(20)) +
  labs("NCOS NDVI", 
       subtitle = gg_labelmaker(current_ggplot+1),
       caption = "New yellow to green scheme") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_sf()



# Refine Plot & Tile Labels
###########################

# we have to do something about the names on the individual chips.
# the lesson uses gsub
names(ndvi_series_stack)
raster_names <- names(ndvi_series_stack)

# convert raster_names to mm yyyy
raster_names <- substr(raster_names, 3, 10)
raster_names
ndvi_series_stack <- setNames(ndvi_series_stack, raster_names)

ndvi_series_df <- as.data.frame(ndvi_series_stack, xy=TRUE, na.rm=FALSE) %>% 
  pivot_longer(-(x:y), names_to = "date", values_to= "NDVI")
str(ndvi_series_df)

# this really changes the character of the plot
# images from the same date get mushed together
ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  labs("NCOS NDVI", 
       subtitle = gg_labelmaker(current_ggplot+1),
       caption = "New labels on the chips") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_sf()




# Change Layout of Panels
########################

# Challenge: Divergent Color Ramps
##################################

# we already have one. 
# I like Red to Yellow to Green like this:
ggplot() +
  geom_raster(data = ndvi_series_df, aes(x = x, y = y, fill = NDVI)) +
  facet_wrap(~ date) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  labs("NCOS NDVI", 
       subtitle = gg_labelmaker(current_ggplot+1),
       caption = "New divergent red-yellow-green color scheme") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_sf()


# Now we talk about the weather
# An episode ep_weather
# to be plugged in post-intermediate R

# clean the environment and hidden objects
rm(list=ls())

current_episode <- 13.5


# Sigrid is going to walk us towards weather stations.

library(tidyverse)
library(tictoc)

download.file (
  url = "https://www.ncei.noaa.gov/data/ghcnm/v4/precipitation/doc/ghcn-m_v4_prcp_inventory.txt",
  destfile = "downloaded_data/rain.txt"
)

station_inventory <- read_fwf(
  file = "downloaded_data/rain.txt") 

colnames(station_inventory) <- c("stationId", "lat", "long", "elevation", "state", "stationName", "wmold", "firstYear", "lastYear")
str(station_inventory)

# add a column
station_inventory$country=""

# time our code
tic()
for (i in 1:nrow(station_inventory)) {
  station_inventory[i,"country"] = substring(station_inventory[i,"stationId"],1,2) 
}
toc()

# I would think there's a tidy solution
station_inventory$countryTidy=""
cbind(station_inventory, substring(station_inventory$countryTidy, 1,2))
station_inventory






current_episode <- "episode 13 end"

