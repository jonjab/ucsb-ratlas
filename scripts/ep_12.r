# episode 12: time series rasters
library(terra)
library(scales)
library(tidyverse)
library(ggplot2)
library(sf)

# clean the environment and hidden objects
rm(list=ls())

current_episode <- 12


getwd()


#### create the little tiffs
#    that get made in map12.r
#    but abbreviated like

# load the 2023-24 8-band rasters
# loop over the files and build a raster stack

# get a file list
scene_paths <- list.files("source_data/planet/planet/20232024_UCSB_campus_PlanetScope/PSScene/",
                          full.names = TRUE,
                          pattern = "8b_clip.tif")
scene_paths
campus_crs <- rast("source_data/campus_DEM.tif") %>% crs()

ucsb_extent <- project(x=ucsb_extent, y=campus_crs)
crs(ucsb_extent)

# someplace to put our images
dir.create("output_data/ndvi", showWarnings = FALSE)

# calculate the NDVIs and fill in (extend) to the AOI
# loop
# this takes a while
for (images in scene_paths) {
  source_image <- rast(images)
  source_image <- aggregate(source_image, fact = 4)
  ndvi_tiff <- ((source_image[[8]] - source_image[[6]]) / (source_image[[8]] + source_image[[6]]))
  new_filename <- (substr(images, 67,90))
  new_path <- paste("output_data/ndvi/", new_filename, ".tif", sep="")
  ndvi_tiff <- extend(ndvi_tiff, ucsb_extent, fill=NA, snap="near")
  set.ext(ndvi_tiff, ext(ucsb_extent))
  names(ndvi_tiff) <- substr(new_filename, 0,13)
  print(names(ndvi_tiff))
  print(new_filename)
  print(dim(ndvi_tiff))
  writeRaster(ndvi_tiff, new_path, overwrite=TRUE)
}

# 3 or 4 of the resulting tiffs are wonky
# their dimensions are wildly off.
# but almost all of them are 554 x 885 pixels
# let's get rid of the ones that aren't:
# # get a list of the new files:
ndvi_series_names <- list.files("output_data/ndvi")
ndvi_series_names

testraster_path <- paste("output_data/ndvi/", ndvi_series_names[1], sep="")

testraster <- rast(testraster_path)


# check the files's resolutions and 
# keep only the
# 554 x 885 now that we are downsampled
length(ndvi_series_names)
str(ndvi_series_names)
valid_tiff <- c(554,885,1)
str(valid_tiff)


# delete any files that aren't the standard 
# resolution
for (image in ndvi_series_names) {
  test_size <- rast(paste("output_data/ndvi/", image, sep = ""))
  # length 1 qualifier 
  test_result <- (dim(test_size) == valid_tiff)
  print(test_result)  
  ifelse((dim(test_size) == valid_tiff), print("A match!!!"), file.remove(paste("output_data/ndvi/", image, sep = "")))
}

# reload the names
ndvi_series_names <- list.files("output_data/ndvi")
ndvi_series_paths <- paste("output_data/ndvi/", ndvi_series_names, sep="")
ndvi_series_paths

# now we can see there are 4 fewer tiffs.
length(ndvi_series_names)

#####
## end little tiffs. 








# make a list of all your tiffs
# these little NDVIs get calculated by map 12.
#    --make sure you have run map 12 
ndvi_series_path <- list.files("output_data/ndvi", full.names = TRUE)

# build a raster stack
ndvi_series_stack <- rast(ndvi_series_path)

# challenge: what are the x,y resolutions?
# what units are the resolution in?
yres(ndvi_series_stack)
xres(ndvi_series_stack)
# fun! we don't have square pixels.


str(ndvi_series_stack)
summary(ndvi_series_stack[,1])

# convert to a data frame
ndvi_series_df <- as.data.frame(ndvi_series_stack, xy=TRUE, na.rm=FALSE) %>% 
  pivot_longer(-(x:y), names_to = "date", values_to= "value")
str(ndvi_series_df)

# unlike in the lesson, our NDVIs go from -1 to 1, like they are supposed to
# so scale factors are not necessary.
ggplot() +
  geom_raster(data = ndvi_series_df , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ date)

# View Distribution of Raster Values

# Explore Unusual Data Patterns
# by comparing to weather data
# which we can get here: 
# https://files.countyofsb.org/pwd/hydrology/historic%20data/rainfall/XLS%20Dailys/200dailys.xls

# change dates from characters to dates

# plot daily precipation for 2023-2024

# Challenge: examine RGB raster files
# What explains our NDVI big changes?