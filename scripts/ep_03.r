#############################################
# ep3.r
# re-project raster data
# overlays


# libraries for this episode:
library(ggplot2)
library(dplyr)
library(terra)


# clean the environment and hidden objects
rm(list=ls())
current_episode <- 3

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



# ## Set up objects from previous episodes

# create the campus DEM
campus_DEM <- rast("source_data/campus_DEM.tif")

campus_DEM_df <-  campus_DEM %>% 
  as.data.frame(xy=TRUE) %>% 
  rename(elevation = greatercampusDEM_1_1)


# add the custom bins to the dataframe
custom_bins <- c(-3, -.01, .01, 2, 3, 4, 5, 10, 40, 200)
campus_DEM_df <- campus_DEM_df %>% 
  mutate(binned_DEM = cut(elevation, breaks = custom_bins))

str(campus_DEM_df)

# ## Raster Projection in R
#################################

# ep3 is reprojections. We need a raster in a different projection.
# how about bathymetry? (this is our digital terrain model)
# SB_bath.tif came out of data_prep.r
# make it the tidy way, so that there's not an extra object
bath_rast <- rast("source_data/SB_bath.tif")  
bath_rast 
campus_DEM

bath_df <-  bath_rast %>% 
  as.data.frame(xy=TRUE)
str(bath_df)

colnames(bath_df)[colnames(bath_df) == "Bathymetry_2m_OffshoreCoalOilPoint"] <- "depth"
str(bath_df)


summary(bath_df, maxsamp = ncell(bath_df))
# summary gives us a hint on ranges
# for custom bins

# the summary view also shows the pixel coordinates are different--
# so that's a clear indication these won't overlay.

# as in lesson 1
ggplot() +
  geom_raster(data = bath_df) +
  aes(x=x, y=y, fill=depth) +
  scale_fill_viridis_c() +
  ggtitle(gg_labelmaker(current_ggplot+1)) +
  coord_sf()

# histogram helps determine good bins
ggplot() +
  geom_histogram(data = bath_df, aes(depth), bins = 10) +
  ggtitle(gg_labelmaker(current_ggplot+1)) 
# these should work:
custom_bath_bins <- c(1, -5, -15, -35, -45, -55, -60, -65, -75, -100, -125)

bath_df <- bath_df %>% 
  mutate(binned_bath = cut(depth, breaks=custom_bath_bins))

summary(bath_df)

ggplot() + 
  geom_raster(data = bath_df, aes(x=x, y=y, fill = binned_bath)) +
  scale_fill_manual(values = terrain.colors(10)) +
  ggtitle(gg_labelmaker(current_ggplot+1)) +
  coord_quickmap()


# 'create a map of DTM layered over hillshade'
# from the lesson. Ours is:
# put the coastal DEM over the coastal bathymetry
# this reproduces the '2 rasters with mismatched projections'
# part of the lesson
ggplot() +
  geom_raster(data = bath_df, aes(x=x, y=y, fill = binned_bath)) +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, alpha = elevation)) +
  scale_alpha(range = c(0.15, 0.65), guide = "none") +
  ggtitle(gg_labelmaker(current_ggplot+1)) +
  coord_quickmap()


# Exercise: View the CRS for each dataset 

# let's remake bath_df with a re-projected raster
# check the original bathymetry raster projection:
crs(bath_rast , proj=TRUE)
crs(campus_DEM , proj=TRUE)

crs(bath_rast, parse=TRUE)

# I need to get projection and resolution objects somewhere.
crs(campus_DEM ) 
res(campus_DEM)


# ### Reproject Rasters
#################################
# We can reproject using the other raster as reference matching projection and resolution

# this is terra project or sf project?
# terra. We don't load sf in this file. 
reprojected_bath <- project(bath_rast, campus_DEM)
reprojected_bath

# Have a look
plot(reprojected_bath)

# remake bath_df
bath_df <- as.data.frame(reprojected_bath, xy=TRUE) %>% 
  rename(depth = Bathymetry_2m_OffshoreCoalOilPoint) 

str(bath_df)

# add the binned column to both dataframes
bath_df <- bath_df %>% 
  mutate(binned_bath = cut(depth, breaks = custom_bath_bins))


# so now they are in the same crs, and overlay!
# (even if they are not very pretty)
ggplot() +
  geom_raster(data = bath_df, aes(x=x, y=y, fill = binned_bath)) +
  scale_alpha_binned(range = c(0.15, 0.65), guide = "none") +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, fill = binned_DEM)) +
  ggtitle(gg_labelmaker(current_ggplot+1)) +
  coord_quickmap()


# hide the NA's
# scale_alpha doesn't seem to like na.value
# plot 2 custom binned maps for the sake of the overlay
ggplot() +
  geom_raster(data = bath_df, aes(x=x, y=y, fill = depth)) +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, fill = elevation)) +
  scale_fill_viridis_c(na.value="red") +
  coord_quickmap()

ggplot() +
  geom_raster(data = bath_df, aes(x=x, y=y, fill = depth)) +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, fill = elevation)) +
  scale_fill_viridis_c(na.value="NA") +
  coord_quickmap()



## to do
# ### Deal with Raster Resolutionn 

# the challenges.
# Challenge: Reproject, then Plot a Digital Terrain Model

# Is there a before-and-after DEM of NCOS?







# maybe nothing past this point is really related to the lesson narrative.
# ####################################


# get a bounding box out of campus DEM to clip the bathymetry.
# later on we will clip to extent, but for now we will leave it at this:

# extent object
campus_border <- ext(campus_DEM)
campus_border

#can be turned into a spatial object
campus_border_poly <- as.polygons(campus_border, crs(campus_DEM))
campus_border_poly

# and written out to a file:
writeVector(campus_border_poly, 'output_data/ep_3_campus_borderline.shp', overwrite=TRUE)

# from ep 11: crop the bathymetry to the extent
# of campus_DEM
bath_clipped <- crop(x=reprojected_bath, y=campus_border_poly)
plot(bath_clipped)

# now we can make a big, slow overview map, and save the clipped bathymetry
# for overlaying goodness:

# save the file:
# ( as in ep 4:)
dir.create("output_data", showWarnings = FALSE)
writeRaster(bath_clipped, "output_data/ep_3_campus_bathymetry_crop.tif",
            filetype="GTiff",
            overwrite=TRUE)

# and the DEM:
writeRaster(campus_DEM, "output_data/ep_3_campus_DEM.tif",
            filetype="GTiff",
            overwrite=TRUE)


# Note that with the terra package, 
# we dealt with both reprojection and cropping at the same time.
# did we though? Did we really crop?
# reprojected_bath <- project(bath_rast, campus_DEM)

campus_bath_df <- as.data.frame(bath_clipped, xy=TRUE) %>% 
  rename(depth = Bathymetry_2m_OffshoreCoalOilPoint)
str(campus_bath_df)
colnames(campus_bath_df)



# now we have a smaller campus bathymetry dataframe:
ggplot() +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, fill = elevation)) +
  geom_raster(data = campus_bath_df, aes(x=x, y=y, fill = depth)) +
  scale_fill_viridis_c(na.value="NA") +
  coord_quickmap()

