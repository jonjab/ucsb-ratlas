# map 3-4-5 is a tryptic,
# that runs horizontally on top of map 7
# zooming in to campus
# ie: the zoom to Cali locator sheet

# each of the 3 maps is a grayscale alpha hillshade
# over (some color-scheme) DEMs, 
# -- just as in the overlay episode --
#  -- which overlay episode Jonnie?     --


# clean the environment and hidden objects
rm(list=ls())

library(terra)
library(geojsonsf)
library(sf)
library(ggplot2)
library(tidyterra)
library(dplyr)
library(ggpubr)
library(raster)

# make sure output window is 1x1
# because you muck with it a lot
par(mfrow = c(1,1))

# We'll need a grayscale palette later
grays <- colorRampPalette(c("black", "white"))(255)

# set map number
current_sheet <- 3
# set ggplot counter
current_ggplot <- 0

# our auto ggtitle maker
gg_labelmaker <- function(plot_num){
  gg_title <- c("Map:", current_sheet, " ggplot:", plot_num)
  plot_text <- paste(gg_title, collapse=" " )
  print(plot_text)
  current_ggplot <<- plot_num
  return(plot_text)
}
# every ggtitle should be:
# ggtitle(gg_labelmaker(current_ggplot+1))
# end automagic ggtitle           #######


# set up a local CRS to use throughout
campus_DEM <- rast("source_data/campus_DEM.tif") 
campus_crs = crs(campus_DEM)
str(campus_crs)




# #####################
# Map 3
# Zoom 1: west US overview

# hillshade:
world <- rast("source_data/global_raster/GRAY_HR_SR_OB.tif")
plot(world)

# dem:
zoom_1_dem <- rast("source_data/dem90_hf/dem90_hf.tif")
plot(zoom_1_dem)
# this is a bit too big later on. let's downsample it:
zoom_1_dem <- aggregate(zoom_1_dem, fact=5)

# clip the hillshade
# using an AOI we defined in Planet
zoom_1_extent <- geojson_sf("source_data/cali_overview.geojson")
zoom_1_extent <- vect(zoom_1_extent)
zoom_1_extent <- project(zoom_1_extent, crs(world))

# crop before re-projecting
# because that speeds things up
# AND gives us an interesting shape
zoom_1_hillshade <- crop(x=world, y=zoom_1_extent)
plot(zoom_1_hillshade)

# clip the dem
zoom_1_extent <- project(zoom_1_extent, crs(zoom_1_dem))
zoom_1_dem <- crop(x=zoom_1_dem, y=zoom_1_extent)
plot(zoom_1_dem)


# we'll need this
# socal_aoi.geojson is the next crop extent.
# it came from a Planet harvest that we did
zoom_2_crop_extent <- geojson_sf("source_data/socal_aoi.geojson")
zoom_2_crop_extent <- vect(zoom_2_crop_extent)

# now we do it with ggplot
# and alphas
#################################################
# first check that projections match
crs(zoom_1_dem) == crs(zoom_1_hillshade)
zoom_1_dem <- project(zoom_1_dem, crs(zoom_1_hillshade))
crs(zoom_1_dem) == crs(zoom_1_hillshade)

crs(zoom_1_dem) == crs(zoom_2_crop_extent)
zoom_2_crop_extent <- project(zoom_2_crop_extent, crs(zoom_1_dem))
crs(zoom_1_dem) == crs(zoom_2_crop_extent)

# now all 3 layers are in the same CRS

# zoom 1 hillshade as ggplot
str(zoom_1_hillshade)
zoom_1_hillshade_df <- as.data.frame(zoom_1_hillshade, xy=TRUE)
colnames(zoom_1_hillshade_df)

zoom_1_hillshade_plot <- ggplot() +
  geom_raster(data = zoom_1_hillshade_df,
              aes(x=x, y=y, fill=GRAY_HR_SR_OB)) +
  scale_fill_continuous() +
  theme_dark() +
  coord_sf() + 
  ggtitle("Western US Hillshade", subtitle = gg_labelmaker(current_ggplot+1))

zoom_1_hillshade_plot

# zoom 1 DEM as ggplot
str(zoom_1_dem)
zoom_1_dem_df <- as.data.frame(zoom_1_dem, xy=TRUE)
colnames(zoom_1_dem_df)

zoom_1_dem_plot <- ggplot() +
  geom_raster(data = zoom_1_dem_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_continuous() +
  theme_dark() +
  coord_sf() + 
  ggtitle("Western US DEM", subtitle = gg_labelmaker(current_ggplot+1))

zoom_1_dem_plot

# now overlay them

zoom_1_overlay_plot <- ggplot() +
  geom_raster(data = zoom_1_dem_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_continuous() +
  geom_raster(data = zoom_1_hillshade_df,
              aes(x=x, y=y, alpha=GRAY_HR_SR_OB)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=zoom_2_crop_extent, color="red", lwd= 1.5, fill=NA) +
  theme_dark() +
  coord_sf() + 
  ggtitle("Western US Fancy Overlay", subtitle = gg_labelmaker(current_ggplot+1))

zoom_1_overlay_plot



zoom_1_plot <- ggplot() +
  geom_raster(data = zoom_1_dem_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  geom_spatvector(data=zoom_2_crop_extent, color="red", lwd= 2, fill=NA) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf(crs=campus_crs) + 
  ggtitle("Map 3: zm 1: Western US Hillshade", subtitle = gg_labelmaker(current_ggplot+1))

zoom_1_plot

# bring in the hillshade with alpha
zoom_1_plot <- ggplot() +
  geom_raster(data = zoom_1_dem_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  geom_raster(data = zoom_1_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=zoom_2_crop_extent, color="red", lwd= 2, fill=NA) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf(crs=campus_crs) + 
  ggtitle("Map 3: zm 1: Western US Hillshade", subtitle = gg_labelmaker(current_ggplot+1))





# we may not keep the blue color scheme








# now let's add 
# 'california populated places'
# which is census data
# for the sake of a nice vizualization
places <- vect("source_data/tl_2023_06_place/tl_2023_06_place.shp")
plot(places)
# overlay this on top of zoom 1 and zoom 2

zoom_1_plot <- ggplot() +
  geom_raster(data = zoom_1_hillshade_df,
              aes(x=x, y=y, fill=GRAY_HR_SR_OB)) +
  geom_spatvector(data=places, fill="gray") +
  geom_spatvector(data=zoom_2_crop_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf(crs=campus_crs) +
  thin_grat +
  ggtitle("Map 3: zm 1: Western US Hillshade", subtitle = gg_labelmaker(current_ggplot+1))

zoom_1_plot




# #######################################################
# back to start of zoom 
# do these all look the same yet?







#######################################################################################


ggsave("images/map3.png", width = 3, height = 4, plot=zoom_1_plot)
ggsave("images/map4.png", width = 3, height = 4, plot=zoom_2_plot)
ggsave("images/map5.png", width = 4, height = 3, plot=zoom_3_plot)

