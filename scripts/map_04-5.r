# map 4
# clean the environment and hidden objects

rm(list=ls())

library(terra)
library(geojsonsf)
library(ggplot2)

# set map number
current_sheet <- 4
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
# end automagic ggtitle 

# gray like map 3
grays <- colorRampPalette(c("black", "white"))(255)

# ############################
# Map 4
# Zoom 2: Bite of California

#use campus CRS
campus_DEM <- rast("source_data/campus_DEM.tif") 
crs(campus_DEM)
campus_crs <-crs(campus_DEM)


# Crop western region DEM to zoom 2 AOI
# socal_aoi.geojson
zoom_2 <- rast("source_data/dem90_hf/dem90_hf.tif")
plot(zoom_2)

# we'll need this
# socal_aoi.geojson is the next crop extent.
# it came from a Planet harvest that we did
zoom_2_crop_extent <- geojson_sf("source_data/socal_aoi.geojson")
zoom_2_crop_extent <- vect(zoom_2_crop_extent)

crs(zoom_2_crop_extent) == crs(zoom_2)

# project it to match west_us
# this will also 'rotate' southern california
# when we crop it, making it 
# visually more readable
zoom_2_crop_extent <- project(zoom_2_crop_extent, crs(zoom_2))
crs(zoom_2_crop_extent) == crs(zoom_2)

# to confirm that's the correct extent
# that you want to crop to
plot(zoom_2)
polys(zoom_2_crop_extent)

# now crop to that extent
zoom_2_cropped <- crop(x=zoom_2, y=zoom_2_crop_extent)
plot(zoom_2_cropped)

# we now turn zoom 2 DEM into a hillshade of the area to match:
# hillshades are made of slopes and aspects
zoom_2_slope <- terrain(zoom_2_cropped, "slope", unit="radians")
plot(zoom_2_slope)

zoom_2_aspect <- terrain(zoom_2_cropped, "aspect", unit="radians")
plot(zoom_2_aspect)
zoom_2_hillshade <- shade(zoom_2_slope, zoom_2_aspect,
                          angle = 15,
                          direction = 270,
                          normalize = TRUE)


# remake dataframes
zoom_2_DEM_df <- as.data.frame(zoom_2_cropped, xy=TRUE)
zoom_2_hillshade_df <- as.data.frame(zoom_2_hillshade, xy=TRUE)
str(zoom_2_hillshade_df)

# test and reproject as necessary
zoom_3_extent <-ext(campus_DEM)
crs(zoom_3_extent) == campus_crs
crs(zoom_2_cropped) == campus_crs
crs(zoom_2_hillshade) == campus_crs

zoom_2_cropped <- project(zoom_2_cropped, campus_crs)
zoom_2_hillshade <- project(zoom_2_cropped, campus_crs)


# 'california populated places'
# which is census data
# for the sake of a nice vizualization
places <- vect("source_data/tl_2023_06_place/tl_2023_06_place.shp")
plot(places)
crs(places) == campus_crs
places <- project(places, campus_crs)

#places still not showing up
---------------------------------------

# For zoom 2, places does not overlay nicely.
# this is another CRS error
zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_DEM_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_viridis_c() +
  geom_raster(data = zoom_2_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=places, fill="gray") +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none") +
  coord_sf() +
  thin_grat +
  ggtitle("Map 4: zm 2: Zoom test", subtitle=gg_labelmaker(current_ggplot+1))

zoom_2_plot


# move layer lower, add overlays first?
# crs(places) == campus_crs


crs(zoom_2_cropped) == campus_crs
crs(zoom_2_hillshade) == campus_crs
crs(places) == campus_crs

# try again:
# nope, you are gonna need to crop places
zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_DEM_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_viridis_c() +
  geom_raster(data = zoom_2_hillshade_df,
              aes(x=x, y=y, alpha=dem90_hf)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=places, fill="gray") +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none") +
  coord_sf() + 
  thin_grat +
  ggtitle("Map 4: zm 2: Bite of California", subtitle = gg_labelmaker(current_ggplot+1))

# this plots, but at the full extent of california.
zoom_2_plot

zoom_2_places <- crop(places, zoom_2_cropped)

zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_DEM_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_viridis_c() +
  geom_spatvector(data=zoom_2_places, fill="lightgray", color="gray") +
  geom_raster(data = zoom_2_hillshade_df,
              aes(x=x, y=y, alpha=dem90_hf)) +
  scale_alpha(range = c(0.2, 0.7), guide="none") +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf() + 
  thin_grat +
  ggtitle("Map 4: zm 2: Bite of California", subtitle = gg_labelmaker(current_ggplot+1))

zoom_2_plot












# now we want to use the extent of the campus_DEM
# for our next zoom indicator
# but they don't overlay
zoom_3_extent <- ext(campus_DEM) %>% vect()

plot(zoom_2_cropped)
polys(zoom_3_extent, border="red", lwd=4)

# because the projections don't match
crs(zoom_3_extent) == crs(zoom_2_cropped)
# in fact, zoom 3 has no crs set:
crs(zoom_3_extent)
# so we need to set it
zoom_3_extent <- set.crs(zoom_3_extent, crs(campus_DEM))
crs(zoom_3_extent)

# now we can re-project:
zoom_3_extent <- project(zoom_3_extent, zoom_2_cropped)

crs(zoom_3_extent) == crs(zoom_2_cropped)

# now we can plot them together
plot(zoom_2_cropped)
polys(zoom_3_extent, border="red", lwd=2)



plot(zoom_2_hillshade, col = grays)
polys(zoom_3_extent, border="red", lwd=4)

plot(zoom_2_cropped, col = grays)
polys(zoom_3_extent, border="red", lwd=4)

# later on we will overlay in ggplot


# ###########################
# Map 5
# Zoom 3: UCSB & Environs
# these come pre-made

plot(campus_DEM)
zoom_3_hillshade <- rast("source_data/campus_hillshade.tif")
plot(zoom_3_hillshade, col = grays)



# ##########################
# do all the AOI polygons work with 
# all the hillshades and DEMs?
# make sure everything is in campus_CRS
# maybe I don't want to do this. 

# zoom 1:
crs(zoom_1_dem) == campus_crs
# zoom_1_dem <- project(zoom_1_dem, campus_crs)

crs(zoom_1_hillshade) == campus_crs
# zoom_1_dem <- project(zoom_1_dem, campus_crs)

# zoom 2:
crs(zoom_2) == campus_crs
# renaming this here also for convenience
# zoom_2_dem <- project(zoom_2, campus_crs)

crs(zoom_2_hillshade) == campus_crs
# renaming this here also for convenience
# zoom_2_dem <- project(zoom_2, campus_crs)

# zoom 3:
crs(campus_DEM) == campus_crs
crs(zoom_3_hillshade) == campus_crs


# test the plots with locators
plot(zoom_1_dem, col = grays)
polys(zoom_2_crop_extent, border="red",lwd=5)

# zoom 2: works
plot(zoom_2_hillshade, col = grays)
polys(zoom_3_extent, border="red", lwd=5)

# zoom 3:
# doesn't have a locator





#################################################
# zoom 2 as ggplot
plot(zoom_2_hillshade)
polys(zoom_3_extent, border="red", lwd=2)

zoom_2_DEM_df <- as.data.frame(zoom_2_cropped, xy=TRUE)
colnames(zoom_2_DEM_df)

zoom_2_hillshade_df <- as.data.frame(zoom_2_hillshade, xy=TRUE)
colnames(zoom_2_hillshade_df)


# ggplot the hillshade
zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none") +
  coord_sf() + 
  ggtitle("Zoom 2: Bite of California hillshade", subtitle = gg_labelmaker(current_ggplot+1))

zoom_2_plot

# ggplot the DEM
zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_DEM_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_viridis_c() +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none") +
  coord_sf() + 
  ggtitle("Zoom 2: Bite of California w DEM", subtitle = gg_labelmaker(current_ggplot+1))

zoom_2_plot

# now overlay them with alpha
zoom_2_plot <- ggplot() +
  geom_raster(data = zoom_2_DEM_df,
              aes(x=x, y=y, fill=dem90_hf)) +
  scale_fill_viridis_c() +
  geom_raster(data = zoom_2_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  geom_spatvector(data=zoom_3_extent, color="red", fill=NA, lwd=1) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf() + 
  ggtitle("Zoom 2: Bite of California w alpha overlay", 
          gg_labelmaker(current_ggplot+1))

zoom_2_plot

# Zoom 2 still needs water. 



#################################################
# zoom3 as ggplot
campus_hillshade <- rast("source_data/campus_hillshade.tif")
str(campus_hillshade)
zoom_3_hillshade_df <- as.data.frame(campus_hillshade, xy=TRUE)
colnames(zoom_3_hillshade_df)

# ggplot the hillshade
zoom_3_plot <- ggplot() +
  geom_raster(data = zoom_3_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf() + 
  ggtitle(gg_labelmaker(current_ggplot+1), subtitle = "Campus hillshade")

zoom_3_plot

# ggplot the DEM
zoom_3_DEM_df <- as.data.frame(campus_DEM, xy=TRUE)
str(zoom_3_DEM_df)

zoom_3_plot <- ggplot() +
  geom_raster(data = zoom_3_DEM_df,
              aes(x=x, y=y, fill=greatercampusDEM_1_1)) +
  scale_fill_viridis_c() +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf() + 
  ggtitle(gg_labelmaker(current_ggplot+1), subtitle = "UCSB DEM")
zoom_3_plot

# now overlay
zoom_3_plot <- ggplot() +
  geom_raster(data = zoom_3_DEM_df,
              aes(x=x, y=y, fill=greatercampusDEM_1_1)) +
  scale_fill_viridis_c() +
  geom_raster(data = zoom_3_hillshade_df,
              aes(x=x, y=y, alpha=hillshade)) +
  scale_alpha(range = c(0.05, 0.5), guide="none") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position="none", 
        panel.ontop=TRUE,
        panel.background = element_blank()) +
  coord_sf(label_axes="ES") + 
  ggtitle("Map 5: zm 3: UCSB & Surroundings", subtitle = gg_labelmaker(current_ggplot+1))


zoom_3_plot




# ################################
# for thinner graticules
thin_grat <- theme(
  panel.grid.major = element_line(colour = "white", linewidth = 0.05),  # was 0.5
  panel.grid.minor = element_line(colour = "white", linewidth = 0.05)
)



# zoom 2 needs water
zoom_2_plot

# zoom 3 needs water, or should use topo_batho
zoom_3_plot