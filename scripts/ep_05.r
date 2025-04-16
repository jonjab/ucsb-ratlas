# episode 5
# Work with Multi-Band Rasters
# ie: color
# https://datacarpentry.github.io/r-raster-vector-geospatial/05-raster-multi-band-in-r.html

# clean the environment and hidden objects
rm(list=ls())

library(terra)
library(tidyverse)
library(raster)

current_episode <- 5

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


# Getting Started with Multi-Band Data in R
######################

# load just 1 layer.
west_campus_1ft <- rast("source_data/cirgis_1ft/w_campus_1ft.tif", lyrs = 1)
# it draws realslow. do let's downsample
# as in episode 12
west_campus_4ft <- aggregate(west_campus_1ft, fact = 4)

west_campus_df <- as.data.frame(west_campus_4ft, xy = TRUE)
str(west_campus_df)

ggplot() +
  geom_raster(data = west_campus_df,
              aes(x = x, y = y, alpha = w_campus_1ft_1)) + 
  coord_quickmap()



# Import A Specific Band




# Raster Stacks in R
######################

# if you don't speficy, you get all the bands

west_campus_1ft <- rast("source_data/cirgis_1ft/w_campus_1ft.tif")
# it draws realslow. do let's downsample
# as in episode 12
west_campus_4ft <- aggregate(west_campus_1ft, fact = 4)

# this shows there are 5 bands
west_campus_4ft

# can send just 1 band to stdout:
west_campus_4ft[[2]]

west_campus_df <- as.data.frame(west_campus_4ft, xy = TRUE)
str(west_campus_df)


# Create A Three Band Image
######################
# https://datacarpentry.github.io/r-raster-vector-geospatial/05-raster-multi-band-in-r.html#create-a-three-band-image

# plotRGB is for stacks
plotRGB(west_campus_4ft)

plotRGB(west_campus_4ft, stretch = "lin")
plotRGB(west_campus_4ft, stretch = "hist")


plotRGB(west_campus_4ft,
        r = 1,
        g = 2, 
        b = 3)

plotRGB(west_campus_4ft,
        r = 2,
        g = 3, 
        b = 4)

plotRGB(west_campus_4ft,
        r = 3,
        g = 4, 
        b = 5)

plotRGB(west_campus_4ft,
        r = 3,
        g = 2, 
        b = 1)




# plotRGB(NCOS, r=1, g=2, b=3)



# brick is from raster
# a brick is a new class for us
# nl is the number of layers it should expect.
natural_color_brick <- brick("source_data/cirgis_1ft/w_campus_1ft.tif")
natural_color_brick

plotRGB(natural_color_brick)


# the metadata shows 5 bands. How 
# about nodata value?

# as we did in ep. 1
# we can use >describe<
# but I don't see NA's described
describe("source_data/w_campus_1ft/w_campus_1ft.tif")

# SpatRaster in R
###################

# SpatRasterDataset comes from terra

# ????????????????????
# natural_color_sds <- sds(natural_color_brick)


# Challenge: What Functions Can Be Used on an 
# R Object of a particular class?
#


