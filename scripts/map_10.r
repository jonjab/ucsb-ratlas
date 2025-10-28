# map 10
library(terra)
library(tidyverse)
library(tidyterra)
# clean the environment and hidden objects
rm(list=ls())

# set map number
current_sheet <- 10
# set ggplot counter
current_ggplot <- 0

gg_labelmaker <- function(plot_num){
  gg_title <- c("Map:", current_sheet, " ggplot:", plot_num)
  plot_text <- paste(gg_title, collapse=" " )
  print(plot_text)
  current_ggplot <<- plot_num
  return(plot_text)
}

# every ggtitle() or labs() should be:
# ggtitle(gg_labelmaker(current_ggplot+1))
# end automagic ggtitle           #######

# ---------- 1. read layers ----------
# bike_paths <- vect("") # replace with updated bike-path file
bike_paths <- vect("source_data/icm_bikes/bike_paths/bikelanescollapsedv8.shp")
creeks     <- vect("source_data/california_streams/streams_crop.shp")

# ---------- 2. common CRS ----------
creeks      <- project(creeks, bike_paths)

# plot creeks and paths together here to prove it
creeks_and_bikes <- ggplot() +
  geom_sf(data=bike_paths, color = "darkgray") +
  geom_sf(data=creeks, color = "blue") +
  coord_sf() +
  ggtitle(gg_labelmaker(current_ggplot+1))
creeks_and_bikes


# ---------- 3. intersection points ----------
crossings <- intersect(creeks, bike_paths)

str(crossings)
crossings <- vect(crossings)

plot(crossings)


# ---------- 4. write shapefile ----------
#  Do we output a new point shapefile as a result?
out_file <- "output_data/creek_bike_intersections.shp"
writeVector(creek_bike_pts, out_file, overwrite=TRUE)

# 5 visualize all that for me
creeks_and_bikes <- ggplot() +
  geom_sf(data=bike_paths, color = "darkgray") +
  geom_sf(data=creeks, color = "blue") +
  geom_sf(data=creek_bike_pts, color = "red", size = 4) +
  coord_sf() +
  ggtitle(gg_labelmaker(current_ggplot+1))


creeks_and_bikes



# Create two example line SpatVectors
line1_coords <- matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)
line2_coords <- matrix(c(0, 5, 5, 0), ncol = 2, byrow = TRUE)

line1 <- vect(line1_coords, type = "lines", crs = "epsg:4326")
line2 <- vect(line2_coords, type = "lines", crs = "epsg:4326")

# Find the intersection
intersection_points <- intersect(line1, line2)

# Plot the results (optional)
plot(line1, col = "blue", lwd = 2)
plot(line2, col = "red", lwd = 2, add = TRUE)
plot(intersection_points, col = "green", pch = 16, cex = 1.5, add = TRUE)
