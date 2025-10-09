# map 11
# look at a dibble

library(terra)

# clean the environment and hidden objects
rm(list=ls())

# set map number
current_sheet <- 11
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

dibblee_gol <- rast("source_data/07gGoleta/DB0007.tif")
dibblee_gol <- rectify(dibblee_gol, method="bilinear")

crs(dibblee_gol)

plot(dibblee_gol)
plotRGB(dibblee_gol)

# oops, we lost this file somewhere
#dibblee_gav <- rast("source_data/16gSolvangGaviota/DB0016.tif")
#dibblee_gav <- rectify(dibblee_gav, method="bilinear")

#plot(dibblee_gav)
#plotRGB(dibblee_gav)
#collar <- read.delim("source_data/07gGoleta/DB0007.tif.txt", sep="\t", header = FALSE)
#str(collar)
#collar_df <- as.data.frame(collar)
#str(collar_df)

#collar_vect <- vect(collar)
## Jon is thinking this is a georeferencing table -- not a definition of the collar

# Crop extent from lat/long on the map corners:
# NW corner: 34° 30' 00" N (34.500) / 119° 52' 30" W (-119.875) 
# NE corner: 34° 30' 00" N (34.500) / 119° 45' 00" W (-119.750)
# SE corner: 34° 22' 30" N (34.375) / 119° 45' 00" W (-119.750)
# SW corner: 34° 22' 30" N (34.375) / 119° 52' 30" W (-119.875) 
ll_ext <- ext(-119.875, -119.750, 34.375, 34.500)

# make a vector to reproject form lat/long to utm 
ll_vec <- as.polygons(ll_ext, crs="EPSG:4326")
utm_ext <- ext(project(ll_vec, "EPSG:32611"))

# do the crop
# crop_dibblee_gol <- crop(dibblee_gol, utm_ext)
# plotRGB(crop_dibblee_gol)

# project(dibblee_gol,  "EPSG:32611")


crs(campus_bathotopo)

campus_bathotopo <- rast("output_data/campus_bathotopo.tif") %>% 
  project(campus_bathotopo, dibblee_gol)



crop_dibblee_map_1 <- crop(dibblee_gol, campus_bathotopo)
plotRGB(crop_dibblee_gol)



png(filename="final_output/dibble_aoi.png")
plot(crop_dibblee_gol)
dev.off()
