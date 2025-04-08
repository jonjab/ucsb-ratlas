# episode 10
# Convert from .csv to a Vector Layer

# we will also use a json aoi as we use that elsewhere.

# make a csv from existing data and then backwards convert it?

library(sf)
library(terra)

# make a csv from existing bird point data and conver it to csv
birds <- st_read("source_data/NCOS_Bird_Survey_Data_20190724shp/NCOS_Bird_Survey_Data_20190724_web.shp")
str(birds)

# write out a csv
st_write(birds, "output_data/bird_points.csv", append=FALSE, layer_options = "GEOMETRY=AS_XY")

# Import .csv
birds_csv <- read.csv("output_data/bird_points.csv")

# Identify x,y, columns
str(birds_csv)
# head()

# .csv to sf object

# st_as_sf() 



# Plot Spatial Object

# Plot Extent
# AOI plus points

# use a geojson aoi


# Challenge: import & plot more points

# Export to an Esri shapefile


# ends with st_write out to a point shapefile.
# from this point on, maybe we should depend more on 
# the output data folder.
