# Map 7
# this is the  layout page with 4 maps on it
# maps 3 - 4 - 5 zoom in tryptic
# and map 6, which is a version of map 1
# 

# Load magick package, which allows to make a montage of images
library(magick)

# We'll be doing a 12inx9in image, which at 300 PPI is 3600x2700 pixels

# Load maps 3, 4, 5, and 6 using the magick::image_read function
map4 <- image_read("images/map3.png") # 3x4 inches = 900x1200 pixels
map5 <- image_read("images/map4.png") # 3x4 in = 900x1200 px
map6 <- image_read("images/map5.png") # 4x3 in = 1200x900 px
map7 <- image_read("images/map6.0.png") # 12x9 in = 3600x1200 px

# There are multiple ways to use the magick package to append images,
# you could use image_append(), image_montage(), or image_composite()

# Let's first make row number 1 or our atlas page, which combines maps 4, 5, and 6"
# This will have a geometry of 4800x1200px (12x4 inches), so each tile is 1600x1200

map7_row1 <- image_montage(image = c(map4, map5, map6),
              tile = "3x1",
              geometry = "1600x1200",
              gravity = "center")
map7_row1

map7 <- image_montage(image = c(map7_row1, map7),
                      tile = "1x2",
                      geometry = "4800x1200")
map7

#save map7
image_write(map7, "images/map7.png", format = "png")