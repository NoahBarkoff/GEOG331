# Noah Barkoff
# In class Activity


# Load in the terra package
library(terra)

# Set the working directory
setwd("Z:/students/nbarkoff/GitHub/GEOG331/")

# Read in a raster file
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# Plot the raster
plot(p)

# Plot an rgb rendering depicting the data
plotRGB(p, r=3, g=2, b=1)

# Plot an rgb rendering depicting the data
plotRGB(p, r=3, g=2, b=1,
        scale = 65535,
        stretch= "hist")
# Read file with field observations of canopy cover
tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv")

# Convert to vector object using terra package
gtree <- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

# Project the data to match the coordinate system of the raster layer
gtree2 <- project(gtree,p)

# Create a polyon from the extent of the points
b <- as.lines(ext(gtree), "epsg:4326")

# Reproject the polygons to the same projection as our raster
b2 <- project(b,crs(p))

# Buffer the extent by 200m
b3 <- buffer(b2, width= 200)
plot(gtree2, add=T, col= "red")
ext(gtree2)