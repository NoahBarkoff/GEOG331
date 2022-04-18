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

# Use this buffer to crop the raster layer so we can see only the area in our study
p2 <- crop(p,b3, overwrite = T,
           filename = "20190706_SR_crop.tif")

# Make a plot of p2
plotRGB(p2, r=3, g=2, b=1,
        scale = 65535,
        stretch= "lin")

# Add plot points to the p2 image that are sized based on canopy cover
points(gtree2, col = "blue", cex = gtree2$cc.pct/50)

# Calculate NDVI
ndvi <- (p2[[4]]-p2[[3]])/(p2[[4]]+p2[[3]])

# Set the layer name to ndvi to avoid confusion
names(ndvi) <- "ndvi"

# create a plot of the ndvi map with sample points on top
png(filename = "ndvi_map.png",
    width = 6, height = 4, units = "in", res = 300)

plot(ndvi)
points(gtree2, cex = gtree$cc.pct/50, col = "blue")

# Turn off 
dev.off()

# Extract ndvi values for each point
nt <- terra::extract(ndvi, gtree2, fun = mean, method = 'bilinear')

# Plot ndvi vs. canopy cover 
plot (nt$ndvi, gtree2$cc.pct,
      pch = 16, col = "blue",
      xlim = c(0,1))

