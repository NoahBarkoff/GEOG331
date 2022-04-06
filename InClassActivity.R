# Noah Barkoff
# In class Activity


# Load in the terra package
install.packages("terra")
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