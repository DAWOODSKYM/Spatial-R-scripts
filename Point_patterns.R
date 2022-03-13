#An R script to create and plot density maps(point patterns)
library(spatstat)
library(sf)
library(raster)
library(maptools)
library(sp)
library(rgdal)
#Reading the Subcounty shapefiles
shapefile<- readOGR("Laikipia_Subcounty_projected.shp")
Laikipia  <- as.owin(shapefile)
Laikipia.km <- rescale(Laikipia, 1000)

#Reading the health facilities
HealthSH  <- readOGR("Health_facilities.shp")  
HCF  <- as.ppp(HealthSH)
marks(HCF)<- NULL
HCF <- rescale(HCF, 1000)
Window(HCF) <- Laikipia.km

#plotting and saving the Health facilities and sub counties as jpg file
jpeg("SubCounty_HealthFacilities.jpg", width = 400, height = 400)
plot(HCF, main="Subcounty and Healthfacilities", cols=rgb(0,0,0,.2), pch=20)
dev.off()

#computting and plotting The quadrants and counting number of points in each as jpg file
QLaikipia <- quadratcount(HCF, nx= 6, ny=2)
jpeg("Quadrant_points.jpg", width = 400, height = 400)
plot(HCF, pch=20, cols="grey70", main="Quadrants For laikipia")  # Plot points
plot(QLaikipia, add=TRUE)
dev.off()

# Compute and plotting and saving the density of points for each quadrat as jpg file
Q.d <- intensity(QLaikipia)
jpeg("Quadrant_Density.jpg", width = 400, height = 400)
plot(intensity(QLaikipia, image=TRUE), main="Quadrant_density for laikipia", las=1)  # Plot density raster
plot(HCF, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points
dev.off()

#computing, plotting and saving the Kernel density raster as jpg file
HCFKernel <- density(HCF) # Using the default bandwidth
jpeg("Kernel_Density.jpg", width = 400, height = 400)
plot(HCFKernel, main="Kernel Density", las=1)
contour(HCFKernel, add=TRUE)
# 3. Close the file
dev.off()