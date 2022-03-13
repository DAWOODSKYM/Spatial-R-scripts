#An R script to calculate the areas and centroids of polygons given the coordinates of the polygons
library(sp)
library(rgeos)
x_POlYGON56 <- c(933270.32,933232.31,933231.78,933223.47,933223.89,933262.15,933270.32)
y_POlYGON56 <- c(9855259,9855235,9855235,9855248,9855249,9855272,9855259)

x_POlYGON57 <- c(933278.06,933239.68,933239.44,933231.78,933232.31,933270.32,933278.06)
y_POlYGON57 <- c(9855246,9855222,9855222,9855235,9855235,9855259,9855246)

x_POlYGON58 <- c(933286.17,933247.6,933247.1,933239.44,933239.68,933278.06,933286.17)
y_POlYGON58 <- c(9855233,9855210,9855210,9855222,9855222,9855246,9855233)

x_POlYGON59 <- c(933293.97,933255.73,933255.27,933247.1,933247.6,933286.17,933293.97)
y_POlYGON59 <- c(9855220,9855197,9855197,9855210,9855210,9855233,9855220)

POL56 <- cbind(x_POlYGON56, y_POlYGON56)
POL57 <- cbind(x_POlYGON57, y_POlYGON57)
POL58 <- cbind(x_POlYGON58, y_POlYGON58)
POL59 <- cbind(x_POlYGON59, y_POlYGON59)

#Creating polygon56 
poly56 = Polygon(POL56)
ps56 = Polygons(list(poly56),1)
spatialP56 = SpatialPolygons(list(ps56))

#Creating polygon57 
poly57 = Polygon(POL57)
ps57 = Polygons(list(poly57),1)
spatialP57 = SpatialPolygons(list(ps57))

#Creating polygon58 
poly58 = Polygon(POL58)
ps58 = Polygons(list(poly58),1)
spatialP58 = SpatialPolygons(list(ps58))

#Creating polygon59 
poly59 = Polygon(POL59)
ps59 = Polygons(list(poly59),1)
spatialP59 = SpatialPolygons(list(ps59))

#Get the area & centroid poly56
area56<-gArea(spatialP56)
centroid56 = gCentroid(spatialP56,byid=TRUE)
print(centroid56)
print(paste("The area of Polygon 56 is",area56))

#Get the area & centroid poly57
area57<-gArea(spatialP57)
centroid57 = gCentroid(spatialP57,byid=TRUE)
print(centroid57)
print(paste("The area of Polygon 57 is",area57))

#Get the area & centroid poly58
area58<-gArea(spatialP58)
centroid58 = gCentroid(spatialP58,byid=TRUE)
print(centroid58)
print(paste("The area of Polygon 58 is",area58))

#Get the area & centroid poly56
area59<-gArea(spatialP59)
centroid59 = gCentroid(spatialP59,byid=TRUE)
print(centroid59)
print(paste("The area of Polygon 59 is",area59))
