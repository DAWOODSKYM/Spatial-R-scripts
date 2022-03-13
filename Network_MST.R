#R code to solve a minimum spanning tree problem
#A minimum spaning tree network problem is a problem that aims to minimize the total length covered
#whilst ensuring every point is reachable from every other point
library(rgdal)
library(tmap)
library(spdep)
library(maptools)
### loading data
bh<-readShapePoly(("F://Dropbox//JKUAT//CourseWork//2020-2021//Sem_II//EGE_2421//data//shapes//bhicv.shp")[1])
#plot the shape file
Plot(bh)
### Scaling / Normalizing the column
dpad<-data.frame(scale(bh@data[,5:8]))
### neighborhood list
#The function builds a neighbors list based on regions with contiguous boundaries, that is sharing one or more boundary point
bh.nb<-poly2nb(bh)
### calculating costs
#Compute Cost Of Edges. The cost of each edge is the distance between it nodes.
lcosts<-nbcosts(bh.nb, dpad)
### making listw
# Spatial Weights For Neighbours Lists. The nb2listw function supplements a neighbours list with spatial weights for the chosen coding scheme.
nb.w<-nb2listw(bh.nb, lcosts, style="B")
### find a minimum spanning tree
system.time(mst.bh <-mstree(nb.w,5))
### the mstreeplot
par(mar=c(0,0,0,0))
plot(mst.bh, coordinates(bh), col=2,
     cex.lab=.7, cex.circles=0.035, fg="blue")
plot(bh, border=gray(.5), add=TRUE)