##An R Script to:
#a.	Read a counties boundary shapefile 
#b.	Read a csv file containing population data (male/female)
#c.	Join the two based on a common field
#d.	Compute population density 
#e.	Plot and output a pdf map size A4	
#Loading the required linbraries
library(tmap)
library(sf)
library(dplyr)
library(rgeos)
library(rgdal)
#Reading county and csv data
County<- st_read("F:\\Data\\kenya_data\\Countiesprj.shp")
County$Area

#Reading the csv file and saving it as a dataframe
csvfile<- read.csv("F:\\Studies\\Mathomo 3.2\\Spatial Analysis 1\\Assignments\\Cat\\2019-population_census.csv")
Pop<-data.frame(csvfile)

#joining the two files
Datajoin<-left_join(County,Pop,by=c('Codes'='CODES'))

#Calculating the population density
popDen <- function(pop,area) 
{
  pop/area
}
popDen(Datajoin$Total_Population19,Datajoin$Area)

#Ploting the population density
map1<-tm_shape(Datajoin)+tm_polygons("Population.Density",palette="Reds",contrast=1)+tm_layout(legend.frame="Red",title="Kenya Population")
print(map1)
#Saving the map as a pdf
tmap_save(map1,file="map.pdf")