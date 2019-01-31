## Creating a choropleth of Namibia
## inputs:
## outputs:
## by Hannah Meredith
## Last updated January 30, 2019

install.packages("ggplot2")
install.packages("maptools")
install.packages("rgeos")
install.packages("Cairo")
install.packages("ggmap")
install.packages("scales")
install.packages("RColorBrewer")
install.packages("sf")
install.packages("rgdal")


library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(rgdal)
library(readxl)
set.seed(8000)

##set directory to the folder where the shapefile is, then input shapefile
setwd("/Users/Hannah/Dropbox/Johns Hopkins/Mobility project/R/NAM_adm_shapeFiles")  # set working directory. Make sure both script and datafiles are in the same file
# states.shp <- readShapeSpatial("NAM_adm1.shp")
# states.shp <- st_read("NAM_adm1.shp")
states.shp <- readOGR("NAM_adm1.shp") 
class(states.shp)

#import city locations and names

city_data <- read.csv("NAM_adm2_centroids_name_number_mapping.csv")
lat <- city_data$Y
long <- city_data$X
name <- city_data$NAME_2

cities <- data.frame(cbind(lat, long, name))

## check names of states
print(states.shp$NAME_1)

## input data to plot on map

num.states <- length(states.shp$NAME_1)
mydata<-data.frame(Name_1 = states.shp$NAME_1,id = states.shp$ID_1, popDensity = rnorm(num.states, 55,20))
head(mydata)

## check that data ID in data is the same as in the shape file
 print(mydata$id)
 print(states.shp$ID_1)

# fortify shaoe file to get into dataframe
states.shp.f <- fortify(states.shp, region = "ID_1")
class(states.shp.f)

head(states.shp.f)

## merge with coefficients and reorder
merge.shp.coef <- merge(states.shp.f, mydata, by="id", all.x = TRUE)
final.plot <- merge.shp.coef[order(merge.shp.coef$order), ]

## plot
ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group), fill = "orange",  # could have fill depend on variable (i.e. population density)
               color = "black", size = 0.25)+
  coord_map() + 
  scale_fill_distiller(name = "Percent", palette = "YlGn", breaks = pretty_breaks(n = 5)) +
  labs(title = "Cities in Namibia")+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  geom_text(data=cities, aes(long, lat, label = name), size=3, fontface="bold")