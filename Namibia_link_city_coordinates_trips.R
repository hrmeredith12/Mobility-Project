## Create data.frame that holds: city IDs, coordinates, and trip # for each date
## inputs: city coordinate data: NAM_adm2_centroids_name_number_mapping.csv
##       : city ID and trip data: NAM_between_day_mobility_updated.csv
## outputs: data.frame that holds: city IDs, coordinates, and trip # for each date
##          map visualizing trips between cities
## by Hannah Meredith
## Last updated January 31, 2019

# install.packages("ggplot2")
# install.packages("maptools")
# install.packages("rgeos")
# install.packages("Cairo")
# install.packages("ggmap")
# install.packages("scales")
# install.packages("RColorBrewer")
# install.packages("sf")
# install.packages("rgdal")
# 
# 
# library(ggplot2)
# library(maptools)
# library(rgeos)
# library(Cairo)
# library(ggmap)
# library(scales)
# library(RColorBrewer)
# library(sf)
# library(rgdal)
# library(readxl)
# set.seed(8000)


##set directory to the folder 
setwd("/Users/Hannah/Dropbox/Johns Hopkins/Mobility project/R/NAM_adm_shapeFiles")  # set working directory. Make sure both script and datafiles are in the same file

#*************************************************************************************
# import and create map

states.shp <- readOGR("NAM_adm1.shp") 
class(states.shp)

## check names of states
print(states.shp$NAME_1)

## input data to plot on map

num.states <- length(states.shp$NAME_1)
mydata<-data.frame(Name_1 = states.shp$NAME_1,id = states.shp$ID_1, popDensity = rnorm(num.states, 55,20))
head(mydata)

## check that data ID in data is the same as in the shape file
print(mydata$id)
print(states.shp$ID_1)

# fortify shape file to get into dataframe
states.shp.f <- fortify(states.shp, region = "ID_1")
class(states.shp.f)

head(states.shp.f)

## merge with coefficients and reorder
merge.shp.coef <- merge(states.shp.f, mydata, by="id", all.x = TRUE)
final.plot <- merge.shp.coef[order(merge.shp.coef$order), ]

#******************************************************************#
# import city locations and names
city_data <- read.csv("NAM_adm2_centroids_name_number_mapping.csv")
lat <- city_data$Y
long <- city_data$X
name <- city_data$NAME_2
ID <- city_data$ID_2
cities <- data.frame(cbind(long, lat, name, ID))

#******************************************************************#
# import trip data

mydata <- read.csv("NAM_between_day_mobility_updated.csv", check.names = FALSE) # import .cvs file; check.names is False because it was placing an X infront of date

# determine total number of people at each timepoint for standardization

totalTrips <- colSums(mydata[,-1:-2])
normData<- mydata[,-1:-2]/totalTrips * 100  # normalize raw data by the number of people who are reported to travel on a given date. Reported as a %

origin <- mydata[,1]                        # ID of the district at origin of trip
destination <-mydata[,2]                    # ID of the district at the destination of trip
days <- seq(as.Date("2010-10-02"), as.Date("2014-04-30"), by = "days")

# characteristics of dataset

dimensions = dim(normData)
pairs = dimensions[1]                          # number of possible pairs between origin and destination
dates = dimensions[2]                          # dates traveled 

districtNumber=unique(origin)                  # district number/ID  
locations = length(districtNumber)             # number of districts

#***************************************************************************************************
# matching City ID with longitude and latitude

coordinates <- matrix(ncol = 4, nrow = pairs)  #establish empty matrix for loading

for(p in 1:pairs){          # look at each entry to identify "stays", when both locations in a pair are the same
  # find ID in trip matrix
  startID = mydata$i[p]
  endID   = mydata$j[p]
  
  # find corresponding index in coordinate matrix and load into pair coordinate matrix
  start_ix = which(grepl(startID, cities$ID))
  coordinates[p,1] = cities$long[start_ix[1]] 
  coordinates[p,2] = cities$lat[start_ix[1]]
  
  end_ix = which(grepl(endID,cities$ID))
  coordinates[p,3] = cities$long[end_ix[1]]
  coordinates[p,4] = cities$lat[end_ix[1]]
}

coordinates.df <- data.frame(coordinates, check.names=FALSE)   # convert matrix of stays into a dataframe
colnames(coordinates.df)<-c("x_start", "y_start", "x_end", "y_end") # name columns

# create data frame with origin and destination IDs, coordinates, and normalized trips for each date
trips.df <- data.frame(cbind(origin,destination, coordinates, normData))   #add column for District ID

#*****************************************************************************************************
## plotting everything


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
  geom_text(data=cities, aes(long, lat, label = name), size=3, fontface="bold")+
  geom_segment(data = coordinates.df, aes(x = x_start, y = y_start, xend = x_end, yend = y_end))
