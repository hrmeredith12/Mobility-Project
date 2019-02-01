## create master matrix for Namibia:
# data includes: Origin ID / Origin Name / Origin longitude / Origin latitude / Origin population density /
#                Destination ID / Destination Name / Dest. longitude/ Dest. latitude/ Dest population density/
#                trip # for each date
# created by Hannah Meredith
# last updated February 1, 2019

setwd("/Users/Hannah/Dropbox/Johns Hopkins/Mobility project/R/compiling data set")  # set working directory. Make sure both script and datafiles are in the same file
# install.packages("gdata")
# install.packages("reshape")
# 
# library(gdata) # library to import data
# library(reshape)
 
#*********************************************************************************************************
# import the relevant datasets: trips; coordinates and city names; population density

trips <- read.csv("NAM_between_day_mobility_updated.csv", check.names = FALSE) # import .cvs file; check.names is False because it was placing an X infront of date
city_data <- read.csv("NAM_adm2_centroids_name_number_mapping.csv")
popdensity <- read.csv ("NAM_pop_from_worldpop_adm2.csv")

# Pull characteristics from trip file: 
origin <- trips[,1]                        # ID of the district at origin of trip
destination <-trips[,2]                    # ID of the district at the destination of trip
days <- seq(as.Date("2010-10-02"), as.Date("2014-04-30"), by = "days")  # time window of record
dimensions = dim(trips)
pairs = dimensions[1]                          # number of possible pairs between origin and destination

# Standardize trip numbers 
totalTrips <- colSums(trips[,-1:-2])         # calculate total trip number per day
normTrips<- trips[,-1:-2]/totalTrips * 100  # normalize raw data by the number of people who are reported to travel on a given date. Reported as a %
normTrips.df <- data.frame(normTrips)      
colnames(normTrips.df) <- days

# Pull characteristics from city file
cities <- data.frame(city_data$NAME_2, city_data$ID_2,city_data$X, city_data$Y)
colnames(cities) <- cbind("name", "ID", "long", "lat" )

# Pull characteristics from population density file
population <- data.frame(popdensity$NAME_2, popdensity$ID_2, popdensity$popsum)
colnames(population) <- cbind("name", "ID", "PopulationDensity")


# ***************************************************************************************************
# matching City ID with longitude and latitude and population density
# name_start <- factor(c(ncol = 1, nrow = pairs))
x_start <- matrix(ncol = 1, nrow = pairs)
y_start <- matrix(ncol = 1, nrow = pairs)
pop_start <- matrix(ncol = 1, nrow = pairs)
# name_end <- matrix(ncol = 1, nrow = pairs)
x_end <- matrix(ncol = 1, nrow = pairs)
y_end <- matrix(ncol = 1, nrow = pairs)
pop_end <- matrix(ncol = 1, nrow = pairs)


for(p in 1:pairs){          # loop through each pair of cities to load in name, coordinates and pop density
  
  startID = origin[p]                               # find origin ID in trip matrix
  start_city_ind = which(grepl(startID, cities$ID)) # find origin ID's corresponding index in cities matrix
  # name_start[p] <- as.string(cities$name[start_city_ind[1]])    # name of origin city
  x_start[p] = cities$long[start_city_ind[1]]       # longitude of origin city
  y_start[p] = cities$lat[start_city_ind[1]]        # latitude of origin city
  start_pop_ind = which(grepl(startID, population$ID))  # find origin ID's corresponding index in population matrix
  pop_start[p] = population$PopulationDensity[start_city_ind[1]] # population density of origin city
  
  
  endID   = destination[p]                         # find destination ID in trip matrix
  end_city_ind = which(grepl(endID, cities$ID)) # find destination ID's corresponding index in cities matrix
  # name_end[p] = cities$name[end_city_ind[1]]    # name of destination city
  x_end[p] = cities$long[end_city_ind[1]]       # longitude of destination city
  y_end[p] = cities$lat[end_city_ind[1]]        # latitude of destination city
  end_pop_ind = which(grepl(endID, population$ID))  # find destination ID's corresponding index in population matrix
  pop_end[p] = population$PopulationDensity[end_city_ind[1]] # population density of destination city
  
  # end_city = which(grepl(endID,cities$ID))
  # coordinates[p,3] = cities$long[end_ix[1]]
  # coordinates[p,4] = cities$lat[end_ix[1]]
  
  
}

# NAM_master_matrix <- data.frame(origin, name_start, x_start, y_start, pop_start, destination, name_end, x_end, y_end, pop_end, normTrips.df)
NAM_master_matrix <- data.frame(origin, x_start, y_start, pop_start, destination, x_end, y_end, pop_end, normTrips.df)

# save dataframe as csv
write.csv(NAM_master_matrix, file = "NAM_masterdata.csv")