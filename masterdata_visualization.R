## Visualizing mobility data
## input: datafile with information: IDFrom, IDTo, Timestamp, Number of trips between 2 districts
## output: line plot of stays over time for each location; 
## by Hannah Meredith
## Last updated Feb 1, 2019

setwd("/Users/Hannah/Dropbox/Johns Hopkins/Mobility project/R/visualizing NAM")  # set working directory. Make sure both script and datafiles are in the same file
# install.packages("gdata") #
# install.packages("ggplot2")
# install.packages("reshape")
library(gdata) # library to import data
library(ggplot2)
library(reshape)


mydata <- read.csv("NAM_masterdata.csv", check.names = FALSE) # import .cvs file; check.names is False because it was placing an X infront of date

# characteristics of dataset
trips <- mydata[,-1:-9]
dimensions = dim(trips)
pairs = dimensions[1]                          # number of possible pairs between origin and destination
dates = dimensions[2]                          # dates traveled 

districtNumber=unique(mydata$origin)           # district number/ID  
locations = length(districtNumber)             # number of districts
days <- seq(as.Date("2010-10-02"), as.Date("2014-04-30"), by = "days")

# ************************************************************************************
# Number of stays

counter <- 1 # counts through number of possible "stay" pairs
stays <- matrix(ncol = dates, nrow = locations)  #establish empty matrix for loading

for (d in 1:dates){           # if looking at number of stays for entire time window, then loop through all the dates 
  counter <- 1                # reset counter to 1 for next date
  for(p in 1:pairs){          # look at each entry to identify "stays", when both locations in a pair are the same
    if (mydata$origin[p] == mydata$destination[p]){  # if a stay occured, then record the number of stays for that given location and date
      stays[counter,d]= trips[p,d]  # add 2 to date index to account for i+j columns
        if (counter < locations){
        counter <- counter + 1  # update location counter
      } 
    }
  }
}

stays.df <- data.frame(stays, check.names=FALSE)   # convert matrix of stays into a dataframe
colnames(stays.df) <- days

stays.df <- cbind(districtNumber, stays.df)   #add column for District ID
stays_long <- melt(stays.df, id.vars = "districtNumber")   # melt matrix into long form
stays_long$variable <- as.Date(stays_long$variable)#, "%Y/%m/%d")  # convert date from factor to date form
stays_long.df <- data.frame(stays_long)                         # convert into dataframe   
colnames(stays_long.df)<- c("DistrictNumber", "Date", "Stays")  # name columns

## create bar chart
barplot(stays[,100],names.arg = districtNumber, xlab="locations", ylab="Normalized number of stays (%)") # plots a barchart looking at number of stays for each location on a given day
barplot(stays[100,], xlab="date", ylab="Normalized number of stays (%)") # plots a barchart looking at number of stays over time for a given location
plot(stays[100,], xlab="date", ylab="Normalized number of stays (%)", type = "l") # plots a barchart looking at number of stays over time for a given location


## create line plot to compare all cities
ggplot(stays_long.df, aes(x=Date,y=Stays, colour = as.factor(DistrictNumber)))+
  geom_line()+
  ylab(label = "Number of Stays (%)") +
  xlab(label = "Date")
  #scale_y_continuous(trans = 'log10')


## subset data to grab top 10 locations with most stays
sub_stays <- data.frame(subset(stays_long.df, Stays> 7))

ggplot(sub_stays, aes(x=Date,y=Stays, colour = as.factor(DistrictNumber)))+
  geom_line() +
  ylab(label = "Number of Stays (%)") +
  xlab(label = "Date")+
  scale_y_continuous(trans = 'log10')

#*********************************************************************************