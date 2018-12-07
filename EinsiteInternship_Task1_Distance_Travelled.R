library(sqldf)
library(geosphere)
library(dplyr)
library('sf')

rm(list=ls(all=TRUE))
setwd("C:\\Users\\Gopal Mukkamala\\Desktop\\Einsite Project")

TotalData <- read.csv("Data_for_intern_project_1.csv", header = T, sep = ",")

#Data Cleaning
#Extracting good log values only
GoodLogData <- sqldf("SELECT * from TotalData
                     WHERE status_code = 61445")

#No. of NA's per column -> No NA's in the data set
colSums(is.na(GoodLogData))


#Removing records not having Latitude Longitude values
GoodLogLatLong_Data <- sqldf("SELECT * from GoodLogData
                     WHERE latitude_gps <> 0 and longitude_gps <> 0")


#Count of equipments
Equipments <- sqldf("SELECT asset_type , COUNT(asset_type) from GoodLogLatLong_Data
                    GROUP BY asset_type") 


#Function to calculate distance between points
earth.dist <- function (lat1,long1,lat2,long2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#Selecting relavant attributes
Dataset <- sqldf("SELECT asset_type,latitude_gps , longitude_gps , device_time_stamp
                  FROM GoodLogLatLong_Data")

#Movable Equipment type 1
EquipOneDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '4%' ")

DayOneEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE device_time_stamp LIKE '12/13%' ")
DayOneEquipOne$latitude_gps <- as.numeric(as.character(DayOneEquipOne$latitude_gps))
DayOneEquipOne$longitude_gps <- as.numeric(as.character(DayOneEquipOne$longitude_gps))
totalDistance = 0
for(i in 1:length(DayOneEquipOne$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayOneEquipOne$latitude_gps[i],DayOneEquipOne$longitude_gps[i],
                                              DayOneEquipOne$latitude_gps[i+1],DayOneEquipOne$longitude_gps[i+1]))
  print(totalDistance)
}

DayTwoEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE device_time_stamp LIKE '12/14%' ")
DayTwoEquipOne$latitude_gps <- as.numeric(as.character(DayTwoEquipOne$latitude_gps))
DayTwoEquipOne$longitude_gps <- as.numeric(as.character(DayTwoEquipOne$longitude_gps))
totalDistance = 0
for(i in 1:length(DayTwoEquipOne$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayTwoEquipOne$latitude_gps[i],DayTwoEquipOne$longitude_gps[i],
                                              DayTwoEquipOne$latitude_gps[i+1],DayTwoEquipOne$longitude_gps[i+1]))
  print(totalDistance)
}

DayThreeEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE device_time_stamp LIKE '12/15%' ")
DayThreeEquipOne$latitude_gps <- as.numeric(as.character(DayThreeEquipOne$latitude_gps))
DayThreeEquipOne$longitude_gps <- as.numeric(as.character(DayThreeEquipOne$longitude_gps))
totalDistance = 0
for(i in 1:length(DayThreeEquipOne$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayThreeEquipOne$latitude_gps[i],DayThreeEquipOne$longitude_gps[i],
                                              DayThreeEquipOne$latitude_gps[i+1],DayThreeEquipOne$longitude_gps[i+1]))
  print(totalDistance)
}

DayFourEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE device_time_stamp LIKE '12/16%' ")
DayFourEquipOne$latitude_gps <- as.numeric(as.character(DayFourEquipOne$latitude_gps))
DayFourEquipOne$longitude_gps <- as.numeric(as.character(DayFourEquipOne$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFourEquipOne$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFourEquipOne$latitude_gps[i],DayFourEquipOne$longitude_gps[i],
                                              DayFourEquipOne$latitude_gps[i+1],DayFourEquipOne$longitude_gps[i+1]))
  print(totalDistance)
}

DayFiveEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE device_time_stamp LIKE '12/17%' ")
DayFiveEquipOne$latitude_gps <- as.numeric(as.character(DayFiveEquipOne$latitude_gps))
DayFiveEquipOne$longitude_gps <- as.numeric(as.character(DayFiveEquipOne$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFiveEquipOne$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFiveEquipOne$latitude_gps[i],DayFiveEquipOne$longitude_gps[i],
                                              DayFiveEquipOne$latitude_gps[i+1],DayFiveEquipOne$longitude_gps[i+1]))
  print(totalDistance)
}

#Movable Equipment type 2
EquipTwoDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '8%' ")

DayOneEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE device_time_stamp LIKE '12/13%' ")
DayOneEquipTwo$latitude_gps <- as.numeric(as.character(DayOneEquipTwo$latitude_gps))
DayOneEquipTwo$longitude_gps <- as.numeric(as.character(DayOneEquipTwo$longitude_gps))
totalDistance = 0
for(i in 1:length(DayOneEquipTwo$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayOneEquipTwo$latitude_gps[i],DayOneEquipTwo$longitude_gps[i],
                                              DayOneEquipTwo$latitude_gps[i+1],DayOneEquipTwo$longitude_gps[i+1]))
  print(totalDistance)
}

DayTwoEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE device_time_stamp LIKE '12/14%' ")
DayTwoEquipTwo$latitude_gps <- as.numeric(as.character(DayTwoEquipTwo$latitude_gps))
DayTwoEquipTwo$longitude_gps <- as.numeric(as.character(DayTwoEquipTwo$longitude_gps))
totalDistance = 0
for(i in 1:length(DayTwoEquipTwo$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayTwoEquipTwo$latitude_gps[i],DayTwoEquipTwo$longitude_gps[i],
                                              DayTwoEquipTwo$latitude_gps[i+1],DayTwoEquipTwo$longitude_gps[i+1]))
  print(totalDistance)
}

DayThreeEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE device_time_stamp LIKE '12/15%' ")
DayThreeEquipTwo$latitude_gps <- as.numeric(as.character(DayThreeEquipTwo$latitude_gps))
DayThreeEquipTwo$longitude_gps <- as.numeric(as.character(DayThreeEquipTwo$longitude_gps))
totalDistance = 0
for(i in 1:length(DayThreeEquipTwo$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayThreeEquipTwo$latitude_gps[i],DayThreeEquipTwo$longitude_gps[i],
                                              DayThreeEquipTwo$latitude_gps[i+1],DayThreeEquipTwo$longitude_gps[i+1]))
  print(totalDistance)
}

DayFourEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE device_time_stamp LIKE '12/16%' ")
DayFourEquipTwo$latitude_gps <- as.numeric(as.character(DayFourEquipTwo$latitude_gps))
DayFourEquipTwo$longitude_gps <- as.numeric(as.character(DayFourEquipTwo$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFourEquipTwo$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFourEquipTwo$latitude_gps[i],DayFourEquipTwo$longitude_gps[i],
                                              DayFourEquipTwo$latitude_gps[i+1],DayFourEquipTwo$longitude_gps[i+1]))
  print(totalDistance)
}

DayFiveEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE device_time_stamp LIKE '12/17%' ")
DayFiveEquipTwo$latitude_gps <- as.numeric(as.character(DayFiveEquipTwo$latitude_gps))
DayFiveEquipTwo$longitude_gps <- as.numeric(as.character(DayFiveEquipTwo$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFiveEquipTwo$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFiveEquipTwo$latitude_gps[i],DayFiveEquipTwo$longitude_gps[i],
                                              DayFiveEquipTwo$latitude_gps[i+1],DayFiveEquipTwo$longitude_gps[i+1]))
  print(totalDistance)
}

#Stationary Equipment type 1
EquipThreeDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '6%' ")

DayOneEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE device_time_stamp LIKE '12/13%' ")
DayOneEquipThree$latitude_gps <- as.numeric(as.character(DayOneEquipThree$latitude_gps))
DayOneEquipThree$longitude_gps <- as.numeric(as.character(DayOneEquipThree$longitude_gps))
totalDistance = 0
for(i in 1:length(DayOneEquipThree$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayOneEquipThree$latitude_gps[i],DayOneEquipThree$longitude_gps[i],
                                              DayOneEquipThree$latitude_gps[i+1],DayOneEquipThree$longitude_gps[i+1]))
  print(totalDistance)
}

DayTwoEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE device_time_stamp LIKE '12/14%' ")
DayTwoEquipThree$latitude_gps <- as.numeric(as.character(DayTwoEquipThree$latitude_gps))
DayTwoEquipThree$longitude_gps <- as.numeric(as.character(DayTwoEquipThree$longitude_gps))
totalDistance = 0
for(i in 1:length(DayTwoEquipThree$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayTwoEquipThree$latitude_gps[i],DayTwoEquipThree$longitude_gps[i],
                                              DayTwoEquipThree$latitude_gps[i+1],DayTwoEquipThree$longitude_gps[i+1]))
  print(totalDistance)
}

DayThreeEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE device_time_stamp LIKE '12/15%' ")
DayThreeEquipThree$latitude_gps <- as.numeric(as.character(DayThreeEquipThree$latitude_gps))
DayThreeEquipThree$longitude_gps <- as.numeric(as.character(DayThreeEquipThree$longitude_gps))
totalDistance = 0
for(i in 1:length(DayThreeEquipThree$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayThreeEquipThree$latitude_gps[i],DayThreeEquipThree$longitude_gps[i],
                                              DayThreeEquipThree$latitude_gps[i+1],DayThreeEquipThree$longitude_gps[i+1]))
  print(totalDistance)
}

DayFourEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE device_time_stamp LIKE '12/16%' ")
DayFourEquipThree$latitude_gps <- as.numeric(as.character(DayFourEquipThree$latitude_gps))
DayFourEquipThree$longitude_gps <- as.numeric(as.character(DayFourEquipThree$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFourEquipThree$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFourEquipThree$latitude_gps[i],DayFourEquipThree$longitude_gps[i],
                                              DayFourEquipThree$latitude_gps[i+1],DayFourEquipThree$longitude_gps[i+1]))
  print(totalDistance)
}

DayFiveEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE device_time_stamp LIKE '12/17%' ")
DayFiveEquipThree$latitude_gps <- as.numeric(as.character(DayFiveEquipThree$latitude_gps))
DayFiveEquipThree$longitude_gps <- as.numeric(as.character(DayFiveEquipThree$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFiveEquipThree$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFiveEquipThree$latitude_gps[i],DayFiveEquipThree$longitude_gps[i],
                                              DayFiveEquipThree$latitude_gps[i+1],DayFiveEquipThree$longitude_gps[i+1]))
  print(totalDistance)
}

#Stationary Equipment type 2
EquipFourDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '7%' ")

DayOneEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE device_time_stamp LIKE '12/13%' ")
DayOneEquipFour$latitude_gps <- as.numeric(as.character(DayOneEquipFour$latitude_gps))
DayOneEquipFour$longitude_gps <- as.numeric(as.character(DayOneEquipFour$longitude_gps))
totalDistance = 0
for(i in 1:length(DayOneEquipFour$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayOneEquipFour$latitude_gps[i],DayOneEquipFour$longitude_gps[i],
                                              DayOneEquipFour$latitude_gps[i+1],DayOneEquipFour$longitude_gps[i+1]))
  print(totalDistance)
}

DayTwoEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE device_time_stamp LIKE '12/14%' ")
DayTwoEquipFour$latitude_gps <- as.numeric(as.character(DayTwoEquipFour$latitude_gps))
DayTwoEquipFour$longitude_gps <- as.numeric(as.character(DayTwoEquipFour$longitude_gps))
totalDistance = 0
for(i in 1:length(DayTwoEquipFour$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayTwoEquipFour$latitude_gps[i],DayTwoEquipFour$longitude_gps[i],
                                              DayTwoEquipFour$latitude_gps[i+1],DayTwoEquipFour$longitude_gps[i+1]))
  print(totalDistance)
}

DayThreeEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE device_time_stamp LIKE '12/15%' ")
DayThreeEquipFour$latitude_gps <- as.numeric(as.character(DayThreeEquipFour$latitude_gps))
DayThreeEquipFour$longitude_gps <- as.numeric(as.character(DayThreeEquipFour$longitude_gps))
totalDistance = 0
for(i in 1:length(DayThreeEquipFour$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayThreeEquipFour$latitude_gps[i],DayThreeEquipFour$longitude_gps[i],
                                              DayThreeEquipFour$latitude_gps[i+1],DayThreeEquipFour$longitude_gps[i+1]))
  print(totalDistance)
}

DayFourEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE device_time_stamp LIKE '12/16%' ")
DayFourEquipFour$latitude_gps <- as.numeric(as.character(DayFourEquipFour$latitude_gps))
DayFourEquipFour$longitude_gps <- as.numeric(as.character(DayFourEquipFour$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFourEquipFour$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFourEquipFour$latitude_gps[i],DayFourEquipFour$longitude_gps[i],
                                              DayFourEquipFour$latitude_gps[i+1],DayFourEquipFour$longitude_gps[i+1]))
  print(totalDistance)
}

DayFiveEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE device_time_stamp LIKE '12/17%' ")
DayFiveEquipFour$latitude_gps <- as.numeric(as.character(DayFiveEquipFour$latitude_gps))
DayFiveEquipFour$longitude_gps <- as.numeric(as.character(DayFiveEquipFour$longitude_gps))
totalDistance = 0
for(i in 1:length(DayFiveEquipFour$latitude_gps))
{
  totalDistance = totalDistance + (earth.dist(DayFiveEquipFour$latitude_gps[i],DayFiveEquipFour$longitude_gps[i],
                                              DayFiveEquipFour$latitude_gps[i+1],DayFiveEquipFour$longitude_gps[i+1]))
  print(totalDistance)
}



