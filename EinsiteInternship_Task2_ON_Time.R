library(sqldf)
library(geosphere)
library(dplyr)
library('sf')
library(tidyr)
library(lubridate)
library(chron)

rm(list=ls(all=TRUE))
setwd("C:\\Users\\Gopal Mukkamala\\Desktop\\Einsite Project")

TotalData <- read.csv("Data_for_intern_project_1.csv", header = T, sep = ",")

#Data Cleaning

#Extracting good log values only
GoodLogData <- sqldf("SELECT * from TotalData
                     WHERE status_code = 61445")

#No. of NA's per column -> No NA's in the data set
colSums(is.na(GoodLogData))


#Converting time_stamp into proper format
GoodLogData$device_time_stamp <- format(GoodLogData$device_time_stamp,
                                            format = '%Y/%m/%d %H:%M')


#Selecting the relavant attributes
Dataset <- sqldf("SELECT asset_type, device_time_stamp, track_num1
                  FROM GoodLogData")


#Extracting records where ignition=ON i.e., track_num1 = 1
Dataset <- sqldf("SELECT asset_type, device_time_stamp, track_num1
                  FROM GoodLogData
                  WHERE track_num1=1")


#Seperating the Date & Time in device_time_stamp
Dataset <- separate(data = Dataset, col = device_time_stamp, into = c("Date", "Time"), sep = " ")


#Converting time into proper format(HH:MM)

appendZero = function(df){
  paste0("0",df)
}
for(row in 1:length(Dataset$Time))
{
  if(nchar(Dataset$Time[row]) == 4)
    {
      Dataset$Time[row] =  appendZero(Dataset$Time[row])
    }
  else
    {
      next
    }
}


##Equipment Wise Data

#Movable Equipment type 1
EquipOneDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '4%' ")

DayOneEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE Date LIKE '12/13%' ")
DayTwoEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE Date LIKE '12/14%' ")
DayThreeEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE Date LIKE '12/15%' ")
DayFourEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE Date LIKE '12/16%' ")
DayFiveEquipOne <- sqldf("SELECT * FROM EquipOneDataset WHERE Date LIKE '12/17%' ")

DayOneEquipOne$Time = strptime(DayOneEquipOne$Time, format = "%H:%M")
DayOneEquipOneDiff = difftime(max(DayOneEquipOne$Time) , min(DayOneEquipOne$Time))
DayTwoEquipOne$Time = strptime(DayTwoEquipOne$Time, format = "%H:%M")
DayTwoEquipOneDiff = difftime(max(DayTwoEquipOne$Time) , min(DayTwoEquipOne$Time))
DayThreeEquipOne$Time = strptime(DayThreeEquipOne$Time, format = "%H:%M")
DayThreeEquipOneDiff = difftime(max(DayThreeEquipOne$Time) , min(DayThreeEquipOne$Time))
DayFourEquipOne$Time = strptime(DayFourEquipOne$Time, format = "%H:%M")
DayFourEquipOneDiff = difftime(max(DayFourEquipOne$Time) , min(DayThreeEquipOne$Time))
DayFiveEquipOne$Time = strptime(DayFiveEquipOne$Time, format = "%H:%M")
DayFiveEquipOneDiff = difftime(max(DayFiveEquipOne$Time) , min(DayFiveEquipOne$Time))

TotalEquipOne = as.numeric(DayOneEquipOneDiff, units = "hours")+as.numeric(DayTwoEquipOneDiff, units = "hours")+
                as.numeric(DayThreeEquipOneDiff, units = "hours")+as.numeric(DayFourEquipOneDiff, units = "hours")+
                as.numeric(DayFiveEquipOneDiff, units = "hours")

#Movable Equipment type 2
EquipTwoDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '8%' ")

DayOneEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE Date LIKE '12/13%' ")
DayTwoEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE Date LIKE '12/14%' ")
DayThreeEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE Date LIKE '12/15%' ")
DayFourEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE Date LIKE '12/16%' ")
DayFiveEquipTwo <- sqldf("SELECT * FROM EquipTwoDataset WHERE Date LIKE '12/17%' ")

DayOneEquipTwo$Time = strptime(DayOneEquipTwo$Time, format = "%H:%M")
DayOneEquipTwoDiff = difftime(max(DayOneEquipTwo$Time) , min(DayOneEquipTwo$Time))
DayTwoEquipTwo$Time = strptime(DayTwoEquipTwo$Time, format = "%H:%M")
DayTwoEquipTwoDiff = difftime(max(DayTwoEquipTwo$Time) , min(DayTwoEquipTwo$Time))
DayThreeEquipTwo$Time = strptime(DayThreeEquipTwo$Time, format = "%H:%M")
DayThreeEquipTwoDiff = difftime(max(DayThreeEquipTwo$Time) , min(DayThreeEquipTwo$Time))
DayFourEquipTwo$Time = strptime(DayFourEquipTwo$Time, format = "%H:%M")
DayFourEquipTwoDiff = difftime(max(DayFourEquipTwo$Time) , min(DayThreeEquipTwo$Time))
DayFiveEquipTwo$Time = strptime(DayFiveEquipTwo$Time, format = "%H:%M")
DayFiveEquipTwoDiff = difftime(max(DayFiveEquipTwo$Time) , min(DayFiveEquipTwo$Time))

TotalEquipTwo = as.numeric(DayOneEquipTwoDiff, units = "hours")+as.numeric(DayTwoEquipTwoDiff, units = "hours")+
  as.numeric(DayThreeEquipTwoDiff, units = "hours")+as.numeric(DayFourEquipTwoDiff, units = "hours")+
  as.numeric(DayFiveEquipTwoDiff, units = "hours")


#Stationery Equipment type 1
EquipThreeDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '6%' ")

DayOneEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE Date LIKE '12/13%' ")
DayTwoEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE Date LIKE '12/14%' ")
DayThreeEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE Date LIKE '12/15%' ")
DayFourEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE Date LIKE '12/16%' ")
DayFiveEquipThree <- sqldf("SELECT * FROM EquipThreeDataset WHERE Date LIKE '12/17%' ")

DayOneEquipThree$Time = strptime(DayOneEquipThree$Time, format = "%H:%M")
DayOneEquipThreeDiff = difftime(max(DayOneEquipThree$Time) , min(DayOneEquipThree$Time))
DayTwoEquipThree$Time = strptime(DayTwoEquipThree$Time, format = "%H:%M")
DayTwoEquipThreeDiff = difftime(max(DayTwoEquipThree$Time) , min(DayTwoEquipThree$Time))
DayThreeEquipThree$Time = strptime(DayThreeEquipThree$Time, format = "%H:%M")
DayThreeEquipThreeDiff = difftime(max(DayThreeEquipThree$Time) , min(DayThreeEquipThree$Time))
DayFourEquipThree$Time = strptime(DayFourEquipThree$Time, format = "%H:%M")
DayFourEquipThreeDiff = difftime(max(DayFourEquipThree$Time) , min(DayThreeEquipThree$Time))
DayFiveEquipThree$Time = strptime(DayFiveEquipThree$Time, format = "%H:%M")
DayFiveEquipThreeDiff = difftime(max(DayFiveEquipThree$Time) , min(DayFiveEquipThree$Time))

TotalEquipThree = as.numeric(DayOneEquipThreeDiff, units = "hours")+as.numeric(DayTwoEquipThreeDiff, units = "hours")+
  as.numeric(DayThreeEquipThreeDiff, units = "hours")


#Stationery Equipment type 2
EquipFourDataset <- sqldf("SELECT * FROM Dataset WHERE asset_type LIKE '7%' ")

DayOneEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE Date LIKE '12/13%' ")
DayTwoEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE Date LIKE '12/14%' ")
DayThreeEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE Date LIKE '12/15%' ")
DayFourEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE Date LIKE '12/16%' ")
DayFiveEquipFour <- sqldf("SELECT * FROM EquipFourDataset WHERE Date LIKE '12/17%' ")

DayOneEquipFour$Time = strptime(DayOneEquipFour$Time, format = "%H:%M")
DayOneEquipFourDiff = difftime(max(DayOneEquipFour$Time) , min(DayOneEquipFour$Time))
DayTwoEquipFour$Time = strptime(DayTwoEquipFour$Time, format = "%H:%M")
DayTwoEquipFourDiff = difftime(max(DayTwoEquipFour$Time) , min(DayTwoEquipFour$Time))
DayThreeEquipFour$Time = strptime(DayThreeEquipFour$Time, format = "%H:%M")
DayThreeEquipFourDiff = difftime(max(DayThreeEquipFour$Time) , min(DayThreeEquipFour$Time))
DayFourEquipFour$Time = strptime(DayFourEquipFour$Time, format = "%H:%M")
DayFourEquipFourDiff = difftime(max(DayFourEquipFour$Time) , min(DayFourEquipFour$Time))
DayFiveEquipFour$Time = strptime(DayFiveEquipFour$Time, format = "%H:%M")
DayFiveEquipFourDiff = difftime(max(DayFiveEquipFour$Time) , min(DayFiveEquipFour$Time))


TotalEquipFour = as.numeric(DayOneEquipFourDiff, units = "hours")+as.numeric(DayTwoEquipFourDiff, units = "hours")+
  as.numeric(DayThreeEquipFourDiff, units = "hours")+as.numeric(DayFourEquipFourDiff, units = "hours")+
  as.numeric(DayFiveEquipFourDiff, units = "hours")


