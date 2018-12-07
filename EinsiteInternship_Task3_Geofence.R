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

Dataset <- sqldf("SELECT asset_type,latitude_gps , longitude_gps , device_time_stamp
                  FROM GoodLogLatLong_Data")

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


DayOneDataset <- sqldf("SELECT * FROM Dataset
                       WHERE Date LIKE '12/13%' ")

#Selecting Lat Long values b/w 9.00-9.10
LatLongOneDataset <- sqldf("SELECT asset_type,longitude_gps,latitude_gps,Time
                           FROM DayOneDataset
                           WHERE Time LIKE ('09:0%')")


#Coverting to Required Format
LatLongOne = paste("[","'",LatLongOneDataset$asset_type,"'",",",LatLongOneDataset$latitude_gps,",",LatLongOneDataset$longitude_gps,"]",",", sep = '')
write.csv(LatLongOne,"LatLongOne.csv")


DayTwoDataset <- sqldf("SELECT * FROM Dataset
                       WHERE Date LIKE '12/14%' ")

#Selecting Lat Long values b/w 9.00-9.10
LatLongTwoDataset <- sqldf("SELECT asset_type,longitude_gps,latitude_gps,Time
                           FROM DayTwoDataset
                           WHERE Time LIKE ('09:0%')")

#Coverting to Required Format
LatLongTwo = paste("[","'",LatLongTwoDataset$asset_type,"'",",",LatLongTwoDataset$latitude_gps,",",LatLongTwoDataset$longitude_gps,"]",",", sep = '')
write.csv(LatLongTwo,"LatLongTwo.csv")


DayThreeDataset <- sqldf("SELECT * FROM Dataset
                         WHERE Date LIKE '12/15%' ")

#Selecting Lat Long values b/w 9.00-9.10
LatLongThreeDataset <- sqldf("SELECT asset_type,longitude_gps,latitude_gps,Time
                           FROM DayThreeDataset
                           WHERE Time LIKE ('09:0%')")

#Coverting to Required Format
LatLongThree = paste("[","'",LatLongThreeDataset$asset_type,"'",",",LatLongThreeDataset$latitude_gps,",",LatLongThreeDataset$longitude_gps,"]",",", sep = '')
write.csv(LatLongThree,"LatLongThree.csv")




DayFourDataset <- sqldf("SELECT * FROM Dataset
                        WHERE Date LIKE '12/16%' ")

#Selecting Lat Long values b/w 9.00-9.10
LatLongFourDataset <- sqldf("SELECT asset_type,longitude_gps,latitude_gps,Time
                           FROM DayFourDataset
                             WHERE Time LIKE ('09:0%')")

#Coverting to Required Format
LatLongFour = paste("[","'",LatLongFourDataset$asset_type,"'",",",LatLongFourDataset$latitude_gps,",",LatLongFourDataset$longitude_gps,"]",",", sep = '')
write.csv(LatLongFour,"LatLongFour.csv")


DayFiveDataset <- sqldf("SELECT * FROM Dataset
                        WHERE Date LIKE '12/17%' ")

#Selecting Lat Long values b/w 9.00-9.10
LatLongFiveDataset <- sqldf("SELECT asset_type,longitude_gps,latitude_gps,Time
                           FROM DayFiveDataset
                            WHERE Time LIKE ('09:0%')")

#Coverting to Required Format
LatLongFive = paste("[","'",LatLongFiveDataset$asset_type,"'",",",LatLongFiveDataset$latitude_gps,",",LatLongFiveDataset$longitude_gps,"]",",", sep = '')
write.csv(LatLongFive,"LatLongFive.csv")



