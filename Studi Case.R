#IMPORT PACKAGES AND LIBRARY
#Import Packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
#Use Library
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(ggplot2)

#Located CSV file
setwd("C:/Users/asus/Desktop/Case Cylistic/data")

#DATA PREPARATIOn
#Data Extraction From January 2021 to Desember 2021
tripdata_jan_2021 <- read.csv("202101-divvy-tripdata.csv")
tripdata_feb_2021 <- read.csv("202102-divvy-tripdata.csv")
tripdata_mar_2021 <- read.csv("202103-divvy-tripdata.csv")
tripdata_apr_2021 <- read.csv("202104-divvy-tripdata.csv")
tripdata_may_2021 <- read.csv("202105-divvy-tripdata.csv")
tripdata_jun_2021 <- read.csv("202106-divvy-tripdata.csv")
tripdata_jul_2021 <- read.csv("202107-divvy-tripdata.csv")
tripdata_aug_2021 <- read.csv("202108-divvy-tripdata.csv")
tripdata_sep_2021 <- read.csv("202109-divvy-tripdata.csv")
tripdata_oct_2021 <- read.csv("202110-divvy-tripdata.csv")
tripdata_nov_2021 <- read.csv("202111-divvy-tripdata.csv")
tripdata_des_2021 <- read.csv("202112-divvy-tripdata.csv")
#Merge Data to 1 Dataset
tripdata_2021 <- rbind(tripdata_jan_2021, tripdata_feb_2021, tripdata_mar_2021, tripdata_may_2021, tripdata_jun_2021, tripdata_jul_2021, tripdata_aug_2021, tripdata_sep_2021, tripdata_oct_2021, tripdata_nov_2021, tripdata_des_2021)

#Data Knowledge
str(tripdata_2021) #Check the structure of data
head(tripdata_2021, 10) # See the top 10 list of data
#Selecting data data for analysis
data2021 <- tripdata_2021 %>%
  select(ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual )
str(data2021)
head(data2021, 10)

#DATA CLEANING

#Checking Mising Data
colSums(is.na(data2021))
#Checking Duplicated Data

sum(duplicated(data2021))
#Change data type for started_at and ended_at from string to date
data2021$started_at <- as.POSIXct(data2021$started_at, tz="UTC")
data2021$ended_at <- as.POSIXct(data2021$ended_at, tz="UTC")
str(data2021)

#Make colomn ride_lenght 
data2021$ride_length<- as.numeric(difftime(data2021$ended_at, data2021$started_at))# ride length is from 
head(data2021,10)
data2021$ride_length<- round(data2021$ride_length/60) #convert ride_length from second to minute
head(data2021, 10)

#Make colomn day of week
data2021$days_of_week <- weekdays(data2021$started_at)

head(data2021, 10)
#Make Column month
data2021$month <- strftime(data2021$started_at, "%m")

#Make time_category
#get_time_category is the function to 
get_time_category <- function(timestamps){
  hour <- as.numeric(strftime(timestamps, "%H"))
  if (hour >= 0 && hour < 4) {
    return("00.00 - 04.00")
  } else if (hour >= 4 && hour < 8) {
    return("04.00 - 08.00")
  } else if (hour >= 8 && hour < 12) {
    return("08.00 - 12.00")
  }else if (hour >= 12 && hour < 16) {
    return("12.00 - 16.00")
  }else if (hour >= 16 && hour < 20){
    return("16.00 - 20.00")
  } else {
    return("20.00 - 24.00")
  }
}
data2021$time_category <- sapply(data2021$started_at, get_time_category)#apply the get_time_category funtion to colomn started_at to get colomn time_category

#Data ANALYSIS
#create a cystic user report in a year
report_year <- data2021 %>%
  group_by(month, member_casual) %>%
  summarize(
    number_of_ride = n()
  )
view(report_year)
#create weekly cystic user reports
report_days <- data2021 %>%
  group_by(days_of_week, member_casual) %>%
  summarize(
    avg_duration = round(mean(ride_length)),
    number_of_ride = n()
  )
view(report_days)
#create reprot of behavior from casual users
data_filter <- data2021 %>%
  filter(member_casual == "casual")
report_time <- data_filter %>%
  group_by(days_of_week, time_category) %>%
  summarize(number_of_ride = n(), avg_duration = round(mean(ride_length)))

#Visualize annual report
ggplot(report_year, aes(x = month, y = number_of_ride, colour = member_casual)) + 
  geom_line() +  
  scale_x_continuous(breaks = 1:12) + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Bulan", y = "Total Perjalanan", colour = "Tipe Anggota"
  )
#visaulize weekly report
ggplot(report_days, aes(x=days_of_week, y = number_of_ride, fill= member_casual))+
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Hari", y = "Total Perjalanan")
ggplot(report_days, aes(x=days_of_week, y = avg_duration, fill= member_casual))+
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Hari", y = "Rata Lama Perjalanan(menit)")
#visualize Behaviour from casual users
ggplot(report_time, aes(x = days_of_week, y = time_category, fill = number_of_ride)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")+
  labs(x = "Hari", y = "Waktu", fill = "Total Perjalanan")