install.packages('ggplot2')
library(ggplot2)
library(dplyr)
library(lubridate)

weather = read.csv("/cloud/project/campus_weather.csv", na.strings = '#N/A')

metaDat = read.csv("/cloud/project/meter_weather_metadata.csv", na.strings = '#N/A')

sensorLog = read.csv("/cloud/project/Sensor log.csv", na.strings = '#N/A')

sensorLog[4, 2]

average <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)

#Prompt 1

weather$dateF <- mdy_hm(weather$Date)

weather$doy <- yday(weather$dateF)

weather$year <- year(weather$dateF)

ggplot(data=weather[weather$doy >= 121 & weather$doy <= 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line()

ggplot(data=weather[weather$doy >= 152 & weather$doy <= 181 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line()

ggplot(data=weather,
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="springgreen4")+
  theme_classic()

#We don't see any serious issues, to some extent what we'd expect

#Prompt 2

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)

#Daylight savings messes up collection, we have 5 hour gaps

#Question 1

temp_above = weather[weather$AirTemp > 0,]

count(weather) - count(temp_above)

########################################### In class prompt stuff

intv = weather$dateF[1] %--% weather$dateF[2]

int_length(intv)

weather$dateF[-1]

intervals = weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]

interval_times = int_length(intervals)

intervals[interval_times != 900]

###########################################

#Question 2

weather$VoltFlag <- ifelse(weather$BatVolt < 8.5, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero
min(weather$BatVolt)

#Use the ifelse function to find if values below a certain level are present

#Question 3

tempCheck <- function(x){
  temp = weather$AirTemp[weather$Date == x]
  ifelse(temp > 35 || temp < -25,
         1,
         0)
}

solCheck <- function(x){
  Sol = weather$SolRad[weather$Date == x]
  ifelse(Sol > 1050 || Sol < 0,
         1,
         0)
}


#Question 4

ggplot(data=weather[weather$doy > 0 & weather$doy < 89 & weather$year > 2020 & weather$year < 2022 ,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()

ggplot(data=weather[weather$doy > 0 & weather$doy < 100 & weather$year > 2021 & weather$year < 2023 ,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()
