setwd("C:/Users/smcnish/Documents/Vizs/Crime/DC-Crime/raw data/")

library(ggplot2)
library(plyr)
library(dplyr)

#append all 8 files together
file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}



#clean up variables

dataset$hour = format(as.POSIXct(dataset$REPORT_DAT, format = "%m/%d/%Y %I:%M:%S %p"),"%I")
dataset$day <- format(as.Date(dataset$REPORT_DAT,format="%Y-%m-%d %I:%M:%S %p"), "%d")        # day of month
dataset$month <- format(as.Date(dataset$REPORT_DAT,format="%Y-%m-%d %I:%M:%S %p"), "%m")      # month of year 
dataset$year <- format(as.Date(dataset$REPORT_DAT,format="%Y-%m-%d %I:%M:%S %p"), "%y")   # year
dataset$START_DATE<- as.Date(dataset$START_DATE, format = "%m/%d/%Y %I:%M:%S %p")
dataset$END_DATE<- as.Date(dataset$END_DATE, format = "%m/%d/%Y %I:%M:%S %p")
dataset$date<- as.Date(dataset$REPORT_DAT, format = "%m/%d/%Y %I:%M:%S %p")
dataset$DayofWeek <- weekdays(dataset$date)

#filter out 2017
dataset<-dataset[dataset$year != '17',]

#Create dataset of crime by day
df_daily <- dataset %>%
  group_by(date) %>% 
  summarize(count = n()) %>%
  arrange(date)

#Plot crime by day
ggplot(df_daily, aes(x = date, y = count)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(date_breaks="2 years", date_labels = ("%Y")) +
  labs(x = "Date of Crime", y = "# of Crimes", title = "Daily Crimes in DC from 2008 - 2016")

#summarize crime by day of the week and hour
df_time <- dataset %>%
  group_by(DayofWeek, hour) %>%
  summarize(count = n())
