setwd("C:/Users/smcnish/Documents/Vizs/Crime/DC-Crime/raw data/")

library(ggplot2)
library(plyr)
library(dplyr)
library(extrafont)


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

#remove variables don't want
delme <- c("SHIFT","START_DATE","END_DATE","CCN")
dataset<- dataset[,-which(names(dataset) %in% delme)]

#clean up variables

dataset$REPORT_DAT <- strptime(dataset$REPORT_DAT, format = "%m/%d/%Y %I:%M:%S %p")

dataset$date<- as.Date(dataset$REPORT_DAT, format = "%m/%d/%Y %I:%M:%S %p")
dataset$DayofWeek <- weekdays(dataset$date)

dataset$hour <- substr(dataset$REPORT_DAT,12,13)     #hour
dataset$day <- substr(dataset$REPORT_DAT,9,10)    # day of month
dataset$month <- substr(dataset$REPORT_DAT,6,7)      # month of year 
dataset$year <- substr(dataset$REPORT_DAT,1,4)     # year


#filter out 2017 because we only had a few dates
dataset<-dataset[dataset$year != '2017',]

#Create dataset of crime by day
df_daily <- dataset[,-which(names(dataset) %in% c("REPORT_DAT"))] %>%
  group_by(date) %>% 
  summarize(count = n()) %>%
  arrange(date)

#Plot crime by day
ggplot(df_daily, aes(x = date, y = count)) +
  geom_line(color = "#45b293", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  scale_x_date(date_breaks="1 year", date_labels = ("%Y")) +
  labs(x = "Date of Crime", y = "# of Crimes", title = "Daily Crimes in DC from 2008 - 2016") +
  theme(text=element_text(size = 16,family="Courier New" ))

#summarize crime by day of the week and hour
df_time <- dataset[,-which(names(dataset) %in% c("REPORT_DAT"))] %>%
  group_by(DayofWeek, hour) %>%
  summarize(count = n())

dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

df_time$DayofWeek <- factor(df_time$DayofWeek, level = rev(dow_format))
df_time$Hour <- factor(df_time$hour, level = 0:23, label = hour_format)

ggplot(df_time, aes(x = hour, y = DayofWeek, fill = count)) +
  geom_tile() +
  labs(x = "Hour of Crime", y = "Day of Week of Crime", title = "# of Police Arrests in DC from 2008 - 2016, by Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#45b293") +
  coord_fixed(ratio=1) + 
  theme(text=element_text(size = 12,family="Courier New" ))


#factor by crime category
df_time_crime <- dataset[,-which(names(dataset) %in% c("REPORT_DAT"))] %>%
  group_by(OFFENSE, DayofWeek, hour) %>%
  summarize(count = n())

df_time_crime$DayofWeek <- factor(df_time_crime$DayofWeek, level = rev(dow_format))
df_time_crime$Hour <- factor(df_time_crime$hour, level = 0:23, label = hour_format)

head(df_time_crime)

ggplot(df_time_crime, aes(x = hour, y = DayofWeek, fill = count)) +
  geom_tile() +
  labs(x = "Hour of Crime", y = "Day of Week of Crime", title = "# of Police Arrests in DC from 2008 - 2016, by Offense and Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#45b293") +
  facet_wrap(~ OFFENSE, nrow = 3)
