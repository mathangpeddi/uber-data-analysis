#Installing the packages
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("DT")
install.packages("scale")

#Importing the libraries
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#Creating vector of colors to be implemented in our plots
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
           
#Reading the data and storing it into different variable
apr_data=read.csv("uber-raw-data-apr14.csv")
may_data=read.csv("uber-raw-data-may14.csv")
jun_data=read.csv("uber-raw-data-jun14.csv")
jul_data=read.csv("uber-raw-data-jul14.csv")
aug_data=read.csv("uber-raw-data-aug14.csv")
sep_data=read.csv("uber-raw-data-sep14.csv")

#Splitting the Date.Time column into day,month,year,weekday,hour,minute,second columns
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data) #Merging all the datasets to get a big dataframe
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S") #This is to change the format of Date.time(year-month-date hours:mins:seconds)
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S") #Seperate column for time will be created
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time) #ymd_hms() functions automatically assigns the Universal Coordinated Time Zone (UTC) to the parsed date
data_2014$day <- factor(day(data_2014$Date.Time)) #It extracts and creates a seperate column 'day' indicating the day of that particular month(like 1,2,3,...31)
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE)) #It extracts and creates a seperate column 'month' indicating the month(like april,may,june etc.)
data_2014$year <- factor(year(data_2014$Date.Time)) #It extracts and creates a seperate column 'year' indicating the year which is 2014 for all the rows since this dataset contains all the trips that were taken in the year 2014
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE)) # #It extracts and creates a seperate column 'dayofweek' which gives the day of that particular date(like Monday,Tuesday...Sunday)

data_2014$hour <- factor(hour(hms(data_2014$Time)))  #It extracts and creates a seperate column 'hour' indicating the hour(time) of the ride taken(suppose if its 11:47AM then its 11 and suppose if 4:20PM then its 16)
data_2014$minute <- factor(minute(hms(data_2014$Time)))  #It extracts and creates a seperate column 'minute' indicating the minute(time) of the ride taken(suppose if its 11:47AM then its 47 and suppose if 4:20PM then its 20)
data_2014$second <- factor(second(hms(data_2014$Time)))  #It extracts and creates a seperate column 'second' indicating the second(time) of the ride taken

#Plotting the trips by the hours in a day
hour_data <- data_2014 %>% group_by(hour) %>% dplyr::summarize(Total = n()) 
datatable(hour_data) #this gives a table which shows the total no of trips every hour(like 1st hour is 103836(12AM-1AM) and then between 1st hour and 2nd hour(1AM-2AM) is 67227)
#we can understand how the number of passengers fares throughout the day. We observe that the number of trips are higher in the evening around 5:00 and 6:00 PM.(which is 143213)

#Trips for hour and month(this plot shows the total no of trips taken every month and every hour)
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
datatable(month_hour)

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

#Trips every hour(plot representing the datatable)
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Plotting data by trips during every day of the month
#This plot tells us the total no of trips on each and every day by every month(from 1st day till the 31st day by every month)
day_group <- data_2014 %>%group_by(day) %>% dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Plot for Trip by day and month
#This plot tells us the total no of trips on 1stday then 2nd day...31st day by every month.For eg now the total no of trips taken on the 30th day(combining all the months is the highest)
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
datatable(day_month_group)

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#Number of Trips taking place during months in a year
#Shows the total number of trips taken in every month
month_group<-data_2014 %>% group_by(month) %>% dplyr::summarise(Total=n())
datatable(month_group)

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#Trips by Day and Month
#So this gives the total no of trips on each day of the week in every month(like total no of trips on sunday in april,may,june...)
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
datatable(month_weekday)

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#Now we try to find out the number of Trips by bases
#This shows the total number of trips in every base(show the nuber of trips in the 5 bases)
base_group <- data_2014 %>%
  group_by(Base) %>%
  dplyr::summarize(Total = n())
datatable(base_group)

ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#Trips by Bases and Month
#This plot tells the total number of trips taken in every base in every month
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") 

#Trips by Bases and Dayofweek
#This plot tells the total number of trips taken in every base on every day of the week
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

#Now we create a Heatmap visualization of day, hour and month
#Heatmap shows the different correlations between each row and column(similarities and correlations between hour,month,day,dayofweek etc)
#First, we will plot Heatmap by Hour and Day.
#Second, we will plot Heatmap by Month and Day.
#Third, a Heatmap by Month and Day of the Week.
#Fourth, a Heatmap that delineates Month and Bases.
#Finally, we will plot the heatmap, by bases and day of the week.
 
#Day and Hour
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

#Month and Day
day_and_month <- data_2014 %>%
  group_by(day, month) %>%
  dplyr::summarize(Total = n())
datatable(day_and_month)

ggplot(day_and_month, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

#Month and Day of the week
dayofweek_and_month <- data_2014 %>%
  group_by(dayofweek, month) %>%
  dplyr::summarize(Total = n())
datatable(dayofweek_and_month)

ggplot(dayofweek_and_month, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

#Month and Bases
base_and_month <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n())
datatable(base_and_month)

ggplot(base_and_month, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

#Bases and Day of the week
base_and_dayofweek <- data_2014 %>%
  group_by(Base,dayofweek) %>%
  dplyr::summarize(Total = n())
datatable(base_and_dayofweek)

ggplot(base_and_dayofweek, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

#Creating a map visualization of rides in New York City
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

  ggplot(data_2014, aes(x=Lon, y=Lat)) +
    geom_point(size=1, color = "blue") +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat)) +
    theme_map() +
    ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")







              