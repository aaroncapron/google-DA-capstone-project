library(tidyverse)  # calculations
library(lubridate)  # dates 
library(hms)        # time
library(data.table) # exporting dataframe

# load .csv files, 8/2020 - 7/2021
aug08_df <- read_csv("202008-divvy-tripdata.csv") 
sep09_df <- read_csv("202009-divvy-tripdata.csv") 
oct10_df <- read_csv("202010-divvy-tripdata.csv")
nov11_df <- read_csv("202011-divvy-tripdata.csv") 
dec12_df <- read_csv("202012-divvy-tripdata.csv")
jan01_df <- read_csv("202101-divvy-tripdata.csv") 
feb02_df <- read_csv("202102-divvy-tripdata.csv") 
mar03_df <- read_csv("202103-divvy-tripdata.csv")
apr04_df <- read_csv("202104-divvy-tripdata.csv")
may05_df <- read_csv("202105-divvy-tripdata.csv") 
jun06_df <- read_csv("202106-divvy-tripdata.csv") 
jul07_df <- read_csv("202107-divvy-tripdata.csv") 

# merge dataframes into 1-yr view
cyclistic_df <- rbind (aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

# remove individual month dataframes to clear space
remove(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

# new dataframe for new columns
cyclistic_date <- cyclistic_df

# calculate ride length: 'started_at time' - 'ended_at time', then convert to minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

# day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at)                 # yyyy-mm-dd, use starting date
cyclistic_date$day_of_week <- wday(cyclistic_df$started_at)               # calculate the day of the week 
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A")  # day of week
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")        # month
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d")          # day
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y")         # year
cyclistic_date$time <- format(as.Date(cyclistic_date$date), "%H:%M:%S")   # format time as HH:MM:SS
cyclistic_date$time <- as_hms((cyclistic_df$started_at))                  # time
cyclistic_date$hour <- hour(cyclistic_date$time)                          # hour

# converting 'month' to Spring, Summer, Fall, and Winter
cyclistic_date <-cyclistic_date %>% mutate(
    season = case_when(month == "01" ~ "Winter",
                       month == "02" ~ "Winter",
                       month == "03" ~ "Spring",
                       month == "04" ~ "Spring",
                       month == "05" ~ "Spring",
                       month == "06"  ~ "Summer",
                       month == "07"  ~ "Summer",
                       month == "08"  ~ "Summer",
                       month == "09" ~ "Fall",
                       month == "10" ~ "Fall",
                       month == "11" ~ "Fall",
                       month == "12" ~ "Winter"
    )
)

# converting 'time_of_day' to Night, Morning, Afternoon, Evening
cyclistic_date <-cyclistic_date %>% mutate(
    time_of_day = case_when(hour == "0" ~ "Night",
                            hour == "1" ~ "Night",
                            hour == "2" ~ "Night",
                            hour == "3" ~ "Night",
                            hour == "4" ~ "Night",
                            hour == "5" ~ "Night",
                            hour == "6" ~ "Morning",
                            hour == "7" ~ "Morning",
                            hour == "8" ~ "Morning",
                            hour == "9" ~ "Morning",
                            hour == "10" ~ "Morning",
                            hour == "11" ~ "Morning",
                            hour == "12" ~ "Afternoon",
                            hour == "13" ~ "Afternoon",
                            hour == "14" ~ "Afternoon",
                            hour == "15" ~ "Afternoon",
                            hour == "16" ~ "Afternoon",
                            hour == "17" ~ "Afternoon",
                            hour == "18" ~ "Evening",
                            hour == "19" ~ "Evening",
                            hour == "20" ~ "Evening",
                            hour == "21" ~ "Evening",
                            hour == "22" ~ "Evening",
                            hour == "23" ~ "Evening"
    )
)

# data cleaning
cyclistic_date <- na.omit(cyclistic_date)                            # remove NA values
cyclistic_date <- distinct(cyclistic_date)                           # remove duplicate rows 
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] # remove where 'ride_length' is <= 0
cyclistic_date <- cyclistic_date %>%                                 # remove unneeded columns
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

# final data
View(cyclistic_date)

#-----------------total rides--------------
# total number of rides
nrow(cyclistic_date)

#-----------------member type--------------
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#-----------------bike type----------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# total rides 
cyclistic_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#--------------------hour------------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48)

# total rides
cyclistic_date %>%
  count(hour) %>% 
  print(n = 24)

#----------------TIME OF DAY---------------

# -----------------morning-----------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# total rides
cyclistic_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# ---------------afternoon-----------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

# total rides 
cyclistic_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#----------------evening------------------
# total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# total rides
cyclistic_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----------------night-------------------
# number of rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

# number of rides 
cyclistic_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#-----------all times of day--------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

# number of rides
cyclistic_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#------------day of the week--------------
# total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

# total rides 
cyclistic_date %>%
  count(day_of_week)

#-----------day of the month--------------
# total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) # view entire tibble

# total rides
cyclistic_date %>%
  count(day) %>% 
  print(n = 31)

#-----------------month-------------------
# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24)

# total rides
cyclistic_date %>%
  count(month) 

#----------------SEASON-------------------

#----------------spring-------------------

# total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

# total rides
cyclistic_date %>%
  filter(season == "Spring") %>% 
  count(season)

#----------------summer-------------------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

# total rides
cyclistic_date %>%
  filter(season == "Summer") %>% 
  count(season)

#-----------------fall--------------------
# total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

# total rides
cyclistic_date %>%
  filter(season == "Fall") %>% 
  count(season)

#----------------winter-------------------
# total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

# total rides 
cyclistic_date %>%
  filter(season == "Winter") %>% 
  count(season)

#--------------all seasons----------------
# total rides by member type
cyclistic_date %>%
  group_by(season, member_casual) %>% 
  count(season)

# total rides
cyclistic_date %>%
  group_by(season) %>% 
  count(season)

#-----------------------average ride length--------------------
# avg of 'ride_length'
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

#--------------------------member type-------------------------
# avg 'ride_length'
cyclistic_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#------------------------type of bike--------------------------
# total 'rides' by member type 
cyclistic_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----------------------------hour------------------------------
# avg 'ride_length' by member type
cyclistic_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=48)

# avg 'ride_length'
cyclistic_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)

#------------------------TIME OF DAY-------------------------

#--------------------------morning---------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-------------------------afternoon--------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#--------------------------evening---------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----------------------------night---------------------------
# avg 'ride_length' by member type 
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------all times of day---------------------
#average 'ride_length' by member type
cyclistic_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----------------------day of the week----------------------
# avg 'ride_length' by member type
cyclistic_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----------------------day of the month----------------------
# avg ride_length by member type
cyclistic_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=62)

# avg ride_length
cyclistic_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=31)

#---------------------------month----------------------------
# avg 'ride_length' by member type
cyclistic_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)

# avg 'ride_length'
cyclistic_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-------------------------SEASON-------------------------

#-------------------------winter-------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-------------------------spring-------------------------
#average ride length by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-------------------------summer-------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#--------------------------fall--------------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----------------------all seasons----------------------
# avg 'ride_length' by member type
cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# avg 'ride_length'
cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length), list(time = mean))