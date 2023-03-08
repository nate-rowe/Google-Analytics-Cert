# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/Nate/Desktop/Google course/data") #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
Jan_2022 <- read_csv("202201-divvy-tripdata.csv")
Feb_2022 <- read_csv("202202-divvy-tripdata.csv")
Mar_2022 <- read_csv("202203-divvy-tripdata.csv")
Apr_2022 <- read_csv("202204-divvy-tripdata.csv")
May_2022 <- read_csv("202205-divvy-tripdata.csv")
Jun_2022 <- read_csv("202206-divvy-tripdata.csv")
Jul_2022 <- read_csv("202207-divvy-tripdata.csv")
Aug_2022 <- read_csv("202208-divvy-tripdata.csv")
Sep_2022 <- read_csv("202209-divvy-tripdata.csv")
Oct_2022 <- read_csv("202210-divvy-tripdata.csv")
Nov_2022 <- read_csv("202211-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(Jan_2022)
colnames(Feb_2022)
colnames(Mar_2022)
colnames(Apr_2022)
colnames(May_2022)
colnames(Jun_2022)
colnames(Jul_2022)
colnames(Aug_2022)
colnames(Sep_2022)
colnames(Oct_2022)
colnames(Nov_2022)


# Inspect the dataframes and look for incongruencies
str(Jan_2022)
str(Feb_2022)
str(Mar_2022)
str(Apr_2022)
str(May_2022)
str(Jun_2022)
str(Jul_2022)
str(Aug_2022)
str(Sep_2022)
str(Oct_2022)
str(Nov_2022)

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(Jan_2022, Feb_2022, Mar_2022, Apr_2022, May_2022, Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, Nov_2022)

# Remove lat and lng columns as they are not relevant to the analysis
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
all_trips <- all_trips[!(all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)

mean(all_trips$ride_length,na.rm=TRUE) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips$ride_length)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)						# sorts

# Let's visualize the number of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# update this so it only shows top 10% of ride length values so we can check for outliers
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  ggplot(aes(x = weekday, y = ride_length, colour = member_casual)) +
  geom_point()

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

#You're done! Congratulations!

