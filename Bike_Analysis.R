# =============================================================================
# STEP 1: LOAD IN NECESSARY LIBRARIES AND DATASETS
# =============================================================================

# Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

# Load packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# Load in all of the individual datasets
jan_2022_data <- read.csv(file = "202201-divvy-tripdata.csv")
feb_2022_data <- read.csv(file = "202202-divvy-tripdata.csv")
mar_2022_data <- read.csv(file = "202203-divvy-tripdata.csv")
apr_2022_data <- read.csv(file = "202204-divvy-tripdata.csv")
may_2022_data <- read.csv(file = "202205-divvy-tripdata.csv")
jun_2022_data <- read.csv(file = "202206-divvy-tripdata.csv")
jul_2022_data <- read.csv(file = "202207-divvy-tripdata.csv")
aug_2022_data <- read.csv(file = "202208-divvy-tripdata.csv")
sep_2022_data <- read.csv(file = "202209-divvy-publictripdata.csv")
oct_2022_data <- read.csv(file = "202210-divvy-tripdata.csv")
nov_2022_data <- read.csv(file = "202211-divvy-tripdata.csv")
dec_2022_data <- read.csv(file = "202212-divvy-tripdata.csv")

# =============================================================================
# STEP 2: MERGE ALL DATASETS INTO ONE SINGLE FILE
# =============================================================================

# Check the column names for inconsistencies
colnames(jan_2022_data)
colnames(feb_2022_data)
colnames(mar_2022_data)
colnames(apr_2022_data)
colnames(may_2022_data)
colnames(jun_2022_data)
colnames(jul_2022_data)
colnames(aug_2022_data)
colnames(sep_2022_data)
colnames(oct_2022_data)
colnames(nov_2022_data)
colnames(dec_2022_data)

# Look at dataframes for inconsistencies
str(jan_2022_data)
str(feb_2022_data)
str(mar_2022_data)
str(apr_2022_data)
str(may_2022_data)
str(jun_2022_data)
str(jul_2022_data)
str(aug_2022_data)
str(sep_2022_data)
str(oct_2022_data)
str(nov_2022_data)
str(dec_2022_data)

# Combine all the dataframes into one large dataframe
all_2022_rides <- bind_rows(jan_2022_data, feb_2022_data, mar_2022_data, 
                            apr_2022_data, may_2022_data, jun_2022_data,
                            jul_2022_data, aug_2022_data, sep_2022_data,
                            oct_2022_data, nov_2022_data, dec_2022_data)

# =============================================================================
# Step 3: DATA CLEANING/PREPARATION
# =============================================================================

# Inspect the new dataframe
colnames(all_2022_rides)
nrow(all_2022_rides)
dim(all_2022_rides)
head(all_2022_rides)
str(all_2022_rides)
summary(all_2022_rides)

# Change column names
all_2022_rides <- rename(all_2022_rides,
                    bike_type = rideable_type,
                    start_time = started_at,
                    end_time = ended_at,
                    rider_type = member_casual)

# Recode item names in bike_type columns
table(all_2022_rides$bike_type)
all_2022_rides <- all_2022_rides %>%
  mutate(bike_type = recode(bike_type,
                            "classic_bike" = "classic",
                            "docked_bike" = "docked",
                            "electric_bike" = "electric"))

# Add columns listing the date, month, day, and year of each ride
all_2022_rides$date <- as.Date(all_2022_rides$start_time)
all_2022_rides$month <- format(as.Date(all_2022_rides$date), "%m")
all_2022_rides$day <- format(as.Date(all_2022_rides$date), "%d")
all_2022_rides$year <- format(as.Date(all_2022_rides$date), "%Y")

# Add a column listing the length of each ride, measured in seconds
all_2022_rides$ride_length <- difftime(all_2022_rides$end_time, 
                                       all_2022_rides$start_time)
all_2022_rides$ride_length <- as.numeric(as.character(
  all_2022_rides$ride_length))

# Add a column noting the day of the week the ride started on (note 1 = Monday)
all_2022_rides$day_of_week <- wday(all_2022_rides$start_time, week_start = 1)

# Count all the N/A values and blank values in each column
colSums(is.na(all_2022_rides))
colSums(all_2022_rides == "")
        
# Remove all data where ride_length is negative or start_station_name = "HQ QR"
# Also remove all rows that contain blank or N/A values
all_rides_2022_v2 <- all_2022_rides[
  !(all_2022_rides$ride_length < 0 
    | all_2022_rides$start_station_name == "HQ QR"
    | all_2022_rides$start_station_name == ""
    | all_2022_rides$start_station_id == ""
    | all_2022_rides$end_station_name == ""
    | all_2022_rides$end_station_id == ""
    | is.na(all_2022_rides$end_lat)
    | is.na(all_2022_rides$end_lng)),]

# =============================================================================
# STEP 4: DESCRIPTIVE ANALYSIS ON RIDE LENGTH
# =============================================================================

# Summary of ride_length
summary(all_rides_2022_v2$ride_length)

# Compare the ride_length between members and casual riders
aggregate(all_rides_2022_v2$ride_length ~ all_rides_2022_v2$rider_type,
          FUN = mean)
aggregate(all_rides_2022_v2$ride_length ~ all_rides_2022_v2$rider_type,
          FUN = median)
aggregate(all_rides_2022_v2$ride_length ~ all_rides_2022_v2$rider_type,
          FUN = max)
aggregate(all_rides_2022_v2$ride_length ~ all_rides_2022_v2$rider_type,
          FUN = min)

# Observe the average ride time each day for members vs casual riders
aggregate(all_rides_2022_v2$ride_length ~ all_rides_2022_v2$rider_type
          + all_rides_2022_v2$day_of_week, FUN = mean)

# Observe average ride length per weekday per group
all_rides_2022_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(rider_type, weekday) %>%
  summarize(number_of_riders = n(), average_duration = mean(ride_length)) %>%
  arrange(rider_type, weekday)

# Visualize the average ride length per weekday per group
all_rides_2022_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(rider_type, weekday) %>%
  summarize(number_of_riders = n(), average_duration = mean(ride_length)) %>%
  arrange(rider_type, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = rider_type)) +
  geom_col(position = "dodge") +
  labs( title = "Average Number of Riders",
        y = " Average Duration of Ride (seconds)", 
        x = "Day of Week", 
        fill = "Rider Type")

# =============================================================================
# STEP 5: EXPORT CODE TO .CSV FILES FOR VISUALIZATION IN TABLEAU
# =============================================================================

# Export summary info to a .csv file for visualization in Tableau
mean_info <- aggregate(all_rides_2022_v2$ride_length 
                       ~ all_rides_2022_v2$rider_type,
                       FUN = mean)
median_info <- aggregate(all_rides_2022_v2$ride_length 
                         ~ all_rides_2022_v2$rider_type,
                         FUN = median)
max_info <- aggregate(all_rides_2022_v2$ride_length 
                      ~ all_rides_2022_v2$rider_type,
                      FUN = max)
min_info <- aggregate(all_rides_2022_v2$ride_length 
                      ~ all_rides_2022_v2$rider_type,
                      FUN = min)
data_for_summary <- data.frame(mean_info, median_info, max_info, min_info)
write.csv(data_for_summary, file = 'rider_exported_data.csv')

# Export average ride time each day for members vs casual riders to a .csv file
# for visualization in Tableau
data_for_viz <- aggregate(all_rides_2022_v2$ride_length ~ 
                             all_rides_2022_v2$rider_type
                           + all_rides_2022_v2$day_of_week, FUN = mean)
write.csv(data_for_viz, file = 'rider_exported_data_2.csv')