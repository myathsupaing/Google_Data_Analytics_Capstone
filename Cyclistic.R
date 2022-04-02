# install packages and loading library
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("magrittr")
library(magrittr)

# importing files
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2020)
colnames(q4_2019)
colnames(q3_2019)
colnames(q2_2019)

# renaming objects for data consistency
(q3_2019 <-rename(q3_2019
                  ,ride_id = "trip_id"
                  ,started_at = "start_time"
                  ,ended_at = "end_time"
                  ,rideable_type = "bikeid"
                  ,start_station_id = "from_station_id"
                  ,start_station_name = "from_station_name"
                  ,end_station_id = "to_station_id"
                  ,end_station_name = "to_station_name"  
                  ,member_casual = "usertype"))

(q4_2019 <-rename(q4_2019
                  ,ride_id = "trip_id"
                  ,started_at = "start_time"
                  ,ended_at = "end_time"         
                  ,rideable_type = "bikeid"
                  ,start_station_id = "from_station_id"  
                  ,start_station_name = "from_station_name"
                  ,end_station_id = "to_station_id"
                  ,end_station_name = "to_station_name"  
                  ,member_casual = "usertype"))

(q2_2019 <-rename(q2_2019
                  ,ride_id = "X01...Rental.Details.Rental.ID"                    
                  ,started_at = "X01...Rental.Details.Local.Start.Time"            
                  ,ended_at = "X01...Rental.Details.Local.End.Time"               
                  ,rideable_type = "X01...Rental.Details.Bike.ID"                     
                  ,start_station_id = "X03...Rental.Start.Station.ID"                    
                  ,start_station_name = "X03...Rental.Start.Station.Name"                   
                  ,end_station_id = "X02...Rental.End.Station.ID"                      
                  ,end_station_name = "X02...Rental.End.Station.Name"                     
                  ,member_casual = "User.Type"))

# change data types from integers to characters
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)

q4_2019 <-mutate(q4_2019, ride_id = as.character(ride_id),
                 rideable_type = as.character(rideable_type))

q3_2019 <-mutate(q3_2019, ride_id = as.character(ride_id),
                 rideable_type = as.character(rideable_type))

q2_2019 <-mutate(q2_2019, ride_id = as.character(ride_id),
                 rideable_type = as.character(rideable_type))


# binding data
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# removing unwanted columns
all_trips_v2 <- all_trips %>%
  select (-c(birthyear, gender, X01...Rental.Details.Duration.In.Seconds.Uncapped, X05...Member.Details.Member.Birthday.Year, Member.Gender, tripduration, start_lat,start_lng,end_lat, end_lng))

# summary
glimpse(all_trips_v2)

# changing attribute names of member_casual column for data consistency
table(all_trips_v2$member_casual)

all_trips_v2 %>%
  distinct(member_casual)

all_trips_v2 <- all_trips_v2 %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

all_trips_v2 %>%
  distinct(member_casual)

# separating started_at column
all_trips_v2 <- all_trips_v2 %>%
  separate(
    started_at,
    into = c("date", "time_of_day"),
    sep = " ",
    remove = FALSE
  )

# changing the format of month, day_of_week columns
all_trips_v2$month <- format(as.Date(all_trips_v2$date), "%m")
all_trips_v2$day <- format(as.Date(all_trips_v2$date), "%d")
all_trips_v2$year <- format(as.Date(all_trips_v2$date), "%Y")
all_trips_v2 <- all_trips_v2 %>%
  mutate(
    month = factor(month.name[as.numeric(month)], levels = month.name, ordered = TRUE),
    weekday = wday(
      as.Date(all_trips_v2$started_at), 
      label = TRUE, 
      abbr = FALSE)
  )

head(all_trips_v2)

# create ride_length column
all_trips_v2 <- all_trips_v2 %>%
  mutate(ride_length = difftime(ended_at, started_at, units = "mins"))

# inspecting the structure of the columns
str(all_trips_v2)
is.factor(all_trips_v2$ride_length)
all_trips_v2$ride_length <- as.numeric(as.character(all_trips_v2$ride_length))
is.numeric(all_trips_v2$ride_length)

##removing null values since some data entries of trip_duration column shows up as negative
all(all_trips_v2$ride_length <=0)
any(all_trips_v2$ride_length <=0)

#all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

all_trips_v3 <- all_trips_v2[!(all_trips_v2$ride_length < 0),]
head(all_trips_v3$ride_length, n=10)

# extract only hours from time for data visualizing purpose
all_trips_v3$hour <- substr(all_trips_v3$time_of_day, 1, 2)

# relocating columns
all_trips_v3 <- all_trips_v3 %>% relocate(date, day, month, year, weekday, time_of_day, .before = started_at)
all_trips_v3 <- all_trips_v3 %>% relocate(member_casual, .after = ride_id)
all_trips_v3 <- all_trips_v3 %>% relocate(ride_length, .after = ended_at)
head(all_trips_v3)

##statistical summary
install.packages("skimr")
library(skimr)
skim(all_trips_v3)

# the summary of all_trips by user_type
all_trips_v3 %>%
  group_by(member_casual) %>%
  summarize(
    mean_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    min_ride_length = min(ride_length),
    max_ride_length= max(ride_length),
    total_trips = n(),
  ) %>%
  arrange(-total_trips)

# the summary of trip duration by each day for subscriber vs customer users
all_trips_v3 %>%
  group_by(weekday, member_casual) %>%
  summarize(
    mean_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    total_trips = n(),
  ) %>%
  arrange(desc(total_trips))

all_trips_v3 %>%
  group_by(start_station_name, member_casual) %>%
  summarize(
    MaxStartStationName = max(start_station_name),
    total_trips = n(),
  ) %>%
  arrange(desc(total_trips))

all_trips_v3 %>%
  group_by(member_casual, end_station_name) %>%
  summarize(
    MaxEndStationName = max(end_station_name),
    total_trips = n(),
  ) %>%
  arrange(desc(total_trips))

##DATA VISUTALIZATION
# (1) ~Trip Count by Day and Rider Type 
all_trips_v3 %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), .groups = "drop")%>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  scale_y_continuous(label=scales::comma) +
  geom_col(position = "dodge") +
  theme (axis.text.x = element_text(angle =45)) +
  labs(title = "1. Trip Count by Day and Rider Type",
       x = "Weekday", y = "Trip Count", fill = "Rider Type")

# ~Average Trip Duration by Day and Rider Type (already in mins)
all_trips_v3 %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration_min = mean(ride_length), .groups = "drop") %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme (axis.text.x = element_text(angle =45)) +
  labs(title = "2. Average Trip Duration by Day and Rider Type",
       x = "Weekday", y = "Average Trip Duration (min)", fill = "Rider Type")

## Average Trip Duration by Month and Rider Type (line graph)
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(average_duration_min = mean(ride_length), .groups = "drop") %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration_min, color = member_casual)) +
  geom_line(aes(group = member_casual)) +
  theme (axis.text.x = element_text(angle =45)) +
  labs(title = "3. Average Trip Duration by Month and Rider Type", x = "Month", 
       y = "Average Trip Duration (min)", color = "Rider Type")


# Trip Count by Month and Rider Type (bar graph)
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  scale_y_continuous(label=scales::comma) +
  geom_col(position = "dodge") +
  labs(title = "4. Trip Count by Month and Rider Type",
       x = "Month", y = "Trip Count", fill = "Rider Type")+
  coord_flip()

# Trip Count by Time of Day and Rider Type (line graph)
all_trips_v2 %>% 
  group_by(member_casual, hour) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(member_casual, hour) %>% 
  ggplot(aes(x = hour, y = number_of_rides, color = member_casual)) +
  scale_y_continuous(label=scales::comma) +
  geom_line(aes(group = member_casual)) +
  theme (axis.text.x = element_text(angle =90)) +
  labs(title = "5. Trip Count by Time of Day and Rider Type", x = "Hour", 
       y = "Trip Count", color = "Rider Type")

##Export summary file for further analysis

counts <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$weekday, FUN = mean)
write.csv(counts, file = '~/Desktop/Cyclistic/avg_ride_length.csv')



