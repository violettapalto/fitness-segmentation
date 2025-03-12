library(dplyr)
daily_activity <- read.csv("dailyActivity_merged.csv")
heart_rate <- read.csv("heartrate_seconds_merged.csv")
sleep <- read.csv("minuteSleep_merged.csv")

str(daily_activity)
summary(daily_activity)

str(heart_rate)
summary(heart_rate)

str(sleep)
summary(sleep)

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format="%m/%d/%y")

daily_activity <- daily_activity %>% distinct()
heart_rate <- heart_rate %>% distinct()
sleep <- sleep %>% distinct()

heart_rate$Time <- as.POSIXct(heart_rate$Time, format= "%m/%d/%Y %H:%M:%S")
heart_rate <- heart_rate %>% mutate(Date = as.Date(Time))

heart_rate_daily <- heart_rate %>% 
  group_by(Id, Date) %>% 
  summarize(AvgHeartRate = mean(Value, na.rm = TRUE))

head(heart_rate_daily)

sleep <- sleep %>% mutate(Date = as.Date(date))

sleep_daily <- sleep %>%
  group_by(Id, Date) %>%
  summarize(TotalSleepMinutes = sum(value, na.rm = TRUE))

head(sleep_daily)

heart_rate_daily$AvgHeartRate[is.na(heart_rate_daily$AvgHeartRate)] <- median(heart_rate_daily$AvgHeartRate, na.rm = TRUE)

sleep_daily$TotalSleepMinutes[is.na(sleep_daily$TotalSleepMinutes)] <- median(sleep_daily$TotalSleepMinutes, na.rm = TRUE)


merged_data <- full_join(daily_activity, heart_rate_daily, by = c("Id" = "Id", "ActivityDate" = "Date"))
merged_data <- full_join(merged_data, sleep_daily, by = c("Id" = "Id", "ActivityDate" = "Date"))

head(merged_data)


colSums(is.na(merged_data))
# Apply median replacement only for numeric columns
for (col in colnames(merged_data)) {
  if (is.numeric(merged_data[[col]])) {
    merged_data[[col]][is.na(merged_data[[col]])] <- median(merged_data[[col]], na.rm = TRUE)
  }
}

head(merged_data)