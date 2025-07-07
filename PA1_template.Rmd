---
editor_options: 
  markdown: 
    wrap: 72
---

title: "Reproducible Research: Peer Assessment 1" output: html_document:
keep_md: true ---

## Loading and preprocessing the data

\`\`\`{r} library(dplyr)

# Read the dataset

activity \<- read.csv("activity.csv")

# View the structure

str(activity)

## What is mean total number of steps taken per day?

# Calculate total steps per day (ignore NAs)

total_steps_day \<- activity %\>% filter(!is.na(steps)) %\>%
group_by(date) %\>% summarise(total = sum(steps))

# Histogram of total steps per day

hist(total_steps_day\$total, main = "Total Steps per Day", xlab =
"Steps")

# Mean and median

mean(total_steps_day$total)
median(total_steps_day$total)

## What is the average daily activity pattern?

# Average steps per 5-minute interval

interval_avg \<- activity %\>% filter(!is.na(steps)) %\>%
group_by(interval) %\>% summarise(avg_steps = mean(steps))

# Time series plot

plot(interval_avg$interval, interval_avg$avg_steps, type = "l", xlab =
"5-minute interval", ylab = "Average number of steps")

# Interval with max average steps

interval_avg[which.max(interval_avg\$avg_steps), ]

## Imputing missing values

# Total number of missing values

sum(is.na(activity\$steps))

# Impute missing values with mean of each interval

activity_imputed \<- activity
activity_imputed$steps <- ifelse(is.na(activity$steps),
ave(activity$steps, activity$interval, FUN = function(x) mean(x, na.rm =
TRUE)), activity\$steps)

# Total steps per day after imputation

total_steps_imputed \<- activity_imputed %\>% group_by(date) %\>%
summarise(total = sum(steps))

# Histogram after imputation

hist(total_steps_imputed\$total, main = "Imputed Total Steps per Day",
xlab = "Steps")

# Mean and median after imputation

mean(total_steps_imputed$total)
median(total_steps_imputed$total)

## Are there differences in activity patterns between weekdays and weekends?

# Add weekday/weekend column

activity_imputed$date <- as.Date(activity_imputed$date)
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in%
c("Saturday", "Sunday"), "weekend", "weekday")

# Average steps per interval by day type

interval_daytype \<- activity_imputed %\>% group_by(interval, day_type)
%\>% summarise(avg_steps = mean(steps))

# Panel plot

library(lattice) xyplot(avg_steps \~ interval \| day_type, data =
interval_daytype, type = "l", layout = c(1,2), xlab = "5-minute
interval", ylab = "Average number of steps")
