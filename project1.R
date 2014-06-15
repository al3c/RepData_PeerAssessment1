## Peer Assessment 1

## set working directory if necessary
## setwd("~Documents/Coding/Coursera/ReproResearch/data/project1")

library(ggplot2)
library(lubridate)

## read the file in and look at structure
activity <- read.csv("activity.csv")
str(activity)
## 3 variables: steps (integer), date (factor), interval (integer)
activity$date = as.Date(activity$date)
unique(activity$date)
## 61 days

## plot a histogram of the total steps taken each day
tot_steps <- tapply(activity$steps, activity$date, sum)
qplot(tot_steps, main = "Total steps taken per day",
       xlab = "Total steps", ylab = "Count", margins = T)

## calculate the mean and median total number of steps taken per day
act_mean <- mean(tapply(activity$steps, activity$date, mean), 
                 na.rm = TRUE)
act_median <- median(tapply(activity$steps, activity$date, median), 
                     na.rm = TRUE)
## mean is 37.3826, median is 0

## average daily activity pattern

## input missing data

## differences
