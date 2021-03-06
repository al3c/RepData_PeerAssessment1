## Peer Assessment 1

## set working directory if necessary
## setwd("~Documents/Coding/Coursera/ReproResearch/data/project1")

library(ggplot2)
library(lubridate)

## read the file in and look at structure
activity <- read.csv("activity.csv")
str(activity)
## 3 variables: steps (integer), date (factor), interval (integer)
summary(activity)
## format the colums appropriatley - dates as dates; interval 
## as a factor
activity$date = as.Date(activity$date)
unique(activity$date) ## sample size is 61 days
activity$interval = factor(activity$interval)

## plot a histogram of the total steps taken each day
tot_steps <- tapply(activity$steps, activity$date, sum)
qplot(tot_steps, main = "Total steps taken per day",
        xlab = "Total steps", ylab = "Count", margins = T) +
  geom_histogram(fill = "steelblue")

## ggplot(tot_steps, aes(x = steps)) +
##   geom_histogram(fill = "steelblue", binwidth = 1500)
## calculate the mean and median total number of steps taken per day
act_mean <- mean(tapply(activity$steps, activity$date, sum), 
                 na.rm = TRUE)
act_median <- median(tapply(activity$steps, activity$date, sum), 
                     na.rm = TRUE)
## mean is 10766.19, median is 10765

## average daily activity pattern; time series plot
length(unique(activity$interval))
## 288 intervals
intervals <- unique(activity$interval)
avgsteps <- tapply(activity$steps, activity$interval, mean, na.rm = T)
interval_data <- data.frame(intervals, avgsteps)
with(interval_data, plot(intervals, avgsteps, 
                         main = "Average daily steps per interval", 
                         ylab = "average steps", type = "l")) 
## max steps in which intervall
max_index <- as.numeric(which.max(interval_data$avgsteps))
interval_data[max_index,]
## the 835 interval has the most with 206 steps

# input missing data
## total # of NAs
table(is.na(activity$steps))
## 2304 NAs

## for loop: if NA, then check intervall, 
## insert the avg for that interval
## for missing values, replacing them with the mean at that same 
## interval, as I am assuming that activities follow a daily pattern.

##  !! activity$interval <- factor(activity$interval)
steps_per <- aggregate(activity$steps, 
                       by = list(interval = activity$interval), 
                       mean, na.rm = T)
# convert to integers for plotting
steps_per$interval <- as.integer(levels(steps_per$interval)
                                [steps_per$interval])
colnames(steps_per) <- c("interval", "steps")

means_replace <- function(activity, defaults) {
    na_indices <- which(is.na(activity$steps))
    defaults <- steps_per
    na_repl <- unlist(lapply(na_indices, 
                                     FUN = function(ind) {
        interval = activity[ind, ]$interval
        defaults[defaults$interval == interval, ]$steps
    }))
    new_steps <- activity$steps
    new_steps[na_indices] <- na_repl
    new_steps
}
activity_imp <- data.frame(steps = means_replace(activity, avgsteps), 
                           date = activity$date, 
                           interval = activity$interval)

summary(activity_imp)
# differences

intervals <- unique(activity_imp$interval)
avgsteps <- tapply(activity_imp$steps, activity_imp$interval, 
                   mean, na.rm = T)
interval_data_imp <- data.frame(intervals, avgsteps)
with(interval_data_imp, plot(intervals, avgsteps, 
                         main = "Average daily steps per interval", 
                         ylab = "average steps", type = "l")) 
## new mean and median
imp_mean <- mean(tapply(activity_imp$steps, activity_imp$date, sum), 
                 na.rm = TRUE)
imp_median <- median(tapply(activity_imp$steps, activity_imp$date, sum), 
                     na.rm = TRUE)

## plotting weekday vs weekend
