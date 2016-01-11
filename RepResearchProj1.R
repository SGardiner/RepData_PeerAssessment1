## Coursera - Reproducible Research, Project 1
##
## 1.) Code for reading in the dataset and/or processing the data
## 2.) Histogram of the total number of steps taken each day
## 3.) Mean and median number of steps taken each day
## 4.) Time series plot of the average number of steps taken
## 5.) The 5-minute interval that, on average, contains the maximum number of steps
## 6.) Code to describe and show a strategy for imputing missing data
## 7.) Histogram of the total number of steps taken each day after missing values are imputed
## 8.) Panel plot comparing the average number of steps taken per 5-minute interval 
## across weekdays and weekends
## 9.) All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

library(dplyr)
library(lattice)
library(knitr)
setwd("~/rprogramming/ReprodResearch/RepData_PeerAssessment1")

## 1.) Code for reading in the dataset and/or processing the data
## unzip and read in the the activity file
unzip("activity.zip")
activity <- read.csv("activity.csv")

## 2.) Histogram of the total number of steps taken each day
## Sum the total number of steps for each date
stepsPerDay <- aggregate(activity$steps, by=list(activity$date), "sum", na.rm = TRUE)
colnames(stepsPerDay) <- c("date", "steps")

## Plot a histogram of the steps
par(mar=c(5.1,4.1,4.1,2.1))
with(stepsPerDay, hist(steps, main = "Histogram of Steps Taken Each Day"))

## 3.) Mean and median number of steps taken each day
## Calculate mean and median steps per day and add to the current histogram plot
meanSteps <- mean(stepsPerDay$steps, na.rm = TRUE)
abline(v = meanSteps, col = "green", lwd = 2)

medianSteps <- median(stepsPerDay$steps, na.rm = TRUE)
abline(v = medianSteps, col = "blue", lwd = 2, lty = 2)

legend(2000,25, c(paste("Mean=",round(meanSteps,1)), 
                  paste("Median=",round(medianSteps,1))), 
                  fill=c("green", "blue"), bty = "n", cex=0.8)

## 4.) Time series plot of the average number of steps taken
## Calculate mean of steps taken for each interval, averaged across all dates
stepsPerInterval <- aggregate(activity$steps, by=list(activity$interval), 
                              "mean", na.rm = TRUE)
colnames(stepsPerInterval) <- c("interval", "steps")

## Plot the data
with(stepsPerInterval, plot(interval,steps, type = "l", 
                main = "Average daily step activity pattern",
                xlab = "5 minute time intervals",
                ylab = "Avg. Steps"))

## Calculate the index value for average max # of steps
maxSteps <- which.max(stepsPerInterval$steps)
points(stepsPerInterval$interval[maxSteps],stepsPerInterval$steps[maxSteps], 
       pch=19, col = "blue")
legend("topright", paste("Max avg. # steps = ", 
                         round(stepsPerInterval$steps[maxSteps],1),
                         "in interval 835"),
       pch = 19,col ="blue", 
       bty = "n", cex = 0.8)

## 6.) Code to describe and show a strategy for imputing missing data
## First calculate the number of missing values for steps
## 2304 missing values out of 17568 observations
missingData = sum(is.na(activity$steps))  
## Create a second data set with imputed values
activityImp <- activity

## For every NA value, replace with the average steps per 5-minute interval for that day

## Calculate the mean steps for every day, removing all NA values
## If the mean for a specific date is NA, then number of steps = 0
meanPerDay <- aggregate(activityImp$steps, by=list(activityImp$date), "mean", na.rm = TRUE)
colnames(meanPerDay) <- c("date", "steps")
## Replace NA values with 0
for(i in 1:nrow(meanPerDay)) {
        if(is.na(meanPerDay$steps[i])) {
                meanPerDay$steps[i] <- 0
        }
}

## If a step value = NA, replace it with the mean value for the day
for(i in 1:nrow(activityImp)) {
        if(is.na(activityImp$steps[i])) {
                activityImp$steps[i] <- meanPerDay$steps[meanPerDay$date == activityImp$date[i]]
        }
}

## 7.) Histogram of the total number of steps taken each day after missing values are imputed
## As in steps 2 and 3, plot a histogram of the total number of steps taken each day
## from the imputed data and plot the mean and median
stepsPerDayImp <- aggregate(activityImp$steps, by=list(activity$date), "sum", na.rm = TRUE)
colnames(stepsPerDayImp) <- c("date", "steps")
with(stepsPerDayImp, hist(steps, main = "Steps Taken Each Day - Imputed"))

abline(v = mean(stepsPerDayImp$steps, na.rm = TRUE), col = "green", lwd = 2)
abline(v = median(stepsPerDayImp$steps, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)

legend(2000,25, c(paste("Mean=",round(mean(stepsPerDayImp$steps, na.rm = TRUE),1)), 
                  paste("Median=",round(median(stepsPerDayImp$steps, na.rm = TRUE),1))), 
       fill=c("green", "blue"), bty = "n", cex=0.8)

## When missing values are imputed in this manner, the histogram plot and the mean and
## median values are identical to the histogram plots for questions 2.) and 3.)

## 8.) Panel plot comparing the average number of steps taken per 5-minute interval 
## across weekdays and weekends

## Add variable to compute day of the week
activityImp$day <- weekdays(as.POSIXlt(activityImp$date))
## Add variable indicating "weekday" or "weekend"
for(i in 1:nrow(activityImp)) {
        if(activityImp$day[i] == "Saturday" | activityImp$day[i] == "Sunday") {
                activityImp$wkday[i] <- "weekend"
        }
        else activityImp$wkday[i] <- "weekday"
}

## Calculate steps averaged per interval, separated across weekend and weekdays
stepsPerInt <- with(activityImp, aggregate(steps, by=list(interval,wkday), 
                                           "mean", na.rm = TRUE))
colnames(stepsPerInt) <- c("interval", "wkday", "steps")

with(stepsPerInt, xyplot(steps~interval|wkday, type = "l", layout=c(1,2),
                         main="Average steps taken for each time interval",
                         xlab="5 minute time intervals"))











