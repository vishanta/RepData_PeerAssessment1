---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---

### Loading and preprocessing the data


```r
activity <- read.csv("./activity.csv", header = TRUE, sep = ",")
```


### What is mean total number of steps taken per day?

tapply function is used to calculate the total number of steps taken each day.

##### 1. Make a histogram of the total number of steps taken each day


```r
steps <- tapply(activity$steps, activity$date, sum)
hist(steps, main = "Historgram of Total Number of Steps Taken Each Day", xlab = "Total Number of Steps Taken Each Day", col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

##### 2. Calculate and report the mean and median total number of steps taken per day

*Mean*:


```r
mean(steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

*Median*:


```r
median(steps, na.rm = TRUE)
```

```
## [1] 10765
```


### What is the average daily activity pattern?

Making use of tapply function to evaluate the average daily activity pattern.

##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
pattern <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(pattern, type = "l", main = "Time Series Plot", xlab = "Time Interval", ylab = "Average Steps", col = "blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# max(pattern)
pattern[which.max(pattern)]
```

```
##      835 
## 206.1698
```


### Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# length(activity[is.na(activity)])
sum(is.na(activity$steps))
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy adopted: The mean for the 5-minute interval is used to fill up the 'NA' values for the steps.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityFilled <- activity

for (i in 1:nrow(activity)) {
  if (is.na(activity$steps[i])) {
    activityFilled$steps[i] <- pattern[[as.character(activity[i, "interval"])]]
  }
}
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsNew <- tapply(activityFilled$steps, activityFilled$date, sum)
hist(stepsNew, main = "Historgram of Total Number of Steps Taken Each Day", xlab = "Total Number of Steps Taken Each Day", col = "red")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

*Mean*:


```r
mean(stepsNew)
```

```
## [1] 10766.19
```

*Median*:


```r
median(stepsNew)
```

```
## [1] 10766.19
```

The new mean value is unchanged as we imputed means for NAs, however median is shifted closer to mean.


### Are there differences in activity patterns between weekdays and weekends?

Using the dataset with the filled-in missing values for this part.

##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityFilled$dayOfWeek <- c("weekday")
activityFilled$dayOfWeek[weekdays(as.Date(activityFilled$date)) %in% c("Saturday", "Sunday")] <- c("weekend")

activityFilled$dayOfWeek <- factor(activityFilled$dayOfWeek)
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activityFilledWeekday <- subset(activityFilled, activityFilled$dayOfWeek == "weekday")
activityFilledWeekend <- subset(activityFilled, activityFilled$dayOfWeek == "weekend")

activityWD <- tapply(activityFilledWeekday$steps, activityFilledWeekday$interval, mean)
activityWE <- tapply(activityFilledWeekend$steps, activityFilledWeekend$interval, mean)

dfWD <- data.frame(interval = unique(activityFilledWeekday$interval), avg = as.numeric(activityWD), day = rep("weekday", length(activityWD)))
dfWE <- data.frame(interval = unique(activityFilledWeekend$interval), avg = as.numeric(activityWE), day = rep("weekend", length(activityWE)))
dfdata <- rbind(dfWD, dfWE)

library(lattice)
xyplot(avg ~ interval | day, data = dfdata, layout = c(1, 2), type = "l", xlab = "Time Interval", ylab = "Number of steps", main = "Time Series Plot of Number of Steps vs Time Interval")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
