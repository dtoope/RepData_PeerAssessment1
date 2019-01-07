---
title: "Reproducible Research Assignment 1"
author: "David Toope"
date: "January 5, 2019"
output: 
  html_document: 
    keep_md: yes
---
INTRODUCTION
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

DATA
The dataset contains 3 variables and 17568 observations over a period of 61 days, October and November 2012.

Variables:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format, there are 61 days in the dataset.

interval: Identifier for the 5-minute interval in which measurement was taken, There are 288 intervals per day (24 hours * 12 intervals of 5 minutes per hour), numbered from 0 to 2355. Interval 115 will be the 5 minutes interval starting at 01.15am. For each day, the first interval is 0 (00.00pm), the last interval is 2355 (11.55pm)

The elements below follow the structure proposed on the assignment page(https://www.coursera.org/learn/reproducible-research/peer/gYyPt/course-project-1)


## Loading and preprocessing the data


```r
unzip("repdata_data_activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
clean_data <- na.omit(data)
rownames(clean_data) <- 1:nrow(clean_data)
library(ggplot2)
```



## What is mean total number of steps taken per day?

```r
ggplot(clean_data, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](figure/unnamed-chunk-2-1.png)<!-- -->

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalSteps <- aggregate(clean_data$steps, list(Date = clean_data$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate(clean_data$steps, list(interval = as.numeric(as.character(clean_data$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"
ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](figure/unnamed-chunk-3-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- data 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}
head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(newData))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

![](figure/unnamed-chunk-9-1.png)<!-- -->

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](figure/unnamed-chunk-14-1.png)<!-- -->

```

```r
TotalSteps <- aggregate(clean_data$steps,list(clean_data$date),FUN="sum")
MeanSteps <- mean(TotalSteps$x)
MeanSteps
```

```
## [1] 10766.19
```

```r
MedianSteps <- median(TotalSteps$x)
MedianSteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
avgSteps <- aggregate(clean_data$steps, list(interval = as.numeric(as.character(clean_data$interval))), FUN = "mean")

ggplot(avgSteps, aes(interval, avgSteps$x)) + geom_line(color = "red", size = 0.8) + 
  labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](figure/unnamed-chunk-16-1.png)<!-- -->

```r
# Maximum Number of Average Steps
avgSteps[avgSteps$x == max(avgSteps$x), ]
```

```
##     interval        x
## 104      835 206.1698
```


## Imputing missing values

Missing data will be filled in using the mean of the 5 minute interval for each corresponding interval. 


```r
Interval_max <- avgSteps[avgSteps$x == max(avgSteps$x),]
Number_rows <- sum(is.na(data))

Data2 <- data
for (i in 1:nrow(Data2)) {
  if (is.na(Data2$steps[i])) {
    Data2$steps[i] <- avgSteps[which(Data2$interval[i] == avgSteps$interval), ]$x
  }
}
ggplot(Data2, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "Red",
                                             fill = "Red",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + 
  labs(title = "Histogram of Total Number of Steps Taken Each Day (data fill-in)", x = "Date", y = "Total number of steps")
```

![](figure/unnamed-chunk-17-1.png)<!-- -->

```r
newTotalSteps <- aggregate(Data2$steps, 
                           list(Date = Data2$date), 
                           FUN = "sum")$x

# Mean and median of filled in data 
newMean <- mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```

```r
newMedian <- median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```

```r
Meandiff <- newMean- MeanSteps
Meandiff
```

```
## [1] 0
```

```r
Mediandiff <- newMedian - MedianSteps
Mediandiff
```

```
## [1] 1.188679
```


## Are there differences in activity patterns between weekdays and weekends?


```r
Data2$weekdays <- factor(format(Data2$date, "%A"))
levels(Data2$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))

library(lattice)

# xyplot of number of steps over each interval by weekend and weekday. 
avgSteps <- aggregate(Data2$steps, 
                      list(interval = as.numeric(as.character(Data2$interval)), 
                           weekdays = Data2$weekdays),
                      FUN = "mean")
xyplot(avgSteps$x ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](figure/unnamed-chunk-18-1.png)<!-- -->

