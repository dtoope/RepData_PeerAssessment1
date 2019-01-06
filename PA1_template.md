INTRODUCTION It is now possible to collect a large amount of data about
personal movement using activity monitoring devices such as a Fitbit,
Nike Fuelband, or Jawbone Up.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

DATA The dataset contains 3 variables and 17568 observations over a
period of 61 days, October and November 2012.

Variables:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format,
there are 61 days in the dataset.

interval: Identifier for the 5-minute interval in which measurement was
taken, There are 288 intervals per day (24 hours \* 12 intervals of 5
minutes per hour), numbered from 0 to 2355. Interval 115 will be the 5
minutes interval starting at 01.15am. For each day, the first interval
is 0 (00.00pm), the last interval is 2355 (11.55pm)

The elements below follow the structure proposed on the assignment
page(<https://www.coursera.org/learn/reproducible-research/peer/gYyPt/course-project-1>)

Loading and preprocessing the data
----------------------------------

    unzip("repdata_data_activity.zip")
    data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
    data$month <- as.numeric(format(data$date, "%m"))
    clean_data <- na.omit(data)
    rownames(clean_data) <- 1:nrow(clean_data)
    library(ggplot2)

What is mean total number of steps taken per day?
-------------------------------------------------

    # Plot total number of steps each day in a histogram
    library(ggplot2)

    ggplot(clean_data, aes(date, steps)) + 
    geom_bar(stat = "identity", colour = "red", fill="red", width = 0.7) + 
      facet_grid(. ~ month, scales = "free") + 
      labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # Determine the mean and median
    TotalSteps <- aggregate(clean_data$steps,list(clean_data$date),FUN="sum")
    MeanSteps <- mean(TotalSteps$x)
    MeanSteps

    ## [1] 10766.19

    MedianSteps <- median(TotalSteps$x)
    MedianSteps

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

    avgSteps <- aggregate(clean_data$steps, list(interval = as.numeric(as.character(clean_data$interval))), FUN = "mean")

    ggplot(avgSteps, aes(interval, avgSteps$x)) + geom_line(color = "red", size = 0.8) + 
      labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # Maximum Number of Average Steps
    avgSteps[avgSteps$x == max(avgSteps$x), ]

    ##     interval        x
    ## 104      835 206.1698

Imputing missing values
-----------------------

Missing data will be filled in using the mean of the 5 minute interval
for each corresponding interval.

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

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    newTotalSteps <- aggregate(Data2$steps, 
                               list(Date = Data2$date), 
                               FUN = "sum")$x

    # Mean and median of filled in data 
    newMean <- mean(newTotalSteps)
    newMean

    ## [1] 10766.19

    newMedian <- median(newTotalSteps)
    newMedian

    ## [1] 10766.19

    Meandiff <- newMean- MeanSteps
    Meandiff

    ## [1] 0

    Mediandiff <- newMedian - MedianSteps
    Mediandiff

    ## [1] 1.188679

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

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

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
