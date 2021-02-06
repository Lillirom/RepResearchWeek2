---
output: 
  html_document: 
    keep_md: yes
---
# Reproducible Research
## Course Project 1

### Loading and preprocessing data
Load the data with the following code.

```r
data <- read.csv("activity.csv")
```

###What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.

```r
stepsperday <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```


2. Make a histogram of the total number of steps taken each day

```r
hist(stepsperday, 
     xlab="Steps per day", 
     main="Histogram for steps per day")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_spd <- mean(stepsperday)
median_spd <- median(stepsperday)
```
The mean of the total number of steps taken per day is 9354.2295082 and the median 10395.


### What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
meansteps_perint <- tapply(data$steps, data$interval, FUN=mean, na.rm=TRUE)

plot(meansteps_perint, 
     type="l",
     xlab="5-minute intervals", 
     ylab="Average number of steps", 
     main="Avereage number of steps taken per 5-minute interval across all days")
```

![](PA1_template_files/figure-html/time-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_int_num <- which.max(meansteps_perint)
```
The 5-minute interval number 104 contains the maximum number of steps on average across all the days in the dataset.


### Imputing missing values
1. Calculate and report the total number of missing values in the dataset


```r
count_na <- sum(is.na(data$steps), is.na(data$date), is.na(data$interval))
```
The total number of missing values is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset.

Missing values are replaced by the mean of steps of the day.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(data.table)

dataclean = as.data.table(data, keep.rownames=TRUE)

mean_info <- dataclean[,.(steps_mean=mean(steps, na.rm=TRUE)),interval]

setkey(dataclean, interval)
setkey(mean_info, interval)

dataclean <- merge(dataclean, mean_info, all.x=TRUE)

dataclean$steps_clean <- ifelse(
                          is.na(dataclean$steps), 
                          dataclean$steps_mean, 
                          dataclean$steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsperday_clean <- tapply(dataclean$steps_clean, dataclean$date, FUN=sum)

hist(stepsperday_clean, 
     xlab="Steps per day (with NA replaced)", 
     main="Histogram for steps per day")
```

![](PA1_template_files/figure-html/stepsperday_clean-1.png)<!-- -->

```r
mean_spd_clean <- mean(stepsperday_clean)

median_spd_clean <- median(stepsperday_clean)
```

On the data set with replaced missing values the mean is 1.0766189\times 10^{4}and the median 1.0766189\times 10^{4}. In comparison to that in the first part of the assignment where misssing values weren't replaced the mean was 9354.2295082 and the median 10395. By replacing missing values with mean values the new mean and median rise and are equal now.

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(date)
dataclean$day <- weekdays(as.Date(dataclean$date))
dataclean$weekday <- ifelse(
                      dataclean$day %in% c("Samstag","Sonntag"), 
                      "weekend", 
                      "weekday")
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
cleanstepsperint <- aggregate(steps_clean ~ interval+weekday, 
                              dataclean,mean)
xyplot(cleanstepsperint$steps_clean ~ cleanstepsperint$interval | cleanstepsperint$weekday, 
      type="l",
      layout=c(1,2), 
      xlab="5-minute-interval", 
      ylab="Mean steps per interval")
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->

