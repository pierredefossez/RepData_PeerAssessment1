# Reproducible Research Assignment 1
PA Defossez  
26 mars 2017  




##Download data
Show any code that is needed to
    Load the data (i.e. read.csv())
    Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity <- read.csv("activity.csv", header=TRUE)
```

##Change format date

```r
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

    Make a histogram of the total number of steps taken each day

    

```r
histvalues <- activity %>% group_by(date) %>% summarize(total=sum(steps, na.rm=TRUE))

hist(histvalues$total, xlab="number of steps per day", main="histogram of total steps per day", breaks=10)
```

![](RR_Assignment1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day


```r
mean(histvalues$total)
```

```
## [1] 9354.23
```

```r
median(histvalues$total)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
timeseries <- activity %>% group_by(interval) %>% summarise (mean=mean(steps, na.rm=TRUE))

plot(timeseries$interval, timeseries$mean, type="l", xlab="time of day", ylab="avg nb of steps per 5 min")
```

![](RR_Assignment1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
timeseries[which.max(timeseries$mean),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

    Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. I used the mean for that 5-minute interval. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- cbind(activity, timeseries$mean)

names(activity2) <- c("steps","date","interval","default")

for (i in 1:length(activity2$interval)) {
        if (is.na(activity2[i,1])) {activity2[i,1] <- activity2[i,4]}
}
```

Re-draw histogram with the completed series

```r
histvalues2 <- activity2 %>% group_by(date) %>% summarize(total=sum(steps))

hist(histvalues2$total, xlab="number of steps per day", main="histogram of total steps per day, with imputed values", breaks=10)
```

![](RR_Assignment1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Re-calculate mean and median

```r
mean(histvalues2$total)
```

```
## [1] 10766.19
```

```r
median(histvalues2$total)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity2$day <- weekdays(activity2$date)

daytype <- function(day) {
        if (day %in% c("Samedi", "Dimanche")) {
                "weekend"
        } else {
                "weekday"
        }
}
activity2$daytype <- as.factor(sapply(activity2$day, daytype))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
par(mfrow=c(2,1), mar=c(4,4,2,1))
activity2WE <- filter(activity2, daytype=="weekend")
timeseriesWE <- activity2WE %>% group_by(interval) %>% summarise (mean=mean(steps))
plot(timeseriesWE$interval, timeseriesWE$mean, type="l", xlab="time of day (weekend)", ylab="", main="avg nb of steps per 5 min")


activity2WD <- filter(activity2, daytype=="weekday")
timeseriesWD <- activity2WD %>% group_by(interval) %>% summarise (mean=mean(steps))
plot(timeseriesWD$interval, timeseriesWD$mean, type="l", xlab="time of day (weekday)", ylab="")
```

![](RR_Assignment1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


