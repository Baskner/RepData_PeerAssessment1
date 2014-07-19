# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
activity <- fread(unzip("activity.zip"))
```

## What is mean total number of steps taken per day?


```r
steps.per.day <- activity[, sum(steps, na.rm = T), by=date]$V1
hist(steps.per.day, breaks=15)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

the mean **9354.2** and median **10395** total number of steps taken per day


```r
steps.per.day <- aggregate(steps ~ date, activity, sum)$steps
hist(steps.per.day, breaks=15)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

the mean **10766.2** and median **10765** total number of steps taken per day

## What is the average daily activity pattern?


```r
d <- activity[, mean(steps, na.rm = T), by=interval]
setnames(d, "V1", "daily.activity")
with(d, plot(interval, daily.activity, type = "l"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
setkey(d, interval)
```

5-minute interval **835**, on average across all the days in the dataset, contains the maximum number of steps


## Imputing missing values

**2304** is the total number of missing values in the dataset. To impute missing values, the average daily activity per interval already computed as daily.activity are rounded to the nearest integer and inserted.


```r
DT <- copy(activity)
setkey(DT, interval)
DT <- merge(DT, d)

DT <- DT[, bb:= DT[, is.na(steps)] * d[, daily.activity]]

#DT <- DT[, bb:= DT[, is.na(steps)] * mean(steps.per.day)]
DT[is.na(steps), "steps"] <- DT[is.na(steps), as.integer(round(bb, 0))]

steps.per.day <- DT[, sum(steps), by=date]$V1

hist(steps.per.day, breaks=15)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

the mean **10765.6** and median **10762** total number of steps taken per day

## Are there differences in activity patterns between weekdays and weekends?

```r
DT$date <- as.Date(DT$date, format="%Y-%m-%d")
DT <- DT[, we := strftime(DT$date, "%u")>5]
DT$we <- factor(DT$we, levels=c(TRUE,FALSE), labels=c("weekend", "weekday"))

DT <- aggregate(steps ~ interval + we, DT, mean)

library(lattice)
with(DT, xyplot(steps~interval|we, 
     main="activity patterns between weekdays and weekends", 
     type = "l", 
     layout=c(1,2)))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
