# Reproducible Research: Peer Assessment 1

## Load necessary libraries

```r
library(ggplot2)
library(data.table)
```


## Loading and preprocessing the data

```r
setwd("E:/documents/ebooks/ML_AI/RepData_PeerAssessment1")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
total_steps_per_day <- aggregate(steps ~ date, data, sum)

ggplot(data=total_steps_per_day,aes(x=steps)) + geom_histogram(binwidth=5000)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
mean_steps_per_day <- aggregate(steps ~ date, data, mean)
```


## What is the average daily activity pattern?

```r
mean_steps_by_interval <- aggregate(steps ~ interval, data, mean)
ggplot(data=mean_steps_by_interval,aes(x=interval,y=steps)) + geom_line()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
subset(mean_steps_by_interval,steps==max(steps))$interval
```

```
## [1] 835
```


## Imputing missing values

```r
count_rows_with_missing_values <- sum(!complete.cases(data))
```


## Are there differences in activity patterns between weekdays and weekends?
