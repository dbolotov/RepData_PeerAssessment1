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
#convert date to Date format
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
#calculate total number of steps per day
total_steps_per_day <- aggregate(steps ~ date, data, sum)

#plot histogram
ggplot(data=total_steps_per_day,aes(x=steps)) + geom_histogram(binwidth=1000) +
    labs(title="Total Steps per Day",x="Steps per day",y="Count")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
#calculate mean and median total steps per day
mean_tot_steps_per_day <- round(mean(total_steps_per_day$steps,na.rm=T))
median_tot_steps_per_day <- median(total_steps_per_day$steps,na.rm=T)
```

The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>. The median total number of steps taken per day is 10765.




## What is the average daily activity pattern?

```r
#aggregate step data by interval
mean_steps_by_interval <- aggregate(steps ~ interval, data, mean)

#plot time series of 5-minute intervals and the average steps taken
ggplot(data=mean_steps_by_interval,aes(x=interval,y=steps)) + geom_line() +
    labs(title="Average Steps by Interval",x="Interval",y="Mean number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
#find interval with max steps
interval_with_max_steps <- subset(mean_steps_by_interval,steps==max(steps))$interval
```

The 5-minute interval that contains the maximum number of steps is 835.

## Imputing missing values

```r
#calculate total number of rows with missing values
count_rows_with_NA <- sum(!complete.cases(data))
```

The data contains 2304 rows with missing values.

The imputation strategy is to replace each NA with the rounded mean value for the corresponding 5-minute interval (using the dataset calculated above, where steps are averaged by interval across all days).


```r
#create new df and impute NA values using rounded mean value for the interval
data_im <- data

for (j in 1:nrow(data_im)) {
    if(is.na(data_im[j,1])){
        data_im[j,1] <- round(subset(mean_steps_by_interval,interval==data_im$interval[j])[2])
        }
}

#calculate total number of steps per day with imputed values
total_steps_per_day_im <- aggregate(steps ~ date, data_im, sum)

#plot histogram with imputed values
ggplot(data=total_steps_per_day_im,aes(x=steps)) + geom_histogram(binwidth=1000) +
    labs(title="Total Steps per Day with Imputed Values",x="Steps per day",y="Count")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
#calculate mean and median total steps per day with imputed values
mean_tot_steps_per_day_im <- round(mean(total_steps_per_day_im$steps,na.rm=T))
median_tot_steps_per_day_im <- median(total_steps_per_day_im$steps,na.rm=T)
```

With imputed values, the mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>. The median total number of steps taken per day is 1.0762 &times; 10<sup>4</sup>.

These values differ from the estimates where NA values were omitted.The following code plots both datasets on the same panel to show how imputation affects the number of steps. Days containing missing values do not contain any non-NA entries, which means that days with imputed values will all get the same mean total steps per day. This is evident in the plot below, where the red bar shows the additional counts for the imputed dataset.

```r
#plot histogram
ggplot(data=total_steps_per_day,aes(x=steps)) + 
    geom_histogram(binwidth=1000,fill="blue",alpha=0.5) +
    geom_histogram(data=total_steps_per_day_im,binwidth=1000,fill="red",alpha=0.5) + 
    labs(title="Total Steps per Day: with Missing vs Imputed Values",x="Steps per day",y="Count")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


## Are there differences in activity patterns between weekdays and weekends?

The following code plots steps taken in each 5-minute interval, averaged across all weekends or all weekdays. There is a clear difference between these two activity patterns; for example, weekdays show more activity earlier in the day, possibly corresponding to getting ready for and traveling to work.


```r
#create new factor variable (weekend or weekday)
data_im$day_type <- as.factor(ifelse(weekdays(data_im$date) %in% c("Saturday","Sunday"),"weekend","weekday"))

#aggregate step data by interval and day type
mean_steps_by_interval_by_day_type <- aggregate(steps ~ interval+day_type, data_im, mean)

#plot time series of 5-minute intervals and the average steps taken, grouped by day type
ggplot(data=mean_steps_by_interval_by_day_type,aes(x=interval,y=steps)) +
    geom_line() +
    facet_grid(day_type ~ .) + 
    labs(Title="Average Steps by Interval and Day Type",x="Interval",y="Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
