---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## r setup


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
library(ggplot2)
```



## What is mean total number of steps taken per day?

```r
##storing total steps taken in each day in stepsPerday
stepsPerday <- aggregate(steps ~ date, activity, sum)
ggplot(activity, aes(date, steps))+
  labs(x = "date", y = "steps", main = "total no of steps taken per day")+
  scale_y_continuous(breaks = seq(0,22000, 1500))+
  scale_x_date(breaks = "3 days", date_labels = "%b %d")+geom_col()
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
##histogram of steps taken each day
hist((stepsPerday$steps), main = "histogram of steps taken each day", 
     xlab = "steps", col = "green", xlim = c(0,25000), las = 1, breaks = 40)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
## finding mean and median
data <- tapply(activity$steps, activity$date, sum)
mean(data, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(data, na.rm = TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
pattern <- aggregate(steps~ interval, activity, mean)

ggplot(pattern, aes(interval , steps))+ geom_line()+
  scale_x_continuous("day interval", breaks = seq(min(pattern$interval), max(pattern$interval)))+
  labs(y = "average no  of steps", main = "average no of steps in each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
##Maximum daily activity pattern
maximum <- subset(pattern, pattern$steps == max(pattern$steps))
maximum
```

```
##     interval    steps
## 104      835 206.1698
```



## Imputing missing values

```r
##no of rows with missing data
sum(is.na(activity))
```

```
## [1] 2304
```

```r
pattern <- aggregate(steps~ interval, activity, mean)
newactivity <- cbind(activity, pattern$steps)
colnames(newactivity) <- c("steps", "date", "interval", "meanSteps")

for(i in 1:nrow(newactivity)) {
  if (is.na(newactivity$steps[i])){
    newactivity$steps[i] <- newactivity$meanSteps[i]
  }
}           
## the new dataset with the missing data filled in is stored in newactivity

stepsPerday <- aggregate(steps ~ date, newactivity, sum)
ggplot(activity, aes(date, steps))+
  labs(x = "date", y = "steps", main = "total no of steps taken per day")+
  scale_y_continuous(breaks = seq(0,22000, 1500))+
  scale_x_date(breaks = "3 days", date_labels = "%b %d")+geom_col()
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
hist((stepsPerday$steps), main = "histogram of steps taken each day", 
     xlab = "steps", col = "green", xlim = c(0,25000), las = 1, breaks = 40)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
## old mean median
dataOld <- tapply(activity$steps, activity$date, sum)
mean(dataOld, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(dataOld, na.rm = TRUE)
```

```
## [1] 10765
```

```r
##new mean median
datanew <- tapply(newactivity$steps, newactivity$date, sum)
mean(datanew, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(datanew, na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
for (i in 1:nrow(newactivity)){
  if (weekdays(newactivity$date[i]) %in% c("Saturday", "Sunday")){
    newactivity$weekdaytype[i] <- "weekend"
  } else{
    newactivity$weekdaytype[i] <- "weekday"
  }
}

pattern <- aggregate(steps ~ interval + weekdaytype, newactivity, mean)
ggplot(pattern, aes(interval , steps))+ geom_line()+ facet_grid(weekdaytype ~.)+
  scale_x_continuous("day interval", breaks = seq(min(pattern$interval), max(pattern$interval)))+
  labs(y = "average no  of steps", main = "average no of steps in each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
