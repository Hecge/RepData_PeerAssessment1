---
title: "RepData1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
df <- read.csv(unz("activity.zip", "activity.csv"))
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df_complete <- df[complete.cases(df), ]
```

## What is the mean total number of steps taken per day?

Summary of the steps:

```r
summary(df_complete$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   12.00  806.00
```

Histogram of the total number of steps taken each day:

```r
dfsum <- df_complete %>% group_by(date) %>% summarise_at("steps", sum, na.rm = TRUE)
hist(dfsum$steps, breaks = 30, xlab = "Number of steps per day", main = "")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

Mean of the total number of steps taken per day:

```r
mean(dfsum$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day:

```r
median(dfsum$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
dfint <- df_complete %>% group_by(interval) %>% summarise_at("steps", mean, na.rm = TRUE)
ggplot(dfint, aes(interval, steps)) + labs(title = "Average daily activity pattern", y = "Average number of steps")  + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

5-minute interval with maximum average number of steps

```r
dfint[which.max(dfint$steps), ][[1]]
```

```
## [1] 835
```

Average number of steps in that interval

```r
dfint[which.max(dfint$steps), ][[2]]
```

```
## [1] 206.1698
```

## Imputting missing values

Number of missing values: 

```r
sum(!complete.cases(df))
```

```
## [1] 2304
```

Filling a new dataframe with the average value for that 5-minute interval:

```r
df2 <- df

for (i in unique(df$interval)){
  df2[which(is.na(df$steps) & df$interval == i), 1] <- dfint[which(dfint$interval == i), 2]
}
```

Number of missing values with the new dataframe:

```r
sum(!complete.cases(df2))
```

```
## [1] 0
```

Summary of the steps:

```r
summary(df2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```

Histogram of the total number of steps taken each day:

```r
df2sum <- df2 %>% group_by(date) %>% summarise_at("steps", sum, na.rm = TRUE)
hist(df2sum$steps, breaks = 30, xlab = "Number of steps per day", main = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean of the total number of steps taken per day:

```r
mean(df2sum$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day:

```r
median(df2sum$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Creating type of day factor variable:

```r
df2["daytype"] = "weekday"
df2[which(weekdays(df2$date, abbreviate = TRUE) == "sá."), 4] <- "weekend"
df2[which(weekdays(df2$date, abbreviate = TRUE) == "do."), 4] <- "weekend"
df2$daytype <- as.factor(df2$daytype)
```

Creating comparison plot:

```r
df3 <- df2 %>% group_by(interval, daytype) %>% summarise_at("steps", mean, na.rm = TRUE)

ggplot(df3, aes(interval, steps)) + geom_line(aes(col = daytype)) + labs(title = "Activity pattern between weekdays and weekends", y = "Average number of steps") + facet_grid(rows = vars(daytype))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
