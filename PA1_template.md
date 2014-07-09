# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First, we will load the data by reading from the data source - a csv file stored in a zip file.

```r
data <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", 
                 sep=",",colClasses=c("numeric","factor","numeric"))
```

## What is mean total number of steps taken per day?
To answer this question we will plot a histogramm showing the mean total number of steps taken per day.

```r
tot_steps_day <- setNames(aggregate(data$steps, by=list(data$date), FUN=sum,na.rm=TRUE),
                          c("Day","Total_Steps"))
```

```r
library("ggplot2")
ggplot(tot_steps_day, aes(x=Total_Steps)) + geom_histogram(binwidth = diff(range(tot_steps_day$Total_Steps))/30) + xlab("Total number of steps taken per day") 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Next, we will calculate and report the mean and median total number of steps taken per day.

```r
meanmedian <- c(mean(data$steps,na.rm=TRUE),median(data$steps,na.rm = TRUE))
mmdf <- setNames(data.frame(meanmedian[1],meanmedian[2]),c("Mean","Median")) ##Transform to Data Frame for kable
```

```r
kable(mmdf, digits=2)
```



|  Mean| Median|
|-----:|------:|
| 37.38|      0|
## What is the average daily activity pattern?

```r
daily_act_pat <- setNames(aggregate(data$steps, by=list(data$interval), FUN=mean,na.rm=TRUE),                      c("Interval","Mean_Steps"))
maximum <- setNames(c(daily_act_pat$Interval[daily_act_pat$Mean_Steps==max(daily_act_pat$Mean_Steps)],max(daily_act_pat$Mean_Steps)), c("Interval","Maximum_Steps"))
ggplot(data=daily_act_pat, aes(x=Interval, y=Mean_Steps)) + geom_line() + geom_point(data=daily_act_pat[daily_act_pat$Interval==835,], aes(x=Interval, y=Mean_Steps), colour="red", size=5) + geom_text(data = NULL, x = maximum[1]+450,y = maximum[2], aes(label = paste("Maximum  (",as.character(round(maximum[1],1))," / ",as.character(round(maximum[2],1)), ")",sep="")), parse = TRUE)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
