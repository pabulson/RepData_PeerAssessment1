---
title: "Steps Analysis"
author: "Paul Bulson"
date: "Friday, November 14, 2014"
output: html_document
---

The advent of the personal activity monitoring device enables a better understanding of an individuals behaviour. For example, one individuals steps where recorded at 5-minute interval during October 2012 through November 2012. The resulting dataset is available at <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.

To better understand this data, load it into R and take advantage of its capabilities.


```r
# load the data
stepsData <- read.csv("activity.csv", header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
# process the data
stepsPerDay <- aggregate(steps ~ date, data=stepsData, sum)
stepsPerIntervalMean <- aggregate(steps ~ interval, data=stepsData, mean)
```

Generating a histogram of the total number of steps taken each day, gives an approximately normal distribution.


```r
# generate histogram (graphical representation of the distribution of data) of the total number of steps taken each day
library(ggplot2)
plot = ggplot(data=stepsPerDay, aes(x=steps)) 
plot = plot + geom_histogram(colour="darkgreen", fill="lightgreen", binwidth=2000)
plot = plot + ggtitle("Frequency of the Total Number of Steps Taken Each Day")
plot = plot + theme(plot.title = element_text(face="bold"))
plot
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Given the frequency distribution appears normal, one would expect the mean and median to be relatively equal.


```r
# calculate the mean/median total number of steps taken per day
options(scipen=999)
stepsPerDayMean <- round(mean(stepsPerDay$steps))
stepsPerDaySD <- round(sd(stepsPerDay$steps))
stepsPerDayMedian <- median(stepsPerDay$steps)
```

The mean value for steps per day is 10766 with a standard deviation of 4269. The median value is 10765.

Calculating the mean for each time interval provides some idea of the distribution of steps over the course of the day.


```r
# time series plot of the 5 minute interval (x-axis) and the average number of steps taken, averages across all days (y-axis)
plot1 = ggplot(data=stepsPerIntervalMean, aes(interval, steps)) 
plot1 = plot1 + geom_line(colour="blue", size=1) 
plot1 = plot1 + ggtitle("Average Number of Steps Taken per 5-minute Interval")
plot1 = plot1 + theme(plot.title = element_text(face="bold"))
plot1
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


```r
# calculate the 5 minute interval that on average across all the days in the dataset contains the maximum number of steps
interalWithMostSteps <- stepsPerIntervalMean[stepsPerIntervalMean$steps==max(stepsPerIntervalMean$steps),"interval"]
```

The time interval with highest mean number of steps (that peak between 500 and 1000) is 835. 


```r
# calculate and report the total number of missing values (total number of rows with NA)
numberOfNAs <- sum(is.na(stepsData$steps))
```

Unfortunately, the dataset contains 2304. If we replace the NAs with mean for that interval, will make it a difference?


```r
# replace NA with time interval mean
stepsDataMean <- read.csv("activity.csv", header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
for (i in 1:nrow(stepsDataMean)) {
        if(is.na(stepsDataMean$steps[i])==TRUE) {
                stepsDataMean$steps[i] <- stepsPerIntervalMean$steps[stepsPerIntervalMean$interval == stepsDataMean$interval[i]]
        }
}
```



```r
# generate histogram (graphical representation of the distribution of data) of the total number of steps taken each day
stepsPerDay <- aggregate(steps ~ date, data=stepsDataMean, sum)
plot2 = ggplot(data=stepsPerDay, aes(x=steps)) 
plot2 = plot2 + geom_histogram(colour="darkblue", fill="lightblue", binwidth=2000)
plot2 = plot2 + ggtitle("Frequency of the Total Number of Steps Taken Each Day (Replacing NA with interval mean")
plot2 = plot2 + theme(plot.title = element_text(face="bold"))
plot2
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


A quick comparison of the two histograms, the first with the NAs, the second without the NAs and the mean for that time interval instead.


```r
library(gridExtra)
grid.arrange(plot1, plot2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

The removeal of the datapoints narrows the distribution as represented by the mean and standard deviation.


```r
# calculate the mean/median total number of steps taken per day
stepsPerDayMeanNAsRemoved <- round(mean(stepsPerDay$steps))
stepsPerDaySDNAsRemoved <- round(sd(stepsPerDay$steps))
stepsPerDayMedianNAsRemoved <- round(median(stepsPerDay$steps))
```

Recall, the mean value for steps per day is 10766 with a standard deviation of 4269 for the dataset with NAs. After the replacements of the NAs, the mean value for steps per day is 10766 with a standard deviation of 3974. The median value is 10766.

Does this individual display different behaviours during the week than on the weekend? 


```r
# create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given data is a weekday or weekend day
# opting to the mean as the imputation of choice
stepsDataMean$day <- weekdays(as.Date(stepsDataMean$date))
stepsDataMean$dayClassification <- ifelse(stepsDataMean$day=="Saturday" | stepsDataMean$day=="Sunday", "weekend", "weekday")

stepsPerIntervalMeanWeekday <- aggregate(steps ~ interval, data=subset(stepsDataMean,dayClassification=="weekday"), mean)
stepsPerIntervalMeanWeekend <- aggregate(steps ~ interval, data=subset(stepsDataMean,dayClassification=="weekend"), mean)
```



```r
plotWeekday = ggplot(data=stepsPerIntervalMeanWeekday, aes(x=interval,y=steps)) 
plotWeekday = plotWeekday + geom_line(colour="darkblue")
plotWeekday = plotWeekday + ggtitle("Weekday Mean Steps Per Interval")
plotWeekday = plotWeekday + theme(plot.title = element_text(face="bold"))

plotWeekend = ggplot(data=stepsPerIntervalMeanWeekend, aes(x=interval,y=steps)) 
plotWeekend = plotWeekend + geom_line(colour="darkblue")
plotWeekend = plotWeekend + ggtitle("Weekend Mean Steps Per Interval")
plotWeekend = plotWeekend + theme(plot.title = element_text(face="bold"))

grid.arrange(plotWeekday, plotWeekend)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Most certainly yes. The steps start and end later in the day. 
