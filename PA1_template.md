---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
unzip,read data into "raw"


```r
unzip("activity.zip")
raw<-read.csv("activity.csv",na.strings = "NA",header = TRUE)
raw$date<-as.Date(raw$date,"%Y-%m-%d")
```


## What is mean total number of steps taken per day?
Separate the data in term of date and produce a histogram

```r
step<-aggregate(steps~date,data = raw,FUN = sum)
hist(step$steps,main = "Total Number of Steps per Day",ylab="Steps",xlab = "Date")  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


Calculate the mean and median of steps

```r
data.frame(mean1=mean(step$steps),median=median(step$steps))
```

```
##      mean1 median
## 1 10766.19  10765
```



## What is the average daily activity pattern?
separate the steps in terms of intervals

```r
five<-aggregate(steps~interval,data = raw, FUN = mean)
with(five,plot(interval,steps,type = "l",ylab = "average number of steps taken",xlab = '5-minute interval' ))  
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
  determine the maximum steps among all intervals


```r
five[which.max(five$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values
Total NA

```r
sum(is.na(raw$steps))
```

```
## [1] 2304
```
Creating a new dataframe and Replacing NA's with total mean

```r
raw2<-raw
na<- which(is.na(raw2$steps))
meanNA<-rep(mean(raw2$steps,na.rm = TRUE),times=length(na))
raw2[na,1]<-meanNA
```
Finding differences  
Separate the data in term of date and produce a histogram  

```r
step2<-aggregate(steps~date,data = raw2,FUN = sum)
hist(step$steps,main = "Total Number of Steps per Day",ylab="Steps",xlab = "Date")  
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

  
  Calculate the mean and median of steps

```r
mean1=mean(step2$steps)
mean.diff= mean(step2$steps)-mean(step$steps)
median=median(step2$steps)
median.diff=median(step2$steps)-median(step$steps)
```
The difference in mean is 0  
The difference in median is `median.diff`

## Are there differences in activity patterns between weekdays and weekends?  
initialize new day

```r
raw2$weekday<-NA
```
inserting daytype column and compute for their types

```r
library(data.table)
week<-wday(raw2$date)

raw2$weekday<-NA
raw2$weekday<-lapply(raw2[,2],wday)
raw2$daytype<-NA

type<-function(date){if (date==1|date==7){"weekend"}
    else {"weekday"}
    }
   
raw2$daytype<-lapply(raw2[,4],type)
```
Change daytype column to factor type

```r
raw2$daytype<-factor(unlist(raw2$daytype))
```

plotting graph

```r
raw2$interval=as.numeric(raw2$interval)
g<-aggregate(steps~interval+daytype,data = raw2, FUN=mean)
library(lattice)
xyplot(steps~interval|factor(daytype),
       data = g,type="l",
       layout=c(1,2),
       ylab="average number of steps taken",xlab="5-minute interval" )
```

![](PA1_template_files/figure-html/last-1.png)<!-- -->


