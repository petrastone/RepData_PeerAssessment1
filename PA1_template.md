# Reproducible Research Project 1 
Petra Stone  
3/3/2017  



## Loading and preprocessing the data

* Load the data
* Process/transform the data into a format suitable for analysis


```r
    if (!file.exists("activity.csv")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile = "activity.zip")
        unzip("activity.zip", zipfile="activity.csv")
    }
    data <- read.csv( "activity.csv" )
```
 
 Basic summaries:
 

```r
    summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
    str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert the date and time columns to dates:


```r
    data$dateTime <- as.POSIXct(paste(data$date, sprintf("%04d", data$interval)), format="%Y-%m-%d %H%M")
    data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. Calculate and report the mean and median of the total number of steps taken per day
3. Make a histogram of the total number of steps taken each day


```r
    require(dplyr)
    require(ggplot2)
    require(gridExtra)

    stepsPerDay <- data %>%
        group_by(date) %>%
        summarise(totalSteps=sum(steps, na.rm=T), totalIntervals=n())

    mean( stepsPerDay$totalSteps )
```

```
## [1] 9354.23
```

```r
    median( stepsPerDay$totalSteps )
```

```
## [1] 10395
```


```r
    ggplot(stepsPerDay, aes(x=totalSteps) ) +
        geom_histogram() + xlab("Total Steps Per Day") +
        geom_vline( aes(xintercept = mean( stepsPerDay$totalSteps ), colour="mean")) +
        geom_vline( aes(xintercept = median( stepsPerDay$totalSteps ), colour="median")) +
        scale_color_manual(name="",values=c("red","blue")) 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


The distibution of steps per day is a little skewed and bimodal with the mode value being zero steps and the median greater than the mean. 

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

We already know that mean steps per day is 9354 and median steps per day is 10395. Let's look more into the intervals ie. the 288 timestamps at 5 minute intervals which were recorded each of 61 days:

Steps per Interval:


```r
    stepsPerInterval <- data %>%
        group_by(interval) %>%
        summarise(totalSteps=sum(steps, na.rm=T), meanStepsPerInterval=mean(steps, na.rm=T))

    summary(stepsPerInterval)
```

```
##     interval        totalSteps      meanStepsPerInterval
##  Min.   :   0.0   Min.   :    0.0   Min.   :  0.000     
##  1st Qu.: 588.8   1st Qu.:  131.8   1st Qu.:  2.486     
##  Median :1177.5   Median : 1808.0   Median : 34.113     
##  Mean   :1177.5   Mean   : 1981.3   Mean   : 37.383     
##  3rd Qu.:1766.2   3rd Qu.: 2800.2   3rd Qu.: 52.835     
##  Max.   :2355.0   Max.   :10927.0   Max.   :206.170
```

```r
    ggplot( stepsPerInterval, 
                aes(y=meanStepsPerInterval, x=interval, alpha=.5) ) +
            geom_point()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

```r
    mostActiveInterval <- stepsPerInterval[which.max(stepsPerInterval$meanStepsPerInterval),]$interval
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Interval at **08:35** contains 206 steps on average.

    
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset
2. Devise a strategy for filling in all of the missing values in the dataset.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day
5. Calculate and report the mean and median total number of steps taken per day. 
6. Do these values differ from the estimates from the first part of the assignment? 
7. What is the impact of imputing missing data on the estimates of the total daily number of steps?


First let's check what % of the data is NA:


```r
    sum(is.na(data))
```

```
## [1] 2304
```

```r
    paste0(round(mean( is.na(data) )*100,2),"%")
```

```
## [1] "3.28%"
```

Substitute the NA values for steps with the mean for the given interval:


```r
    # Assign the mean per interval to the NAs
    imputed <- left_join(data, subset(stepsPerInterval, select=-c(totalSteps)) )
    imputed$steps[is.na(imputed$steps)] <- imputed$meanStepsPerInterval
    
    imputedStepsPerDay <- imputed %>%
        group_by(date) %>%
        summarise(totalSteps=sum(steps))
    summary(imputedStepsPerDay)
```

```
##       date              totalSteps   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

Plot the new distribution and mean and median:


```r
    ggplot( imputedStepsPerDay, aes(x=totalSteps) ) +
        geom_histogram() +
        geom_vline( aes(xintercept = mean( imputedStepsPerDay$totalSteps ), colour="mean")) +
        geom_vline( aes(xintercept = median( imputedStepsPerDay$totalSteps ), colour="median") ) +
        scale_color_manual(name="",values=c("red","blue")) +
        ggtitle("Total Steps per day\nusing imputed mean of interval for NA values of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

The mean and median are now equal to each other and the distribution appears more normal:


```r
    require(knitr)
    kable( caption="Total Steps Per Day",data.frame(
            Statistic=c("mean", "median"), 
            Data=c(mean( stepsPerDay$totalSteps ), median( stepsPerDay$totalSteps ) ), 
            ImputedData=c(mean( imputedStepsPerDay$totalSteps ), median( imputedStepsPerDay$totalSteps ))
            ),
           digits = c(0,0,0)
    )
```



Table: Total Steps Per Day

Statistic     Data   ImputedData
----------  ------  ------------
mean          9354         10766
median       10395         10766


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset indicating whether a given date is a weekday or weekend day.
2. Make a panel plot of the 5-minute interval and the average number of steps taken across all weekdays or weekend days.


```r
    require(Hmisc)
    imputed$DayOfWeek <- factor( 
        ifelse( !weekdays(imputed$date) %in% c("Saturday","Sunday"), "weekday", "weekend") 
    )
    
    panels <- imputed %>%
        group_by(interval,
                 DayOfWeek) %>%
        summarise(meanStepsPerInterval=mean(steps))
    
    ggplot(panels, 
            aes(x=interval, y=meanStepsPerInterval, color=DayOfWeek, group=DayOfWeek)) +
        geom_line() +
        scale_x_continuous(labels=rep(0:23), breaks=seq(0,2355, by=100)) +
        facet_grid(~DayOfWeek)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

* The graph for weekends does not have the same extreme peak in mean steps as we see on weekdays between 8am and 9am.
* For the 5am to 8am window there is more activity on weekdays.
* On weekends there are more peaks in activity between 8am and 21pm than weekdays.
* On weekends there is a peak in activity between 20pm and 21pm which does not exist on weekdays.


We might be interested in comparing Saturday to Sunday:


```r
    imputed$DayOfWeek2 <- factor( 
        ifelse( !weekdays(imputed$date) %in% c("Saturday","Sunday"), "Weekdays", 
                ifelse(weekdays(imputed$date)=="Saturday", "Saturdays", "Sundays") ) 
    )
    
    panels2 <- imputed %>%
        group_by(interval,
                 DayOfWeek2) %>%
        summarise(meanStepsPerInterval=mean(steps))
    
    ggplot(panels2, 
            aes(x=interval, y=meanStepsPerInterval, color=DayOfWeek2, group=DayOfWeek2)) +
        geom_line() +
        scale_x_continuous(labels=rep(0:23), breaks=seq(0,2355, by=100)) +
        facet_grid(~DayOfWeek2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

* We can see that Saturday looks a lot different that Sunday.
* It even surpasses the early morning peak for weekdays.
