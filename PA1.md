# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
suppressPackageStartupMessages(library(dplyr))
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```r
if(file.exists("activity.zip")){
    unzip("activity.zip");
    df = data.frame(read.csv("activity.csv"));
}else{
    temp <- tempfile("activity.zip");
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "activity.zip", method = "curl");
    unzip("activity.zip");
    unlink(temp);
    df = data.frame(read.csv("activity.csv"));
}

df = mutate(df, date = as.Date(date, format = '%Y-%m-%d'));
head(df);
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
suppressPackageStartupMessages(library(ggplot2))
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
steps_per_day = df %>% filter(complete.cases(steps)) %>% group_by(date) %>% summarise(steps=sum(steps))

breaks <- pretty(range(steps_per_day$steps), n = nclass.FD(steps_per_day$steps), min.n = 1)
bwidth <- breaks[2]-breaks[1]
qplot(data=steps_per_day, steps, xlab="Steps per day", binwidth = bwidth);
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png) 


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
