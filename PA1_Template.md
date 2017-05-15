Reproducible Research: Project Assignment 1
================

### Loading and Pre-Prepocessing the Data:

``` r
setwd('C:/Users/ShopDemo/Documents/GitHub/Reproducible_Research')
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists("fit_data.zip")){download.file(url,"fit_data.zip")}
if(!file.exists("activity.csv")){unzip("fit_data.zip")}

library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mice)

data <- read.csv('activity.csv')
```

### What is mean total number of steps taken per day?

``` r
grouped_data <- group_by(data, date)

steps <- data %>%
   filter(!is.na(steps))%>%
   group_by(date) %>%
   summarise(total_steps = sum(steps))
             
hist(steps$total_steps,breaks = 20, main = "Total Steps per Day", xlab = "Steps")
```

![](PA1_Template_files/figure-markdown_github/Daily%20Steps%20Taken-1.png)

``` r
mean_steps <- mean(steps$total_steps) 
median_steps <- median(steps$total_steps)
```

The mean and median daily steps taken are 1.076618910^{4} and 10765 respectively.

### What is average daily activity pattern?

``` r
avg_daily <- data %>%
  filter(!is.na(steps))%>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

with(avg_daily, plot(formula = steps ~ interval, type ="l"))
```

![](PA1_Template_files/figure-markdown_github/Daily%20Activity%20Pattern-1.png)

``` r
x <- as.data.frame(avg_daily[which.max(avg_daily$steps),])
```

The 5-minute interval that contains the maximum number of steps is interval 835 with 206.1698113 steps

### Imputing missing values

``` r
#3.1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

na <- is.na(data$steps)
total_NAs <- sum(na)
```

There are 2304 missing values

``` r
#3.2: Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day,
#or the mean for that 5-minute interval, etc.
imputed_Data <- mice(data, m = 2, method ="pmm")
```

    ## 
    ##  iter imp variable
    ##   1   1  steps
    ##   1   2  steps
    ##   2   1  steps
    ##   2   2  steps
    ##   3   1  steps
    ##   3   2  steps
    ##   4   1  steps
    ##   4   2  steps
    ##   5   1  steps
    ##   5   2  steps

``` r
imputed_data <- complete(imputed_Data,2)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputed_steps <- imputed_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

par(mfrow=c(2,1))
hist(imputed_steps$total_steps,breaks = 20, main = "Total Steps per Day (With Imputations)", xlab = "Steps", col = 'red')
abline(v= mean(imputed_steps$total_steps), lwd =2 , col ="green")
abline(v= median(imputed_steps$total_steps), lwd =2 , col ="blue")

hist(steps$total_steps,breaks = 20, main = "Total Steps per Day (Without Imputations)", xlab = "Steps", col = 'orange')
abline(v= mean_steps, lwd =4 , col ="green")
```

![](PA1_Template_files/figure-markdown_github/Imputing%20Missing%20Values-1.png)

### Are there differences in activity patterns between weekdays and weekends?

``` r
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

imputed_data <- imputed_data %>%
   mutate(date = parse_date(date), day = weekdays(date),
          weekday = ifelse(day == "Saturday" | day == "Sunday", "Weekend", "Weekday"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



weekday <- subset(imputed_data, weekday=="Weekday")
weekend <- subset(imputed_data, weekday=="Weekend")

weekend <- weekend %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))


weekday <- weekday %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

par(mfrow=c(2,1))


with(weekday, plot(formula = steps ~ interval, type ="l", col = "blue", main = "Weekday"))
with(weekend, plot(formula = steps ~ interval, type ="l", col = "blue", main = "Weekend"))
```

![](PA1_Template_files/figure-markdown_github/Differences%20in%20Activity%20Patterns%20between%20Weekdays%20and%20Weekends-1.png)
