R Markdown
----------

### Set global option

``` r
knitr::opts_chunk$set(echo = TRUE)
Sys.setlocale("LC_TIME","us")
```

    ## [1] "English_United States.1252"

### library

``` r
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
```

### Loading and preprocessing the data

1.  Load the data

``` r
dat <- read.csv("activity.csv") 
```

1.  transform the data class into "tbl\_df"

``` r
dat <- dplyr::as_data_frame(dat)
print(dat)
```

    ## # A tibble: 17,568 x 3
    ##    steps date       interval
    ##    <int> <fct>         <int>
    ##  1    NA 2012-10-01        0
    ##  2    NA 2012-10-01        5
    ##  3    NA 2012-10-01       10
    ##  4    NA 2012-10-01       15
    ##  5    NA 2012-10-01       20
    ##  6    NA 2012-10-01       25
    ##  7    NA 2012-10-01       30
    ##  8    NA 2012-10-01       35
    ##  9    NA 2012-10-01       40
    ## 10    NA 2012-10-01       45
    ## # ... with 17,558 more rows

### What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

``` r
dat2 <- dat %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(total_steps = sum(steps,na.rm=TRUE))
print(dat2)
```

    ## # A tibble: 61 x 2
    ##    date       total_steps
    ##    <fct>            <int>
    ##  1 2012-10-01           0
    ##  2 2012-10-02         126
    ##  3 2012-10-03       11352
    ##  4 2012-10-04       12116
    ##  5 2012-10-05       13294
    ##  6 2012-10-06       15420
    ##  7 2012-10-07       11015
    ##  8 2012-10-08           0
    ##  9 2012-10-09       12811
    ## 10 2012-10-10        9900
    ## # ... with 51 more rows

1.  Make a histogram of the total number of steps taken each day

``` r
g <- ggplot(dat2,aes(x=total_steps))
g + 
 geom_histogram(binwidth = 5000 ,fill="white",colour="black",boundary=0) + 
 labs(x="total steps",title="histogram of the total number of steps taken each day") +
 geom_vline(aes(xintercept=mean(dat2$total_steps),color="mean"),show.legend = TRUE) + 
 geom_vline(aes(xintercept=median(dat2$total_steps),color="median"),show.legend = TRUE) +
 scale_color_manual(name = "statistics", values = c(mean = "red",median = "blue"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
mean <- mean(dat2$total_steps)
median <- median(dat2$total_steps)
c(mean=mean,median=median)
```

        mean   median 
     9354.23 10395.00 

### What is the average daily activity pattern?

1.  Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
dat3 <- dat %>%
        dplyr::group_by(interval) %>%
        dplyr::summarise(average_steps = mean(steps,na.rm=TRUE))
g <- ggplot(dat3,aes(x=interval,y=average_steps))
g + 
 geom_line(size = 1, colour="blue") + 
 labs(x="5-minute interval",y="average number of steps",
      title="the average daily activity pattern")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
dat3 %>%
  dplyr::filter(average_steps == max(average_steps)) %>%
  as.data.frame
```

      interval average_steps
    1      835      206.1698

### Imputing missing values

1.  Calculate and report the total number of missing values in the dataset

``` r
sum(is.na(dat))
```

    [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset.

**Filling in all of the missing values by the average number of steps taken each day and each interval**

First, calculate the average number of steps taken each day

``` r
ave_par_day <- dat %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(average_day = mean(steps,na.rm=TRUE)) 

#If average valu is "NaN", set the mean of the total number of steps across all day
ave_all_day <- mean(ave_par_day$average_day,na.rm=TRUE)
ave_par_day <- ave_par_day %>%
  dplyr::mutate(average_day = ifelse(is.na(average_day),ave_all_day,average_day)) %>%
  dplyr::mutate(weight = average_day/mean(average_day))
ave_par_day
```

    ## # A tibble: 61 x 3
    ##    date       average_day weight
    ##    <fct>            <dbl>  <dbl>
    ##  1 2012-10-01      37.4   1     
    ##  2 2012-10-02       0.438 0.0117
    ##  3 2012-10-03      39.4   1.05  
    ##  4 2012-10-04      42.1   1.13  
    ##  5 2012-10-05      46.2   1.23  
    ##  6 2012-10-06      53.5   1.43  
    ##  7 2012-10-07      38.2   1.02  
    ##  8 2012-10-08      37.4   1     
    ##  9 2012-10-09      44.5   1.19  
    ## 10 2012-10-10      34.4   0.920 
    ## # ... with 51 more rows

Second, calculate the average number of steps taken each interval

``` r
ave_par_interval <- dat %>%
        dplyr::group_by(interval) %>%
        dplyr::summarise(average_interval = mean(steps,na.rm=TRUE)) 
ave_par_interval
```

    ## # A tibble: 288 x 2
    ##    interval average_interval
    ##       <int>            <dbl>
    ##  1        0           1.72  
    ##  2        5           0.340 
    ##  3       10           0.132 
    ##  4       15           0.151 
    ##  5       20           0.0755
    ##  6       25           2.09  
    ##  7       30           0.528 
    ##  8       35           0.868 
    ##  9       40           0     
    ## 10       45           1.47  
    ## # ... with 278 more rows

Next, joining the average number of steps taken each day and each interval,and calculate the interpolation

``` r
interpolate_dat <- dat %>%
                   dplyr::inner_join(ave_par_day,by="date") %>% 
                   dplyr::inner_join(ave_par_interval,by="interval") %>% 
                   dplyr::mutate(fill = weight * average_interval) 
interpolate_dat
```

    ## # A tibble: 17,568 x 7
    ##    steps date       interval average_day weight average_interval   fill
    ##    <int> <fct>         <int>       <dbl>  <dbl>            <dbl>  <dbl>
    ##  1    NA 2012-10-01        0        37.4      1           1.72   1.72  
    ##  2    NA 2012-10-01        5        37.4      1           0.340  0.340 
    ##  3    NA 2012-10-01       10        37.4      1           0.132  0.132 
    ##  4    NA 2012-10-01       15        37.4      1           0.151  0.151 
    ##  5    NA 2012-10-01       20        37.4      1           0.0755 0.0755
    ##  6    NA 2012-10-01       25        37.4      1           2.09   2.09  
    ##  7    NA 2012-10-01       30        37.4      1           0.528  0.528 
    ##  8    NA 2012-10-01       35        37.4      1           0.868  0.868 
    ##  9    NA 2012-10-01       40        37.4      1           0      0     
    ## 10    NA 2012-10-01       45        37.4      1           1.47   1.47  
    ## # ... with 17,558 more rows

Finally, filling in all of the missing values

``` r
interpolate_dat <- interpolate_dat %>%        
   dplyr::mutate(interpolation = ifelse(is.na(steps),fill,steps)) %>%
   dplyr::select(-(average_day:fill))
interpolate_dat
```

    ## # A tibble: 17,568 x 4
    ##    steps date       interval interpolation
    ##    <int> <fct>         <int>         <dbl>
    ##  1    NA 2012-10-01        0        1.72  
    ##  2    NA 2012-10-01        5        0.340 
    ##  3    NA 2012-10-01       10        0.132 
    ##  4    NA 2012-10-01       15        0.151 
    ##  5    NA 2012-10-01       20        0.0755
    ##  6    NA 2012-10-01       25        2.09  
    ##  7    NA 2012-10-01       30        0.528 
    ##  8    NA 2012-10-01       35        0.868 
    ##  9    NA 2012-10-01       40        0     
    ## 10    NA 2012-10-01       45        1.47  
    ## # ... with 17,558 more rows

3.Create a new dataset that is equal to the original dataset but with the missing data filled in

``` r
interpolate_dat <- interpolate_dat %>%        
   dplyr::mutate(steps = interpolation) %>%
   dplyr::select(steps:interval)
interpolate_dat
```

    ## # A tibble: 17,568 x 3
    ##     steps date       interval
    ##     <dbl> <fct>         <int>
    ##  1 1.72   2012-10-01        0
    ##  2 0.340  2012-10-01        5
    ##  3 0.132  2012-10-01       10
    ##  4 0.151  2012-10-01       15
    ##  5 0.0755 2012-10-01       20
    ##  6 2.09   2012-10-01       25
    ##  7 0.528  2012-10-01       30
    ##  8 0.868  2012-10-01       35
    ##  9 0      2012-10-01       40
    ## 10 1.47   2012-10-01       45
    ## # ... with 17,558 more rows

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
interpolate_dat2 <- interpolate_dat %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(total_steps = sum(steps,na.rm=TRUE))

g <- ggplot(interpolate_dat2,aes(x=total_steps))
g + 
 geom_histogram(binwidth = 5000 ,fill="white",colour="black",boundary=0) + 
 labs(x="total steps",
      title="histogram of the total number of steps with the missing data filled in ") +
 geom_vline(aes(xintercept=mean(dat2$total_steps),color="mean_NA"),
            linetype='dashed',show.legend = TRUE) +
 geom_vline(aes(xintercept=mean(interpolate_dat2$total_steps),color="mean"),
            size=1,show.legend = TRUE) + 
 geom_vline(aes(xintercept=median(dat2$total_steps),color="median_NA"),
            linetype='dashed',show.legend = TRUE) +
 geom_vline(aes(xintercept=median(interpolate_dat2$total_steps),color="median"),
            size=1,show.legend = TRUE) +
 scale_color_manual(name = "statistics",
            values = c(mean_NA = "red",mean = "green",median_NA = "blue",median = "green"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-15-1.png)

Do these values differ from the estimates from the first part of the assignment?What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
mean_Fir <- mean(dat2$total_steps)
median_Fir <- median(dat2$total_steps)
mean_Sec <- mean(interpolate_dat2$total_steps)
median_Sec <- median(interpolate_dat2$total_steps)

df <- data.frame(mean=c(mean_Fir,mean_Sec,mean_Sec-mean_Fir),
                 median=c(median_Fir,median_Sec,median_Sec-median_Fir))
rownames(df) <- c("with the missing data","with the missing data filled in","difference")
df
```

    ##                                      mean     median
    ## with the missing data            9354.230 10395.0000
    ## with the missing data filled in 10766.189 10766.1887
    ## difference                       1411.959   371.1887

### Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend”

``` r
dat4 <- interpolate_dat %>%
  mutate(weekday = weekdays(lubridate::ymd(date),abbreviate=TRUE)) %>%
  mutate(weekend = factor(weekday,
   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
   labels = c("weekday", "weekday", "weekday", "weekday", "weekday", "weekend", "weekend")))
dat4
```

    ## # A tibble: 17,568 x 5
    ##     steps date       interval weekday weekend
    ##     <dbl> <fct>         <int> <chr>   <fct>  
    ##  1 1.72   2012-10-01        0 Mon     weekday
    ##  2 0.340  2012-10-01        5 Mon     weekday
    ##  3 0.132  2012-10-01       10 Mon     weekday
    ##  4 0.151  2012-10-01       15 Mon     weekday
    ##  5 0.0755 2012-10-01       20 Mon     weekday
    ##  6 2.09   2012-10-01       25 Mon     weekday
    ##  7 0.528  2012-10-01       30 Mon     weekday
    ##  8 0.868  2012-10-01       35 Mon     weekday
    ##  9 0      2012-10-01       40 Mon     weekday
    ## 10 1.47   2012-10-01       45 Mon     weekday
    ## # ... with 17,558 more rows

1.  Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
dat5 <- dat4 %>%
        dplyr::group_by(interval,weekend) %>%
        dplyr::summarise(average_steps = mean(steps,na.rm=TRUE))
g <- ggplot(dat5,aes(x=interval,y=average_steps,group=weekend))
g + 
 facet_grid(aes(group=weekend)) +
 geom_line(size = 1) + 
 labs(x="5-minute interval",y="average number of steps",
      title="the average daily activity pattern")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-18-1.png)
