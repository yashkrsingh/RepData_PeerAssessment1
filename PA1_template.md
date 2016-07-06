# Reproducible Research RMarkdown

This is the R Markdown file for the *Course Assignment 1* of Reproducible Research. The document is divided into sections with each section answering the asked question for the assignment.

### Loading and preprocessing the data


```r
data <- read.csv("~/Reproducible-Research-Assignment-1-Coursera/activity.csv")
omit <- na.omit(data)
```

### What is mean total number of steps taken per day?

Histogram for total number of steps taken each day


```r
library(ggplot2)
test <- aggregate(steps ~ date, omit, sum)
g <- ggplot(data = test, aes(steps))
g <- g + geom_histogram(bins = 20,col = "black", fill = "blue", alpha = 0.7) 
g <- g + labs(x = "Number of steps", y = "Frequency", title = "Number of Steps Taken Each Day")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and Median of the total number of steps taken per day


```r
test <- aggregate(steps ~ date, omit, sum)
rmean <- mean(test$steps)
rmedian <- median(test$steps)
```

* Mean: 1.0766189\times 10^{4}
* Median: 10765

### What is the average daily activity pattern?

Plot for average number of steps taken averaged across all days


```r
test <- aggregate(steps ~ interval, omit, mean)
g <- ggplot(data = test, aes(interval, steps)) 
g <- g + geom_line(col = "red")
g <- g + labs(x = "Time Intervals", y = "Average Steps", title = "Average Number Of Steps Taken Across All Days")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Among this average across all days, the interval containing maximum number of steps can be found by


```r
test[test$steps == max(test$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values

Total number of missing values in the dataset can be given by


```r
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

Filling in the missing values using the mean of the 5-minute interval across all days. New dataset named 'newdata' has missing values filled in.


```r
newdata <- data
newdata <- merge(aggregate(steps ~ interval, data, mean), newdata, by = "interval", all.x = TRUE, all.y = TRUE)
newdata <- newdata[order(newdata$date), ]
for(i in 1:nrow(newdata)){
  if(is.na(newdata$steps.y[i])){
    newdata$steps.y[i] <- newdata$steps.x[i]
  }
}
newdata <- newdata[, c(1,3,4)]
colnames(newdata) <- c('interval','steps','date')
```

Histogram for total number of steps taken each day


```r
library(ggplot2)
test <- aggregate(steps ~ date, newdata, sum)
g <- ggplot(data = test, aes(steps))
g <- g + geom_histogram(bins = 20,col = "black", fill = "blue", alpha = 0.7) 
g <- g + labs(x = "Number of steps", y = "Frequency", title = "Number of Steps Taken Each Day")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Mean and Median of the total number of steps taken per day


```r
test <- aggregate(steps ~ date, newdata, sum)
rmean <- mean(test$steps)
rmedian <- median(test$steps)
```

* Mean: 1.0766189\times 10^{4}
* Median: 1.0766189\times 10^{4}

### Are there differences in activity patterns between weekdays and weekends?

Plot showing difference in number of steps averaged across all days between weekdays and weekends


```r
omit[,2] <- as.Date(omit[ ,2])
index <- which(weekdays(omit[,2])=="Sunday" | weekdays(omit[,2])=="Saturday")
omit$weekday <- "weekday"
omit[index, 4] <- "weekend"
omit$weekday <- as.factor(omit$weekday)
rm(index)
test <- aggregate(steps ~ interval + weekday, omit, mean)
g <- ggplot(data = test, aes(interval, steps))
g <- g + geom_line(col = "red") 
g <- g + facet_grid(weekday ~ .)
g <- g + labs(x = "Time Intervals", y = "Average Steps", title = "Difference Between Weekdays and Weekends")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
