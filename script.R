## Name         : Yash Kumar Singh
## Program Title: Reproducible Research (Temporary)
## ------------------------------------------------------------------

## ------------------------------------------------------------------
## Loading and Preprocessing Data
## ------------------------------------------------------------------

library(ggplot2)
data <- read.csv("~/Reproducible-Research-Assignment-1-Coursera/activity.csv")
omit <- na.omit(data)

## ------------------------------------------------------------------
## What is mean total number of steps taken per day?
## ------------------------------------------------------------------

test <- aggregate(steps ~ date, omit, sum)
png(filename = "~/Reproducible-Research-Assignment-1-Coursera/histogram1.png", width = 480, height = 480, units = "px", bg = "white")
test <- aggregate(steps ~ date, omit, sum)
g <- ggplot(data = test, aes(steps))
g <- g + geom_histogram(bins = 20,col = "black", fill = "blue", alpha = 0.7) 
g <- g + labs(x = "Number of steps", y = "Frequency", title = "Number of Steps Taken Each Day")
print(g)
dev.off()
test <- aggregate(steps ~ date, omit, sum)
rmean <- mean(test$steps)
rmedian <- median(test$steps)

## ------------------------------------------------------------------
## What is the average daily activity pattern?
## ------------------------------------------------------------------
test <- aggregate(steps ~ interval, omit, mean)
png(filename = "~/Reproducible-Research-Assignment-1-Coursera/lineplot1.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(data = test, aes(test$interval, test$steps)) 
g <- g + geom_line(col = "red")
g <- g + labs(x = "Time Intervals", y = "Average Steps", title = "Average Number Of Steps Taken Across All Days")
print(g)
dev.off()
length(which(is.na(data$steps)))

## ------------------------------------------------------------------
## Imputing missing values
## ------------------------------------------------------------------

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
test <- aggregate(steps ~ date, newdata, sum)
png(filename = "~/Reproducible-Research-Assignment-1-Coursera/histogram2.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(data = test, aes(steps))
g <- g + geom_histogram(bins = 20,col = "black", fill = "blue", alpha = 0.7) 
g <- g + labs(x = "Number of steps", y = "Frequency", title = "Number of Steps Taken Each Day")
print(g)
dev.off()
test <- aggregate(steps ~ date, newdata, sum)
rmean <- mean(test$steps)
rmedian <- median(test$steps)

## ------------------------------------------------------------------
### Are there differences in activity patterns between weekdays and weekends?
## ------------------------------------------------------------------

omit[,2] <- as.Date(omit[ ,2])
index <- which(weekdays(omit[,2])=="Sunday" | weekdays(omit[,2])=="Saturday")
omit$weekday <- "weekday"
omit[index, 4] <- "weekend"
omit$weekday <- as.factor(omit$weekday)
rm(index)
test <- aggregate(steps ~ interval + weekday, omit, mean)
png(filename = "~/Reproducible-Research-Assignment-1-Coursera/lineplot2.png", width = 480, height = 480, units = "px", bg = "white")
g <- ggplot(data = test, aes(interval, steps))
g <- g + geom_line(col = "red") 
g <- g + facet_grid(weekday ~ .)
g <- g + labs(x = "Time Intervals", y = "Average Steps", title = "Difference Between Weekdays and Weekends")
print(g)
dev.off()
rm(rmedian, rmean, i, g, test)

## ------------------------------------------------------------------