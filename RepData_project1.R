#load zip files
zipfile <- "activity.zip"
rawdata <- read.csv(unz(zipfile, filename="activity.csv"), 
                    colClasses=c(date="Date"))

#process 1 : sum total steps taken each day
totalsum <- with(rawdata[!is.na(rawdata$steps),], 
                 aggregate(steps, by=list(date), FUN="sum"))
names(totalsum) <- c("date", "steps")
#draw histogram plot
library(ggplot2)
g <- ggplot(totalsum, aes(x=steps))
g <- g + geom_histogram(fill = "coral")
g <- g + labs(title="Distribution of the total number of steps taken each day")
g <- g + labs(x="Steps taken each day", y="Days")
g <- g + scale_y_continuous(limits=c(0,10), breaks=seq(0, 10, by=2))
print(g)
#calculate mean and median
cat("The mean total number of steps taken per day is : ", mean(totalsum$steps))
cat("The median total number of steps taken per day is : ", 
    median(totalsum$steps))

#process 2 : average steps taken each interval
intervalaverage <- with(rawdata[!is.na(rawdata$steps),], 
                        aggregate(steps, by=list(interval), FUN="mean"))
names(intervalaverage) <- c("interval", "avgsteps")
#draw time series plot
g <- ggplot(intervalaverage, aes(x=interval, y=avgsteps))
g <- g + geom_line(colour="blue")
g <- g + labs(title="Average steps taken through intervals")
g <- g + labs(x="Interval", y="Number of steps")
print(g)
#calculate max value
intervalmax <- 
        intervalaverage[intervalaverage$avgsteps==max(intervalaverage$avgsteps),]
cat("The", intervalmax$interval, "5-minute interval has the maximum step : ", 
    intervalmax$avgsteps)

#process 3 : Imputing missing values
#Calculate and report the total number of missing values in the dataset
naCount <- nrow(rawdata[is.na(rawdata$steps),])
cat("The total number of rows with NAs in the dataset is : ", naCount)
#Fill in all of the missing values in the dataset and create a new dataset
tmp <- merge(rawdata, intervalaverage, by="interval", all.x=T)
tmp$steps[is.na(tmp$steps)] <- tmp$avgsteps[is.na(tmp$steps)]
newData <- tmp[, c("date", "interval", "steps")]
#do process 1 again with new dataset
totalsum2 <- with(newData, aggregate(steps, by=list(date), FUN="sum"))
names(totalsum2) <- c("date", "steps")
contrast <- rbind(cbind(totalsum, flag="before"), cbind(totalsum2, flag="after"))
#draw histogram plot
g <- ggplot(contrast, aes(x=steps))
g <- g + geom_histogram(fill = "coral") + facet_grid(. ~ flag)
g <- g + labs(title="Distribution of the total number of steps taken each day")
g <- g + labs(x="Steps taken each day", y="Days")
g <- g + scale_y_continuous(limits=c(0,12), breaks=seq(0, 12, by=2))
print(g)
#calculate mean and median
cat("The mean total number of steps taken per day: ", "\nbefore: ", 
    mean(totalsum$steps), "\nafter: ", mean(totalsum2$steps))
cat("The median total number of steps taken per day: ", "\nbefore: ", 
    median(totalsum$steps), "\nafter: ", median(totalsum2$steps))

#process 4 : Differences in activity patterns between weekdays and weekends
#create a new variable to indicate 'weekend' and 'weekday'
tmp <- weekdays(newData$date)
weekday <- sapply(tmp, function(x) {
        if(x %in% c("Saturday", "Sunday")) return("weekend")
        else return("weekday")
})
newData2 <- cbind(newData, weekday)
#average steps taken each interval across weekday and weekend
intervalaverage2 <- 
        with(newData2, aggregate(steps, by=list(weekday, interval), FUN="mean"))
names(intervalaverage2) <- c("weekday", "interval", "steps")
#draw time series plot
g <- ggplot(intervalaverage2, aes(x=interval, y=steps))
g <- g + geom_line(colour="blue") + facet_grid(weekday ~ .)
g <- g + labs(title="Average steps taken through intervals")
g <- g + labs(x="Interval", y="Number of steps")
print(g)