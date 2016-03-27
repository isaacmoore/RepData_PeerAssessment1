# ----- Read the data into R -----
data <- as.data.frame(read.csv("activity.csv", header = TRUE))
steps <- as.numeric(data$steps, na.rm = TRUE)

# ----- Make a histogram and print mean and median of the steps -----
plotHist <- hist(steps)
plotHist # (1)
print(summary(steps)) # (2)

# -----What is the average daily activity pattern? -----
library(ggplot2) # (1) load the ggplot2 library
avg <- aggregate(list(data$steps), list(data$interval), FUN = mean, na.rm = TRUE) #set the mean for each interval in the dataset and assign it to "avg"
colnames(avg) <- c("interval", "steps") # reset the column names
plot <- ggplot(avg, aes(x = interval, y = steps)) # set the data for the plot using ggplot to the variable "plot"
plot + geom_step(direction = "hv") + xlab("Interval") + ylab("Average Steps") + ggtitle("Average Steps at each interval")

avg[which.max(avg$steps),] # (2)

# ----- Imputting missing values -----
stepsNa <- mean(is.na(data$steps)) # (1) percentage of missing(NA) values in the dataset
print(stepsNa * 100)
# ----- Create dataset with mean steps filled in for NA values -----
data2 <- data # (3)
meanSteps <- as.integer(mean(data$steps, na.rm = TRUE))
data2$steps[which(is.na(data2$steps))] <- meanSteps

# ----- Make a histogram of the steps -----
plot <- ggplot(data2, aes(steps))
plot + geom_histogram() + ggtitle("Histogram of steps - replacing NA's with mean")
# ----- Summary of data2 -----
print(summary(data2$steps))

# ----- Difference between weekdays and weekends -----
data2$date <- as.Date(data2$date, "%Y-%m-%d")
data2$daysWk <- weekdays(data2$date)
data2$daysWk <- ifelse(data2$daysWk == "Saturday" | data2$daysWk == "Sunday", "Weekend", "Weekday")
# ----- Plot the difference between weekdays and Weekends -----
ggplot(data2, aes(interval, steps)) + facet_grid(daysWk ~ .) + geom_line()