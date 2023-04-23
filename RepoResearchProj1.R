#load needed libraries
library(ggplot2)

#Suppress timezone warnings that happen with date manipulations
defaultW <- getOption("warn") 
options(warn = -1) 


#download and read data file once to save time
data_file <- "repdata_data_activity.zip"

if (!file.exists(data_file)) {
  download_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(download_url, destfile = data_file)
  unzip (zipfile = data_file)
}

if (!exists("activity")) {
  activity <- read.csv("activity.csv") 
#Add day of week
  activity$date <- as.POSIXct(activity$date,"%m%d%Y")
  day <- weekdays(activity$date)
  activity <- cbind(activity, day)
#Add weekdays or weekends
  activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
  activity$daytype <- sapply(activity$date, function(x) {
    if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
    {y <- "Weekend"}
    else {y <- "Weekday"}
    y
  })
}

#enable warnings again
options(warn = defaultW)

#Question 1

#Calculate total steps on a day
activitytotal <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
#Add names
names(activitytotal) <- c("Date", "Steps")

#Get Mean and Median
mean(activitytotal$Steps)
median(activitytotal$Steps)

#Convert data set to data frame
totaldf <- data.frame(activitytotal)

#Plot histogram using ggplot2
Q1plot <- ggplot(totaldf, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "blue", col = "black") + 
  xlab("Total Steps Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Steps Taken on a Day")

print(Q1plot)

#Question 2
#Calculate average steps across all days by 5-min intervals
dailyaverage <- aggregate(activity$steps, by = list(activity$interval), 
                                  FUN = mean, na.rm = TRUE)
#Add names
names(dailyaverage) <- c("Interval", "Mean")

#Max interval
dailyaverage[which.max(dailyaverage$Mean), ]$Interval

#Convert to a dataframe
averagedf <- data.frame(dailyaverage)

#Plot time series
Q2Plot <- ggplot(averagedf, mapping = aes(Interval, Mean)) + 
  geom_line(col = "blue") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval")

print(Q2Plot)

#Question3
missingsum<-sum(is.na(activity$steps))

missingindex<-is.na(activity[,1])

dm<-mean(dailyaverage$Mean)

activity1<-activity
activity1[missingindex,1]<-dm

#Calculate total steps on a day
activitytotal1 <- with(activity1, aggregate(steps, by = list(date), sum, na.rm = TRUE))
#Add names
names(activitytotal1) <- c("Date", "Steps")

#Get Mean and Median
mean(activitytotal1$Steps)
median(activitytotal1$Steps)

#Convert data set to data frame
totaldf1 <- data.frame(activitytotal1)

#Plot histogram using ggplot2
Q3plot <- ggplot(totaldf1, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "blue", col = "black") + 
  xlab("Total Steps Per Day with Imputed Data") + 
  ylab("Frequency") + 
  ggtitle("Total Steps Taken on a Day with Imputed Data")

print(Q3plot)


#Question4

#Create data set to plot
activitybyday <-  aggregate(steps ~ interval + daytype, activity, mean, na.rm = TRUE)

#Plot weekday vs weekend activity
Q4Plot <-  ggplot(activitybyday, 
  aes(x = interval, 
    y = steps, 
    color = daytype)) + 
    geom_line() + 
    ggtitle("Average Daily Steps Weekday vs Weekend") + 
    xlab("Interval") + 
    ylab("Average Number of Steps") +
    facet_wrap(~daytype, ncol = 1, nrow=2) +
    scale_color_discrete(name = "Weekend vs Weekday")

print(Q4Plot)










