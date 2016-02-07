# Reproducible Research: Peer Assessment 1


#setwd("C:/Users/Koen/Documents/GitHub/RepData_PeerAssessment1")
library(lattice)

## Loading and preprocessing the data

#Unzip if necessary
if (!file.exists("activity.csv"))
{
unzip("activity.zip")
}

#load the data
activity<-read.csv("activity.csv", head=TRUE)

#little bit of prepping
activity.prepped<-activity
activity.prepped$date<-as.Date(activity.prepped$date)
activity.prepped<-subset(activity.prepped, !is.na(activity.prepped$steps))

#sum steps by day
activity.prepped.sum.by.date <- aggregate(list(steps = activity.prepped$steps), by=list(date=activity.prepped$date), FUN=sum)
#activity_prepped.sum.by.date

##What is mean total number of steps taken per day?

#create histogram
hist(x=activity.prepped.sum.by.date$steps ,col="red",breaks=25,xlab="Daily steps",
     ylab="Frequency",
     main="The distribution of daily steps")

#Calculate mean and median
mean_steps<-as.integer(mean(activity.prepped.sum.by.date$steps))
median_steps<-as.integer(median(activity.prepped.sum.by.date$steps))

##What is the average daily activity pattern?
#mean steps by interval
activity.prepped.sum.by.interval <- aggregate(list(steps = activity.prepped$steps), by=list(interval=activity.prepped$interval), FUN=mean)
#create the plot 
plot(activity.prepped.sum.by.interval$interval, activity.prepped.sum.by.interval$steps, type="l", xlab="5' intervals", ylab="Average steps")
# which interval has max number of steps
max_steps<-max(activity.prepped.sum.by.interval$steps)
max_interval <- activity.prepped.sum.by.interval[activity.prepped.sum.by.interval$steps==max_steps,]

##Imputing missing values
Number_of_NAs <- nrow(subset(activity, is.na(activity$steps)))
activity.impute <- activity
activity.impute.na <- is.na(activity.impute$steps)
activity.impute.sum.by.interval <- tapply(activity.prepped$steps, activity.prepped$interval, mean, na.rm=TRUE, simplify=T)
activity.impute$steps[activity.impute.na]<-activity.impute.sum.by.interval[as.character(activity.impute$interval[activity.impute.na])]

#create histogram
activity.impute.sum.by.date <- aggregate(list(steps = activity.impute$steps), by=list(date=activity.impute$date), FUN=sum)

hist(x=activity.impute.sum.by.date$steps ,col="red",breaks=25,xlab="Daily steps",
     ylab="Frequency",
     main="The distribution of daily steps")

#Calculate mean and median
new_mean_steps<-as.integer(mean(activity.impute.sum.by.date$steps))
new_median_steps<-as.integer(median(activity.impute.sum.by.date$steps))

##Are there differences in activity patterns between weekdays and weekends?

#add variable day and weekendindicator 
Sys.setlocale("LC_TIME", "English")
activity.impute$weekday<-weekdays(as.Date(activity.impute$date))
activity.impute$weekendind<-ifelse(activity.impute$weekday=="Saturday" |activity.impute$weekday=="Sunday", "Weekend", "Weekday") 

#mean steps by weekendind/interval
activity.impute.wk <- aggregate(steps ~ weekendind+interval, data=activity.impute, FUN=mean)

#create the plot 

xyplot(steps ~ interval | factor(weekendind),
       layout = c(1, 2),
       xlab="Interval",
       ylab="#steps",
       type="l",
       lty=1,
       data=activity.impute.wk)

