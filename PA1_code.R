#Reproducible Reasearch Assignment 1

#The data for this asssigment can be downloaded from the course web site:
#https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

#1) Loading and processing  the data
#unzip the activity data
unzip("activity.zip")
activity<-read.csv("activity.csv")
#processing the data
#turn the date variable into a date type variable 
activity$date<-as.Date(activity$date)

#2) What is mean total number of steps taken per day?

#total of steps taken per day 
totalsteps<-with(activity,tapply(steps,as.factor(date),sum))
# 2.1 Histogram of the total number of steps taken each day
hist(totalsteps, main="Total Number of Steps Taken Each Day",xlab="Total steps",col="blue")
# 2.2 the mean and median total number of steps taken per day
#Mean
mean(totalsteps,na.rm=T)
#Median
median(totalsteps,na.rm=T)

#3) What is the average daily activity pattern?
#time series plot
averageInterval<-aggregate(steps~interval,data=activity,mean)
with(averageInterval,plot(interval,steps,type="l"))
title("Average steps taken across the 5-minutes interval")
#maximum number steps
maxsteps<-averageInterval[averageInterval$steps==max(averageInterval$steps),]


#4) Imputing missing values

#4.1 total number of missing values in the dataset 
sum(is.na(activity))

#4.2 filling missindg data

#4.3 data set without missing values
activity_no_missing<-activity
for(i in 1:length(activity_no_missing[,1])){
    if(is.na(activity_no_missing[i,1])){
        b<-activity_no_missing[activity_no_missing[,3]==activity_no_missing[i,3],]
        activity_no_missing[i,1]<-round(mean(b[,1],na.rm=T))
    }
}

#4.4 Histogram
#total of steps taken per day 
totalsteps2<-with(activity_no_missing,tapply(steps,as.factor(date),sum))
# 2.1 Histogram of the total number of steps taken each day
hist(totalsteps2, main="Total Number of Steps Taken Each Day",xlab="Total steps",col="red")
# 2.2 the mean and median total number of steps taken per day
#Mean
mean(totalsteps2,na.rm=T)
#Median
median(totalsteps2,na.rm=T)

#5) weekends and weekdays
for(i in 1:length(activity_no_missing[,1])){
    if(!(weekdays(activity_no_missing[i,2]) %in% c("sábado","domingo"))){
        activity_no_missing[i,4]<-"weekday"
    }else{
        activity_no_missing[i,4]<-"weekend"
    }
}

activity_no_missing[,4]<-as.factor(activity_no_missing[,4])
colnames(activity_no_missing)[4]<-"weekdayOrWeekend"

averageIntervalWeek<-aggregate(steps~interval+weekdayOrWeekend,data=activity_no_missing,mean)

library(lattice)
with(averageIntervalWeek,xyplot(round(steps)~interval|weekdayOrWeekend,layout=c(1,2),type="l",main="Fig. 4, Activity Patterns Between Weekdays and Weekends"))




