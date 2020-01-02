###############################################
# R_scripts.R
# DESCRIPTION: script for Reproducible Research
#              Course Project1
###############################################
library(data.table)
library(dplyr)
library(ggplot2)
#Loading and processing data
unzip("activity.zip")
activity <- read.csv("activity.csv",header=TRUE,na.strings = "NA")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
activity <- transform(activity,interval=factor(interval))
activity <- as.data.table(activity)
#What is mean total number of steps taken per day?
#1.barplot1
activity_day <- group_by(activity,date)
sum_step_day <- summarize(activity_day,sum=sum(steps,na.rm=TRUE))
png("./figure/barplot1.png")
ggplot(sum_step_day,aes(date,sum))+geom_bar(stat="identity")+theme_bw()+labs(x="Date",y="Total number of steps",title="Barplot of the total number of steps taken each day")
dev.off()
#2.histogram1
png("./figure/histogram1.png")
ggplot(sum_step_day,aes(sum))+geom_histogram(color="pink",fill="pink",bins=30)+theme_bw()+labs(x="Total number of steps taken per day",y="Frequency",title="Histogram of the total number of steps taken each day")
dev.off()
#3.mean and median
mean <- mean(sum_step_day$sum)
median <- median(sum_step_day$sum)
#What is the average daily activity pattern?
#1.plot1
activity_interval <- group_by(activity,interval)
ave_step_interval <- summarize(activity_interval,average=mean(steps,na.rm=TRUE))
png("./figure/plot1.png")
ggplot(ave_step_interval,aes(x=as.integer(interval),y=average))+geom_line(color="red")+labs(x="Time interval",y="Average number of steps")+scale_x_continuous(breaks=seq(1,288,24),labels=ave_step_interval$interval[seq(1,288,24)])
dev.off()
#2.maximum
max_step_interval <- filter(ave_step_interval,average==max(average))
#Imputing missing values
#1.total missing data
sum_na <- sum(is.na(activity$steps))
#2.impute missing values based on average number of steps in particular 5-minutes interval
#3.impute missing values
activity_without_na <- activity
activity_without_na$steps <- as.numeric(activity_without_na$steps)
for(i in 1:nrow(activity)){
  if(is.na(activity[i,"steps"])==TRUE){
    activity_without_na[i,"steps"] <- filter(ave_step_interval,interval==activity[i,"interval"][[1]]) %>% select(average)
  }
}
#4.histogram2, mean, median
activityNoNA_day <- group_by(activity_without_na,date)
sum_stepNoNA_day <- summarize(activityNoNA_day,sum=sum(steps))
png("./figure/histogram2_noNA.png")
ggplot(sum_stepNoNA_day,aes(sum))+geom_histogram(color="pink",fill="pink",bins=30)+theme_bw()+labs(x="Total number of steps taken per day",y="Frequency",title="Histogram of the total number of steps taken each day (NA imputed)")
dev.off()
mean_NoNA <- mean(sum_stepNoNA_day$sum)
median_NoNA <- median(sum_stepNoNA_day$sum)
#Are there differences in activity patterns between weekdays and weekends?
#1.weekday/weekend
is_weekday <- function(x){
  if(weekdays(x) %in% c("Saturday","Sunday")){
    result <- "weekend"
  }else{
    result <- "weekday"
  }
}
activity_without_na <- mutate(activity_without_na,weekdays=sapply(date,is_weekday))
#2.plot2
activity_without_na <- transform(activity_without_na,weekdays=factor(weekdays))
activityNoNA_interval <- group_by(activity_without_na,interval,weekdays)
ave_stepNoNA_interval <- summarize(activityNoNA_interval,average=mean(steps))
png("./figure/plot2.png")
ggplot(ave_stepNoNA_interval,aes(x=as.integer(interval),y=average,color=weekdays))+geom_line()+facet_grid(weekdays~.)+labs(x="Time interval",y="Average number of steps")+scale_x_continuous(breaks=seq(1,288,48),labels=ave_step_interval$interval[seq(1,288,48)])
dev.off()
