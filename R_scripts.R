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
activity_day <- group_by(activity,date)
sum_step_day <- summarize(activity_day,sum=sum(steps,na.rm=TRUE))
mean <- mean(sum_step_day$sum)
median <- median(sum_step_day$sum)
png("./figure/histogram1.png")
ggplot(sum_step_day,aes(sum))+geom_histogram(color="pink",fill="pink",bins=30)+theme_bw()+labs(x="Total number of steps taken per day",y="Frequency",title="Histogram of the total number of steps taken each day")+ geom_vline(xintercept=mean,color="red",linetype="dotted",size=1)+geom_vline(xintercept=median,color="blue",linetype="dotted",size=1)+geom_text(aes(x=mean+100,label="mean",y=10),color="red",angle=90)+geom_text(aes(x=median+100,label="median",y=10),color="blue",angle=90)
dev.off()
#What is the average daily activity pattern?

