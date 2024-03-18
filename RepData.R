library(knitr)

library(ggplot2)
library(lubridate)

getwd()

##Upload the file

activity <- read.csv("D:\\argor\\Documents\\
                     RepData_PeerAssessment\\RepData_PeerAssessment1
                     \\activity\\activity.csv")

##Convert dates and intervals into datetime

activity2 <- sprintf("%04d", activity$interval)
activity3 <- format(strptime(activity2, format="%H%M"), format = "%H:%M")

activity$interval <- activity3


activity$datetime <- paste(activity$date, activity$interval, sep = " ")

stepdata <- activity[, c("steps", "datetime")]

stepdata$datetime <- strptime(stepdata$datetime, format = "%Y-%m-%d %H:%M")

stepdata$date <- as.Date(stepdata$datetime)

stepdata$time <- format(stepdata$datetime, "%H:%M:%S")

steps <- stepdata[, c("steps", "date", "time")]

 ##Plotting a histogram of processed data

library(ggplot2)
library(scales)

ggplot(steps, aes(x=date, y=steps)) +
  geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day", labels = date_format("%m-%d"),
               limits = c(as.Date("2012-10-01"), as.Date("2012-11-30"))) +
  xlab("Date") +
  ylab("Number of Steps Taken") + 
  ggtitle("Number of Steps Taken Per Day") +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 30000, 2000)) +
  theme(axis.text.x = element_text(angle = 90)) 

##Calculating Mean and Median number of steps taken each day

library(dplyr)

steps_avg_med = steps %>%
  group_by(date) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE),
            med_steps = median(steps, na.rm = TRUE))

table(steps_avg_med)

##Creating a Time Series Plot

plot(stepdata$datetime, stepdata$steps, xlab="Date and Time",
     ylab="Steps Taken", type="n", main = "Time Series of Steps Taken")
lines(stepdata$datetime, stepdata$steps)

##Five Minute Interval with most average steps

library(dplyr)

steps_avg_time = steps %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

##Strategy for Replacing NA Values with overall mean number of steps

newsteps = steps

for(i in 1:ncol(newsteps)){
  newsteps[is.na(newsteps[,i]), i] <- mean(newsteps[,i], na.rm = TRUE)
}

##Creating Histogram of total steps with NA values replaced

library(ggplot2)

ggplot(newsteps, aes(x=date, y=steps)) +
  geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day", labels = date_format("%m-%d"),
               limits = c(as.Date("2012-10-01"), as.Date("2012-11-30"))) +
  xlab("Date") +
  ylab("Number of Steps Taken") + 
  ggtitle("Number of Steps Taken Per Day no NA Values") +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 30000, 2000)) +
  theme(axis.text.x = element_text(angle = 90)) 

##Panel Plot of 5 minute intervals of weekdays vs weekends

library(ggplot2)
library(gridExtra)

endsteps = steps %>%
  filter(date == "2012-10-06" | date == "2012-10-07" | date == "2012-10-13" 
         | date == "2012-10-14" | date == "2012-10-20" | date == "2012-10-21"
         | date == "2012-10-27" | date == "2012-10-28" | date == "2012-11-03"
         | date == "2012-11-04" | date == "2012-11-10" | date == "2012-11-11"
         | date == "2012-11-17" | date == "2012-11-18" | date == "2012-11-24"
         | date == "2012-11-25") %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

midsteps = steps %>%
  filter(between(date, as.Date("2012-10-01"), as.Date("2012-10-05")) | 
           between(date, as.Date("2012-10-08"), as.Date("2012-10-12")) |
           between(date, as.Date("2012-10-15"), as.Date("2012-10-19")) |
           between(date, as.Date("2012-10-22"), as.Date("2012-10-26")) |
           between(date, as.Date("2012-10-29"), as.Date("2012-11-02")) |
           between(date, as.Date("2012-11-05"), as.Date("2012-11-09")) |
           between(date, as.Date("2012-11-12"), as.Date("2012-11-16")) |
           between(date, as.Date("2012-11-19"), as.Date("2012-11-23")) |
           between(date, as.Date("2012-11-26"), as.Date("2012-11-30"))) %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))



g1 <- ggplot(midsteps, aes(x=time, y=avg_steps, group = 1)) +
  geom_line() +
  geom_point() +
  xlab("Time (5 minute intervals)") +
  ylab("Average Number of Steps Taken") +
  ggtitle("Average Number of Steps Taken by Interval during Weekdays") +
  theme_bw() +
  theme(axis.text.x = element_blank())

g2 <- ggplot(endsteps, aes(x=time, y=avg_steps, group = 1)) +
  geom_line() +
  geom_point() +
  xlab("Time (5 minute intervals)") +
  ylab("Average Number of Steps Taken") +
  ggtitle("Average Number of steps Taken by Interval during Weekends") +
  theme_bw() +
  theme(axis.text.x = element_blank())

grid.arrange(g1, g2, nrow = 1)

