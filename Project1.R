#Global settings 
library(ggplot2)
library(stringr)

#Loading original data
unzip("activity.zip")
dat <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))

#generate df without na values
df <- na.omit(dat)
#remove rownames
rownames(df) <- NULL
# aggregate steps as per date to get total number of steps in a day
date_steps_table <- aggregate(steps ~ date, df, sum)

#make histogram
hist(date_steps_table$steps, breaks = "Sturges",col = "skyblue",main="Histogram of total number of steps per day", xlab="Total number of steps in a day")

#Calculate and report the mean and median total number of steps taken per day
mean(date_steps_table$steps)
median(date_steps_table$steps)

# aggregate steps as per date to get total number of steps in a day
steps_avg_table <- aggregate(steps ~ interval, df, mean)

#Create line plot of 5-minute interval 
plot(steps_avg_table,type="l", xlab= "5-minute interval", ylab= "average number of steps", col="green")

#5-minutes interval contains maximum no.steps
max_row_id <- which.max(steps_avg_table$steps)
steps_avg_table[max_row_id,]
#Inputting missing values
df_na <- is.na(dat)
#number of na row.
length(dat[df_na])
#replace the NA by the mean for that 5-minute interval.
dat$steps[is.na(dat$steps)] <- steps_avg_table$steps[is.na(dat$steps)]

# aggregate steps as per date to get total number of steps in a day
date_steps_table_na <- aggregate(steps ~ date, dat, sum)

#make histogram
hist(date_steps_table_na$steps, breaks = "Sturges",col = "green",main="Histogram of total number of steps per day (NA included)", xlab="Total number of steps in a day")

#Calculate and report the mean and median total number of steps taken per day
mean(date_steps_table_na$steps)
median(date_steps_table_na$steps)

#Convert to Class Date
dat$date <- as.Date(dat$date, "%Y-%m-%d")

#Add aditional column day to dat
dat$day <- weekdays(dat$date)

#Add aditional column called day type
dat$day_type <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(dat)){
  if (dat$day[i] == "Saturday" || dat$day[i] == "Sunday"){
    dat$day_type[i] <- "weekend"
  }
}
# Convert day_type to factor 
dat$day_type <-as.factor(dat$day_type)

# aggregate steps as per date to get total number of steps in a day
date_steps_table <- aggregate(steps ~ interval + day_type, dat, mean)

qplot(interval, steps, data=date_steps_table, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="")+facet_wrap(~ day_type, nrow=2, ncol=1) 
