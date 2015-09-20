#Reading data
filename <- "activity.zip"
data <- read.csv (unzip (filename))
#data$date <- strptime (data$date, format = "%Y-%m-%d")


# Calculate the total number of steps taken per day
steps_per_day <- summarize (group_by (data, date), n_steps = sum (steps))
# steps_per_day$date <- strptime (steps_per_day$date, format = "%Y-%m-%d")

# Make a histogram of the total number of steps taken each day
hist (steps_per_day$n_steps,
      breaks = 22,
      xlab = "Number of Steps", 
      main = "Total number of steps taken each day")

# Calculate and report the mean and median of the total number of steps taken per day
steps_per_day_mean <- mean (steps_per_day$n_steps, na.rm="true")
steps_per_day_median <- median (steps_per_day$n_steps, na.rm="true")

head (data)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average
# number of steps taken, averaged across all days (y-axis)
steps_avg <- subset (data, na.rm = TRUE)
steps_avg <- summarize (group_by (steps_avg, interval), avg = mean (steps))
str (steps_average)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


library (dplyr)

## Getting data to construct a plot
data_sorted <-
    data %>%
    group_by (date) %>%
    summarise (totals = sum(steps))



steps_per_day_mean <- mean (data_sorted$totals, na.rm="true")
steps_per_day_median <- median (data_sorted$totals, na.rm="true")

data_mean
head (data_sorted)
str (data_sorted)


