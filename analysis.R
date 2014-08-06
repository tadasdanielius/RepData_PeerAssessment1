rm(list = ls(all = TRUE));
library(ggplot2);

# Configuration

config.activity.zip <- "activity.zip";
config.activity.csv <- "activity.csv";
config.figures.to_file <- T;

setwd ("~/studies/Reproducible Research/RepData_PeerAssessment1/");

# Helper functions

create_figure <- function(fun, to_file = config.figures.to_file, filename = "figure.png") {
  if (config.figures.to_file) {
    png(filename = filename, width = 480, height = 480, units = "px");
    fun();
    dev.off();
  } else {
    fun();
  }
}

# Research

# -- Loading and preprocessing the data ---

if (!exists("activity.measured.data")||is.null(activity.measured.data)) {
  conn <- unz(config.activity.zip, config.activity.csv);
  activity.measured.data <- read.csv(conn, na.string="NA");
}


# --- Processing data ---

data <- activity.measured.data;
data$date <- as.Date(data$date,format="%Y-%m-%d");
data.steps_per_day <- aggregate(steps~date, data, sum, na.action = na.omit);
data.steps_per_day$Type <- "Original";

data.steps_per_interval <- aggregate(steps~interval, data, sum, na.action = na.omit);
data.mean.steps_per_interval <- aggregate(steps~interval, data, mean, na.action = na.omit);


# --- Presentation Code ---

# 1. What is mean total number of steps taken per day?

# 1.1 Make a histogram of the total number of steps taken each day
create_figure(function(){
  hist(x=data.steps_per_day$steps, 
       main="Total number of steps taken per day histogram",
       xlab="Steps",
       density = 25,
       breaks = 30,
       col="purple");
}, filename="./figures/figure1.png");

# 1.2 Calculate and report the mean and median total number of steps taken per day

activity.analytics.mean.steps_per_day <- mean(data.steps_per_day$steps);
activity.analytics.median.steps_per_day <- median(data.steps_per_day$steps);

print(paste("Mean of total number of steps taken per day: ",activity.analytics.mean.steps_per_day));
print(paste("Median of total number of steps taken per day: ",activity.analytics.median.steps_per_day));

# 2. What is the average daily activity pattern?

# 2.1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

create_figure(function(){
  plot(data.mean.steps_per_interval$interval, 
       data.mean.steps_per_interval$steps, 
       type="l", 
       main="Average daily activity pattern", 
       xlab="Interval", 
       ylab="Steps",
       col="blue")
}, filename="./figures/figure2.png");

# 2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

index <- which.max(data.mean.steps_per_interval[,2]);
activity.analytics.max.interval <- data.mean.steps_per_interval[index,];
print(paste( "5-minute interval, on average contains the maximum number of steps",activity.analytics.max.interval[1]))

# 3. Imputing missing values

# 3.1 Calculate and report the total number of missing values in the dataset

missing_values_count <- dim(data[is.na(data$steps),])[1];
activity.analytics.missing.count <- missing_values_count;

print(paste ("Missing values: ",activity.analytics.missing.count))

# 3.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
#     For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

replace.missing.value <- function(steps, interval) {
  if (!is.na(steps)){
    new.value <- steps;
  }
  else {
    new.value <- data.mean.steps_per_interval[data.mean.steps_per_interval$interval == interval, 2];
  }
  
  new.value
}
data.replaced <- data;
data.replaced$steps <- mapply(replace.missing.value,data$steps,data$interval);


data.replaced.steps_per_day <- aggregate(steps~date, data.replaced, sum);



# 3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps 
# taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
create_figure(function(){
  hist(x=data.replaced.steps_per_day$steps, 
       main="Total steps taken per day After Imputation",
       xlab="Steps",
       density = 25,
       breaks = 30,
       col="purple");
}, filename="./figures/figure3_1.png");

data.replaced.steps_per_day$Type <- "After Imputation";



create_figure(function(){
  
  merged <- rbind(data.replaced.steps_per_day,data.steps_per_day)
  g<-ggplot(merged, aes(steps, fill = Type)) + geom_density(alpha = 0.1,na.rm=T);
  print(g);
  }, filename="./figures/figure3_2.png");

activity.analytics.mean.replaced.steps_per_day <- mean(data.replaced.steps_per_day$steps);
activity.analytics.median.replaced.steps_per_day <- median(data.replaced.steps_per_day$steps);

print(paste("Mean of total number of steps taken per day (after Imputation): ",activity.analytics.mean.replaced.steps_per_day));
print(paste("Median of total number of steps taken per day (after Imputation): ",activity.analytics.median.replaced.steps_per_day));

# 4. Are there differences in activity patterns between weekdays and weekends?
# 4.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#     indicating whether a given date is a weekday or weekend day.

data.replaced$weekday <- weekdays(data$date);
#data$weekday_weekend <- "weekday";

create.day.factor <- function(day) {
  day.factor <- "workday";
  if (day %in% c("Sunday","Saturday")) 
    day.factor <- "weekend";
  factor(day.factor);
}

data.replaced$day <- mapply(create.day.factor,data.replaced$weekday);
data.replaced.avarage.day <- aggregate(steps ~ interval + day, data.replaced, mean);

create_figure(function(){
  g<-ggplot(data.replaced.avarage.day, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day ~ .) + 
    xlab("Interval") + 
    ylab("Number of steps");
  print(g);
}, filename="./figures/figure4.png");
