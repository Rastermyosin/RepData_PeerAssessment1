# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Load libraries
library(data.table)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
# Load data
data = data.table(read.csv("./data/activity.csv"))
data$date = as.Date(data$date) # Convert date factor to date
```


## What is mean total number of steps taken per day?

```r
# Calculate average steps/activity per day
stepsPerDay = data[, lapply(.SD, sum, na.rm = TRUE), by = date]
stepsPerDay = stepsPerDay[,day:=as.factor(1:61)]

# Calculate the mean and median step count
meanSteps = floor(mean(stepsPerDay$steps, na.rm= TRUE))
medianSteps = floor(median(stepsPerDay$steps, na.rm = TRUE))
```
The mean number of steps taken each day was: 9354.  
The mediam number of steps taken each day was: 1.0395\times 10^{4}.

```r
# Plot histogram
ggplot(stepsPerDay,aes(x=day, y=steps)) + 
  geom_bar(stat = "identity",width=1,colour="black", fill="grey") + 
  scale_x_discrete( breaks=c(1,seq(5,55,by=5), 61)) + 
  theme_bw() +
  xlab("Day") +
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?

```r
# Calculate the average activity per interval
stepsPerPeriod = data[, lapply(.SD, mean, na.rm = TRUE), by = interval]

# Transform interval to string
tmpTime = stepsPerPeriod$interval
tmpTime2 = mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(tmpTime))
tmpTime = paste0(tmpTime2, tmpTime)
tmpTime = format(strptime(tmpTime, format = "%H%M"), format = "%H:%M")

# Determine the interval in which the subject was most active
maxInterval = tmpTime[stepsPerPeriod$steps == max(stepsPerPeriod$steps)]
```
The 5-minute overwhich the most steps are taken, on average, is at 08:35 in the morning.


```r
sizeX = length(stepsPerPeriod$interval)

# plot activity level over time
ggplot(stepsPerPeriod, aes(x=seq(1, sizeX,1), y=steps)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1,sizeX, 24) , labels=tmpTime[seq(1,sizeX,24)]) +
  theme_bw() + 
  xlab("Time") +
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Imputing missing values
For each missing 'steps' value, the average value for the corrosponding interval was inputted into the data.table. The resulting histogram looks different from the original as the missing bars have now appeared. The missing values now force the new mean and median to tend toward the old calculated mean.

```r
# Assign new data table.
data2 = data
numNA = sum(as.numeric(is.na(data$steps))) # Count number of missing values

# For each missing value, fill in the mean for that interval
for (i in 1:length(data2$steps)) {
  if(is.na(data2$steps[i])){
    data2$steps[i] = stepsPerPeriod[interval == data2$interval[i], steps]
  } # END IF
} # END FOR
```
There were 2304 missing values that have been filled in.

```r
# Calculate new mean steps per day
stepsPerDay2 = data2[, lapply(.SD, sum, na.rm = TRUE), by = date]
stepsPerDay2 = stepsPerDay2[,day:=as.factor(1:61)]

meanSteps2 = floor(mean(stepsPerDay2$steps, na.rm= TRUE))

# Plot the new histogram
ggplot(stepsPerDay2,aes(x=day, y=steps)) + 
  geom_bar(stat = "identity",width=1,colour="black", fill="grey") + 
  scale_x_discrete( breaks=c(1,seq(5,55,by=5), 61)) + 
  theme_bw() +
  xlab("Day") +
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


## Are there differences in activity patterns between weekdays and weekends?
It appears that there are activity  differences during the weekends when compated to the weekday. The subject seems to be sleeping in and maintains a relativly constant activity level throughut the rest of the day. This is dffernent from the weekday plot as the subject seems to be quite active in the early parts of the day, but then has a lower activity level for the rest of the day. These plots need signifigance calculations to see if these observations are indeed in the data.

```r
# Convert dates to weekdays and convert weekdays to the weekday/weekend factor
tmpDays = weekdays(data$date)
weekFactor = factor(as.numeric(tmpDays == "Saturday" | tmpDays == "Sunday") + 1,
                    labels = c("Weekday", "Weekend"))

# Assign new column
data = data[, weekFac:=weekFactor]

# calculate new average activity for each interval
stepsPerWeekday = data[, lapply(.SD, mean, na.rm = TRUE), by =c("interval", "weekFac")]

# Create time string from interval
tmpTime = stepsPerWeekday$interval
tmpTime2 = mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(tmpTime))
tmpTime = paste0(tmpTime2, tmpTime)
tmpTime = format(strptime(tmpTime, format = "%H%M"), format = "%H:%M")

sizeX = 288

# Plot activity levels in two plot panels
ggplot(stepsPerWeekday, aes(x=c(seq(1, sizeX,1), seq(1, sizeX,1)), y=steps)) + 
  geom_line() + 
  facet_grid(weekFac ~ .) + 
  scale_x_continuous(breaks = seq(1,sizeX, 24) , labels=tmpTime[seq(1,sizeX,24)]) +
  theme_bw() + 
  xlab("Time") +
  ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
