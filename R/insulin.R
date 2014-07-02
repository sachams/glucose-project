###############################################################################
#
# insulin.R
# Processes insulin data, such as comparing basal profiles to a baseline profile
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(zoo)

InsulinCompareBasalToBaseline <- function(actual.basal, baseline.filename, filter.period=300)
{
  # Load in the basal baseline profile from file
  basal.profile <- LoadBasalProfile(baseline.filename)
  
  # Use this to create a vector of basal rates across the same days as the actual baseline data
  start.time <- head(actual.basal$time, 1)
  end.time <- tail(actual.basal$time, 1)
  baseline.basal <- InsulinGenerateBasalBaseline(start.time, end.time, basal.profile)
  
  # Now calculate a diff between the actual and the baseline data
  basal <- InsulinGetBasalDiff(actual.basal, baseline.basal)
  
  # For some reason if the basal profile says change at 14:00, the profile actually changes at 14:03. 
  # So run a filter to remove any differences of less than x minutes
  basal$duration <- append(diff(basal$time), c(0))  
  basal <- basal[basal$duration > filter.period,]

  # Also remove rows where basal is zero. This might be during a pump suspend for uploading
  basal <- basal[basal$basal > 0,]
  
  # Rename some columns to make it clearer
  names(basal)[names(basal)=='basal'] <- 'actual'
  names(basal)[names(basal)=='baseline.basal'] <- 'baseline'  
  
  basal
}

PlotBasalData <- function(basal.data, use.facets=TRUE, start.date=NULL, end.date=NULL){
  
  # If the start or end date is not null, trim the data accordingly
  if(is.null(start.date)) {
    start.date <- head(basal.data$time,1)
  } else {
    start.date <- as.POSIXct(strptime(start.date, '%Y-%m-%d', tz='GMT'))
    basal.data <- basal.data[basal.data$time >= start.date,]
  }
  
  if(is.null(end.date)) {
    end.date <- tail(basal.data$time, 1)
  } else {
    end.date <- as.POSIXct(strptime(end.date, '%Y-%m-%d', tz='GMT'))
    basal.data <- basal.data[basal.data$time <= end.date,]
  }
  
#    p1 <- ggplot(merged.data, aes(x=time, y=trimp.duration)) + 
#      geom_bar(stat='identity', colour='black') +
#      scale_y_continuous(limits=c(0,500)) +
#      scale_x_datetime(limits=c(start.date, end.date))
    
    p2 <- ggplot(basal.data, aes(x=time, y=difference)) + 
      geom_step() +
      scale_x_datetime(limits=c(start.date, end.date))
    
#    grid.arrange(p1,p2)  
  p2
}

InsulinLoadBasalProfile <- function(filename) {
  # The columns are basal, hour, min
  basal.profile <- read.csv(filename)
  
  # Add a new column which is time in seconds and return re-ordered data frame
  basal.profile$time <- basal.profile$hour*60*60 + basal.profile$min*60
  basal.profile[,c('time', 'hour', 'min', 'basal')]
}

InsulinGenerateBasalBaseline <- function(start.time, end.time, basal.profile) {
  
  # First, generate a vector of dates which run between the start and end times (don't go past the end though)
  dates <- seq(round(start.time, 'days'), round(end.time, 'days') - 24*60*60, by='1 day')
  
  # Reproduce the basal and time pattern for each day
  times <- rep(basal.profile$time, times=length(dates))
  basals <- rep(basal.profile$basal, times=length(dates))
  
  # Now replicate each date the same number of times as elements in the basal profile
  dates <- rep(dates, each=nrow(basal.profile))
  
  # Add the dates and times together to get a full POSIX date/time
  data.frame(time=(dates + times), baseline.basal=basals) 
}

InsulinGetBasalDiff <- function(actual.basal, baseline.basal) {
  # Convert times to POSIXct so we can merge
  actual.basal$time <- as.POSIXct(actual.basal$time)
  baseline.basal$time <- as.POSIXct(baseline.basal$time)
  
  # Do the merge
  merged.basal <- merge(actual.basal, baseline.basal, by.x='time', by.y='time', all=TRUE)
  
  # Where we now have NA rows, extrapolate from the previous value. This doesn't work if there is a NA in the first row, as the resulting 
  # vector is too short. So create a temp vector, and paste it back in with NAs at the start of the new basals.
  temp <- na.locf(merged.basal$basal, fromLast=FALSE)
  merged.basal$basal <- append(c(rep(NA,length(merged.basal$basal) - length(temp))), temp)
  
  temp <- na.locf(merged.basal$baseline.basal, fromLast=FALSE)
  merged.basal$baseline.basal <- append(c(rep(NA,length(merged.basal$baseline.basal) - length(temp))), temp)
  
  # Now add the diff column
  merged.basal$difference <- merged.basal$basal - merged.basal$baseline.basal
  
  merged.basal
}


CalculateBasalDiff <- function(actual, baseline) {
  i.actual <- 1
  i.baseline <- 1
  i.out <- 1
  
  diff.time <- append(actual$time, baseline$time)
  diff.time <- diff.time[!duplicated(diff.time)]
  
  out <- data.frame(time=diff.time, basal=rep(NA, length(diff.time)))
  
  previous.actual <- NA
  previous.baseline <- NA
  
  while(i.actual <= nrow(actual) || i.baseline <= nrow(baseline)) {
    if(i.baseline > nrow(baseline)) {
      out$time[out$time==actual$time[i.actual], 'basal'] = previous.baseline - actual$basal[i.actual]
      
      previous.actual <- actual$basal[i.actual]
      i.actual <- i.actual + 1
    } else if(i.actual > nrow(actual)) {
      out$time[out$time==baseline$time[i.baseline], 'basal'] = baseline$basal[i.baseline] - previous.actual
      
      previous.baseline <- baseline$basal[i.baseline]
      i.baseline <- i.baseline + 1      
    } else if(actual$time[i.actual] <= baseline$time[i.baseline]) {
      out$time[out$time==actual$time[i.actual], 'basal'] = previous.baseline - actual$basal[i.actual]
      
      previous.actual <- actual$basal[i.actual]
      i.actual <- i.actual + 1
    } else if(actual$time[i.actual] > baseline$time[i.baseline]) {
      out$time[out$time==baseline$time[i.baseline], 'basal'] = baseline$basal[i.baseline] - previous.actual
      
      previous.baseline <- baseline$basal[i.baseline]
      i.baseline <- i.baseline + 1      
    } else {
      stop("Why are we here?")
    }
  }
  
  out
  
}