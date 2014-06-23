###############################################################################
#
# trimp.R
# Calculates TRIMP for Strava data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(lubridate)

CalculateTrimpData <- function(strava.data, hr.rest, hr.max) {  
  # Adds trimp.exp and trimp.duration columns to Strava data as returned from GetStravaActivities.
  # trimp.exp is described here: http://fellrnr.com/wiki/TRIMP#TRIMPexp_Exponental_Heart_Rate_Scaling
  # trimp.duration is described here: http://fellrnr.com/wiki/TRIMP#Training_Volume
  #
  # Args:
  #  strava.data: Data as returned from GetStravaActivities
  #  hr.rest: Resting heart rate
  #  hr.max: Maximum heart rate
  #  
  # Returns:
  #  Returns the input data with trimp.exp and trimp.duration columns added. Note that if no heart rate
  #  data is present in an input data frame, trimp.exp will be NA
  #
  
  # Add trimp data
  sapply(strava.data, function(x) {AddTrimpToDataFrame(x, hr.rest, hr.max)})
}

RebucketTrimpData <- function(trimp.data, bucket.width) {  
  # Rebuckets the input data into periods specified by bucket.width
  #
  # Args:
  #  trimp.data: a list of strava data frames
  #  bucket.width: width of the bucket in seconds
  #
  # Returns:
  #  Returns the input data rebucketed into buckets of width bucket.width
  #
  
  # Rebucket the data
  lapply(trimp.data, function(x) {RebucketDataFrame(x, bucket.width)})
}

CollateTrimpData <- function(trimp.data, bucket.width, hr.rest, hr.max) {  
  # Joins together a list of TRIMP data frames into one data frame. 
  # A single entry is added after each activity at resting heart rate.
  #
  # Args:
  #  trimp.data: a list of strava data frames
  #  bucket.width: width of the bucket in seconds
  #  hr.rest: resting heart rate
  #  hr.max: maximum heart rate
  #
  # Returns:
  #  Returns the input data merged into one big data frame
  #
  modified.data <- lapply(trimp.data, function(x) { 
                          dummy.record <- data.frame(time=(x$time[nrow(x)] + bucket.width),
                                                     trimp.duration=0,
                                                     trimp.exp=CalculateTrimpExp(bucket.width, hr.rest, hr.rest, hr.max))
                          rbind(x, dummy.record)})
  
  do.call('rbind', modified.data)
}

RebucketDataFrame <- function(trimp.data.frame, bucket.width) {  
  # Rebuckets the input data frame into periods specified by bucket.width
  #
  # Args:
  #  strava.data: a single data frame
  #  bucket.width: width of the bucket in seconds
  #
  # Returns:
  #  Returns the input data rebucketed into buckets of width bucket.width.
  #
  
  # Generate the buckets we want to resample into
  buckets <- seq(from=trimp.data.frame[1,'abs.time'], 
           to=(trimp.data.frame[nrow(trimp.data.frame),'abs.time'] + bucket.width), 
           by=bucket.width)
  
  # Cut the data into these buckets
  bucketed.time <- cut(trimp.data.frame$abs.time, buckets)
  
  # Now sum up trimp data by these buckets
  bucketed.trimp.exp <- tapply(trimp.data.frame$trimp.exp, bucketed.time, sum)
  bucketed.trimp.duration <- tapply(trimp.data.frame$trimp.duration, bucketed.time, sum)

  # Extract the actual times used - these are in fact the row names returned from tapply. Convert them to POSIX on the way.
  time.rows <- strptime(rownames(bucketed.trimp.duration),"%Y-%m-%d %H:%M:%S", tz='GMT')
  
  # And return a data frame
  data.frame(time=time.rows,
         trimp.duration=bucketed.trimp.duration,
         trimp.exp=bucketed.trimp.exp,
         row.names=NULL)
}

#
# Utility functions
#

AddTrimpToDataFrame <- function(strava.data.frame, hr.rest, hr.max) {
  # Calculates trimp.exp and trimp duration for a single activity data frame and adds it to the data frame.
  # If heart rate data is not available, trimp.exp will be NA.
  #
  # Args:
  #  strava.data.frame: single data.frame of activity data. Expected columns:
  #            time
  #            heartrate (optional)
  #            abs.time
  #  hr.rest: resting heart rate
  #  hr.max: maximum heart rate
  #
  # Returns:
  #  Input data with two new columns: trimp.exp and trimp.duration
  #
  
  # message(paste("Processing activity from date/time", strava.data.frame[1,'abs.time']))
  
  # Generate a vector of durations (ie, differences between one time and the next). have to add a zero at the start to make the 
  # vectors the same length
  strava.data.frame$trimp.duration <- append(c(0), diff(strava.data.frame$time))
  
  # If there is no heartrate column, then don't calculate trimp.exp and instead add a columnn of NAs
  if(is.null(strava.data.frame$heartrate)){
    strava.data.frame$trimp.exp <- rep(NA, nrow(strava.data.frame))
  } else {
    strava.data.frame$trimp.exp <- CalculateTrimpExp(strava.data.frame$trimp.duration, 
                           strava.data.frame$heartrate, 
                           hr.rest,
                           hr.max)
  }
  strava.data.frame
}

CalculateTrimpExp <- function(duration, hr, hr.rest, hr.max) {
  # Calculates exponential TRIMP for a single HR reading
  #
  # Args:
  #  duration: duration of the activity in seconds
  #  hr: heart rate
  #  hr.rest: resting heart rate
  #  hr.max: maximum heart rate
  #
  # Returns:
  #  Exponential training impulse as defined here: http://fellrnr.com/wiki/TRIMP#TRIMPexp_Exponental_Heart_Rate_Scaling
  #
  
  hr.reserve <- (hr - hr.rest)/(hr.max - hr.rest) 
  trimp.exp <- (duration/60) * hr.reserve * 0.64 * exp(1.92 * hr.reserve)
  trimp.exp
}