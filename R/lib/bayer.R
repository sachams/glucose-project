###############################################################################
#
# bayer.R
# Loads data from Bayer glocuse meter CSV files
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(zoo)
library(data.table)

LoadBayer <- function(filename) {
  message(paste('Loading file',filename,'...'))

  data <- read.csv(filename)
  time <- as.POSIXct(strptime(paste(data$Date,data$Time),"%m/%d/%Y %H:%M:%S", tz='GMT'))

  data <- data.frame(time=time,
       blood.glucose=data$Blood.Sugar..mmol.L.)
  
  data[order(data$time),]
}

CompareBayerCGM <- function(cgm.data, meter.data, cgmchange.data){
  # Interpolate CGM values at the same points as the meter values
  # so that the values can be compared
  compdata <- approx(x=cgm.data$time, y=cgm.data$blood.glucose, xout=meter.data$time)
  compdata <- data.frame(time=compdata$x, meter.glucose=meter.data$blood.glucose, cgm.glucose=compdata$y)
  
  # Generate a diff
  compdata$diff <- compdata$meter.glucose - compdata$cgm.glucose
  compdata$abs.diff <- abs(compdata$diff)
  
  # Now merge in the times when the sensor was changed, and roll forward each time change
  # We'll then do a diff betrween the time change column and the actual reading time
  
  cgmchange.data.modified <- cgmchange.data[cgmchange.data$change=='change',]
  cgmchange.data.modified$last.change.time <- cgmchange.data.modified$time
  cgmchange.data.modified$change <- NULL
  
  compdata <- merge(compdata, cgmchange.data.modified, by.x='time', by.y='time', all=TRUE)
  
  temp <- na.locf(compdata$last.change.time, fromLast=FALSE)
  compdata$last.change.time <- append(c(rep(as.POSIXlt(NA),length(compdata$last.change.time) - length(temp))), temp)
  
  # Now get rid of rows where there is no time diffference
  compdata$time.since.last.change <- round(as.numeric(compdata$time - compdata$last.change.time)/(60*60*24))
  compdata <- compdata[!is.na(compdata$time.since.last.change),]  
  
  # Bucket the rows into days, and calculate the mean and std deviation
  diff.mean  <- tapply(compdata$diff, compdata$time.since.last.change, function(x) mean(x, na.rm=TRUE))
  diff.stddev  <- tapply(compdata$diff, compdata$time.since.last.change, function(x) sd(x, na.rm=TRUE))
  
  bucketed.comp <- data.frame(time=as.integer(rownames(diff.mean)), diff.mean, diff.stddev)
  
  bucketed.comp
}