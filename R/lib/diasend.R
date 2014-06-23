###############################################################################
#
# trimp.R
# Calculates TRIMP for Strava data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(XLConnect)


ProcessDiasendWorkbooks <- function(input.directory, output.directory) {
  # Converts Diasend .xls files in the given directory into two CSV files, one for cgm and one for insulin data.
  # All .xls files are processed and any overlapping data is removed. The output files are named cgm.csv and insulin.csv.
  #
  # Args:
  #  input.directory: Directory contining input .xls files
  #  output.directory: Directory where the .csv files should be written
  #
  # Returns:
  #  Nothing
  #  
  
  # Get the list of .xls files and process them one by one
  files <- list.files(path=input.directory, pattern="*.xls", full.names=TRUE)
  message(paste('Found',length(files),'files. Loading...'))
  
  data <- lapply(files, ProcessOneWorkbook)
  data
}


ProcessOneWorkbook <- function(filename){
  message(paste('Processing', filename))
  
  # Load in the workbook
  workbook <- loadWorkbook(filename)
  
  # Process CGM data
  cgm.data <- readWorksheet(workbook, sheet='CGM', startRow=2)
  colnames(cgm.data) <- tolower(colnames(cgm.data))
  cgm.data$time <- strptime(cgm.data$time,"%d/%m/%Y %H:%M")
  
  # Process insulin data
  insulin.data <- readWorksheet(workbook, sheet='Insulin use and carbs')
  colnames(insulin.data) <- tolower(colnames(insulin.data))
  insulin.data$time <- strptime(insulin.data$time,"%d/%m/%Y %H:%M")
  
  names(insulin.data)[names(insulin.data)=='basal.amount..u.h.'] <- 'basal'
  names(insulin.data)[names(insulin.data)=='bolus.volume..u.'] <- 'bolus'
  names(insulin.data)[names(insulin.data)=='immediate.volume..u.'] <- 'bolus.immediate'
  names(insulin.data)[names(insulin.data)=='extended.volume..u.'] <- 'bolus.extended'
  names(insulin.data)[names(insulin.data)=='duration..min.'] <- 'bolus.duration'
  names(insulin.data)[names(insulin.data)=='carbs.g.'] <- 'carbs'
  
  # Return a list with the CGM and insulin data
  list(cgm.data=cgm.data, insulin.data=insulin.data)
}

ProcessOneWorkbookXlsx <- function(filename){
  message(paste('Processing', filename))
  
  # Process CGM data
  cgm.data <- read.xlsx(filename, sheetName='CGM', startRow=2)
  colnames(cgm.data) <- tolower(colnames(cgm.data))
  cgm.data$time <- strptime(cgm.data$time,"%d/%m/%Y %H:%M")
  
  # Process insulin data
  insulin.data <- read.xlsx(filename, sheetName='Insulin use and carbs')
  colnames(insulin.data) <- tolower(colnames(insulin.data))
  insulin.data$time <- strptime(insulin.data$time,"%d/%m/%Y %H:%M")
  
  names(insulin.data)[names(insulin.data)=='basal.amount..u.h.'] <- 'basal'
  names(insulin.data)[names(insulin.data)=='bolus.volume..u.'] <- 'bolus'
  names(insulin.data)[names(insulin.data)=='immediate.volume..u.'] <- 'bolus.immediate'
  names(insulin.data)[names(insulin.data)=='extended.volume..u.'] <- 'bolus.extended'
  names(insulin.data)[names(insulin.data)=='duration..min.'] <- 'bolus.duration'
  names(insulin.data)[names(insulin.data)=='carbs.g.'] <- 'carbs'
  
  # Return a list with the CGM and insulin data
  list(cgm.data=cgm.data, insulin.data=insulin.data)
}

ProcessDiasendCGMWorksheet <- function(workbook) {
  # Extracts CGM data from the given workbook and returns a data frame containing the CGM data
  #
  # Args:
  #  workbook: The input .xls file
  #
  # Returns:
  #  CGM data in data.frame format
  #  
  
  # Get the list of .xls files and process the CGM sheet one by one
  
}



#
# We use these packages
#
library(lubridate)
library(zoo)

#
# cgm.generate
# Calculate CGM parameters (# lows, # highs, mean, std dev) from CGM data if given filename
#
cgm.generate <- function(filename, bg.min, bg.max) {
  cgm.data <- read.csv(filename)
  cgm.data$TimePosix <- strptime(cgm.data$Time,"%d/%m/%Y %H:%M")
  cgm.data$DatePosix <- as.Date(cgm.data$TimePosix)
  out.mean <- aggregate(cgm.data$mmol.L, list(date=cgm.data$DatePosix), FUN = mean)
  out.sd <- aggregate(cgm.data$mmol.L, list(date=cgm.data$DatePosix), FUN = sd)
  out.min <- aggregate(cgm.data$mmol.L, list(date=cgm.data$DatePosix), function(x) sum(x < bg.min))
  out.max <- aggregate(cgm.data$mmol.L, list(date=cgm.data$DatePosix), function(x) sum(x > bg.max))
  out <- data.frame(date=out.mean$date, mean=out.mean$x, sd=out.sd$x, min=out.min$x, max=out.max$x)
  out
}

#
# convdate
# Converts a datetime object into a datetime object where the time is set to 00:00:00
#
convdate <- function(dt) {
  ISOdatetime(year(dt), month(dt), day(dt), 00,00,00)
}

#
# insulin.generate
# Generates basal insulin usage from diasend CSV file
#

insulin.generate <- function(filename) {
  in.data <- read.csv(filename, colClasses=c(NA,NA,'NULL','NULL','NULL','NULL','NULL','NULL','NULL'))

  in.data$TimePosix <- strptime(in.data$Time,"%d/%m/%Y %H:%M")
  in.data$Time <- NULL  
  names(in.data)[1] <- "Basal"
  names(in.data)[2] <- "Time"
  
  # we now have two columns - Basal and Time (time is in Posix format)
  
  # Create a column called date where the time is set to zero (and the date is per the time column)
  in.data <- transform(in.data, Date=convdate(Time))
  
  # Create a vector of unique dates
  Time <- unique(in.data$Date)
  dummy.entries <- data.frame(Time, Date=Time)
  
  # Now merge this data frame with our original data frame. This will add in rows with null Basal values where the time is midnight
  in.data <- merge(in.data, dummy.entries, all=TRUE)
  
  # Fill in NA Basal values with the previous value (make sure we don't start with an NA otherwise the vectors will be different
  # lengths)
  #
  # if(is.na(in.data$Basal[1])) in.data$Basal[1]=in.data$Basal[2]
  temp = na.locf(in.data$Basal, fromLast=FALSE)
  in.data$Basal <- append(c(rep(0,length(in.data$Basal) - length(temp))), temp)
  
  
  # Now calculate the total basal dose per row
  in.data$Duration <- append(diff(in.data$Time), c(0))/3600
  in.data <- transform(in.data, TotalBasal=Duration*Basal)
  
  # Nearly there - now sum up by date
  out <- aggregate(in.data$TotalBasal, list(Date=in.data$Date), FUN = sum)
  names(out)[1] <- "date"
  names(out)[2] <- "total.basal"
  
  # and finally, transform the date column from a posix date/time to a date type (so we can merge later on)
  out$date = as.Date(out$date)
  out
}


  
