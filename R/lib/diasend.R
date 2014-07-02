###############################################################################
#
# diasend.R
# Processes Diasend XLS spreadsheets to extract CGM and insulin data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(gdata)


DiasendProcessWorkbooks <- function(directory) {
  # Converts Diasend .xls files in the given directory into two CSV files, one for cgm and one for insulin data per input .xls.
  # The output files are named _cgm.csv and _insulin.csv.
  #
  # Args:
  #  directory: Directory contining input .xls files. The .csv fiels are written to the same directory.
  #
  # Returns:
  #  Nothing
  #  
  
  # Get the list of .xls files and process them one by one
  files <- list.files(path=directory, pattern="*.xls", full.names=TRUE)
  message(paste('Found',length(files),'Diasend files. Processing...'))
  
  # Extract CGM and insulin data
  cgm.data <- lapply(files, DiasendProcessCGMWorkbook)
  insulin.data <- lapply(files, DiasendProcessInsulinWorkbook)
  
  # Bind the results together
  cgm.data <- do.call('rbind', cgm.data)
  insulin.data <- do.call('rbind', insulin.data)

  # Remove duplicates
  cgm.data <- cgm.data[!duplicated(cgm.data),]
  insulin.data <- insulin.data[!duplicated(insulin.data),]
  
  # Split out the insulin data into separate sets (otherwise you have stacks of NAs and 
  # it makes it hard to see what is going on)
  insulin.data$time <- as.POSIXct(insulin.data$time) # needed to do merging on time
  basal.data <- insulin.data[!is.na(insulin.data$basal),c('time','basal')]
  bolus.data <- insulin.data[!is.na(insulin.data$bolus),c('time','bolus')]
  carbs.data <- insulin.data[!is.na(insulin.data$carbs),c('time','carbs')]

  # Merge the carbs and bolus back together again, as they are split out on separate rows
  bolusandcarbs.data <- merge(bolus.data, carbs.data, by.x='time', by.y='time', all=TRUE)
  
  # Replace NAs with zeros
  bolusandcarbs.data[is.na(bolusandcarbs.data$carbs), 'carbs'] <- 0
  bolusandcarbs.data[is.na(bolusandcarbs.data$bolus), 'bolus'] <- 0
  
  # And save the files
  write.csv(cgm.data, paste(directory,'cgm.csv', sep='/'), row.names=FALSE)
  write.csv(bolusandcarbs.data, paste(directory,'bolusandcarbs.csv', sep='/'), row.names=FALSE)
  write.csv(basal.data, paste(directory,'basal.csv', sep='/'), row.names=FALSE)
  
  invisible()
}

DiasendLoadData <- function(directory) {
  # Loads *_cgm.csv files from the given directory and returns a single dta frame containing CGM data.
  # Any overlapping/duplicate data is removed.
  #
  # Args:
  #  directory: Directory contining input .csv files
  #
  # Returns:
  #  data.frame with CGM data
  #  
  cgm.data <- DiasendLoadIndividualFile(directory, 'cgm.csv')
  basal.data <- DiasendLoadIndividualFile(directory,'basal.csv')
  bolusandcarbs.data <- DiasendLoadIndividualFile(directory,'bolusandcarbs.csv')

  list(cgm.data=cgm.data, basal.data=basal.data, bolusandcarbs.data=bolusandcarbs.data)
}

DiasendLoadIndividualFile <- function(directory, filename) {
  # Loads *_insulin.csv files from the given directory and returns a single dta frame containing insulin data.
  # Any overlapping/duplicate data is removed.
  #
  # Args:
  #  directory: Directory contining input .csv files
  #
  # Returns:
  #  data.frame with insulin data
  #  
  full.name <- paste(directory, filename,sep='/')
  
  if(file.exists(full.name)) {
    message(paste('Loading file', full.name, '...'))
    data <- read.csv(full.name)
    data$time <- strptime(data$time,"%Y-%m-%d %H:%M:%S", tz='GMT')
  } else {
    message('Unable to find file', full.name)
  }
  
  data
}

DiasendProcessCGMWorkbook <- function(input.filename) {
  # Exracts CGM data from the specified .xls file and saves it as a csv file in the same directory.
  # If the target file exists and skip.existing=TRUE, the file is not re-saved.
  # 
  # Args:
  #   input.filename: Input .xls file
  #   skip.existing: Don't process the file if the target filename exists
  # 
  # Returns:
  #   Nothing
  # 
  
  # Process CGM data
  cgm.data <- read.xls(input.filename, sheet='CGM', skip=1)
  colnames(cgm.data) <- tolower(colnames(cgm.data))
  cgm.data$time <- strptime(cgm.data$time,"%d/%m/%Y %H:%M", tz='GMT')

  names(cgm.data)[names(cgm.data)=='mmol.l'] <- 'blood.glucose'
  
  cgm.data
}

DiasendProcessInsulinWorkbook <- function(input.filename) {
  # Exracts insulin data from the specified .xls file and saves it as a csv file in the same directory.
  # If the target file exists and skip.existing=TRUE, the file is not re-saved.
  # 
  # Args:
  #   input.filename: Input .xls file
  #   skip.existing: Don't process the file if the target filename exists
  # 
  # Returns:
  #   Nothing
  # 

  # Process insulin data
  insulin.data <- read.xls(input.filename, sheet='Insulin use and carbs')
  colnames(insulin.data) <- tolower(colnames(insulin.data))
  insulin.data$time <- strptime(insulin.data$time,"%d/%m/%Y %H:%M", tz='GMT')
  
  names(insulin.data)[names(insulin.data)=='basal.amount..u.h.'] <- 'basal'
  names(insulin.data)[names(insulin.data)=='bolus.volume..u.'] <- 'bolus'
  names(insulin.data)[names(insulin.data)=='immediate.volume..u.'] <- 'bolus.immediate'
  names(insulin.data)[names(insulin.data)=='extended.volume..u.'] <- 'bolus.extended'
  names(insulin.data)[names(insulin.data)=='duration..min.'] <- 'bolus.duration'
  names(insulin.data)[names(insulin.data)=='carbs.g.'] <- 'carbs'
  
  insulin.data
}

RoundTime <- function(time, rounding) {
  as.POSIXlt(round(as.double(testdt)/rounding)*rounding, origin='1970-01-01', tz='GMT')
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