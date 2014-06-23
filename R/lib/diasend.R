###############################################################################
#
# diasend.R
# Processes Diasend XLS spreadsheets to extract CGM and insulin data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(gdata)

DiasendProcessWorkbooks <- function(directory, skip.existing=TRUE) {
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
  message(paste('Found',length(files),'files. Loading...'))
  
  lapply(files, function(x) DiasendProcessCGMWorkbook(x, skip.existing))
  lapply(files, function(x) DiasendProcessInsulinWorkbook(x, skip.existing))
  
  invisible()
}

DiasendLoadCGMData <- function(directory) {
  # Loads *_cgm.csv files from the given directory and returns a single dta frame containing CGM data.
  # Any overlapping/duplicate data is removed.
  #
  # Args:
  #  directory: Directory contining input .csv files
  #
  # Returns:
  #  data.frame with CGM data
  #  
  
  cgm.files <- list.files(path=directory, pattern="*_cgm.csv", full.names=TRUE)
  message(paste('Found',length(cgm.files),'CGM files. Loading...'))

  cgm.data <- lapply(cgm.files, read.csv)
  cgm.data <- do.call('rbind', cgm.data)
  cgm.data$time <- strptime(cgm.data$time,"%Y-%m-%d %H:%M:%S", tz='GMT')

  # Remove duplicates
  cgm.data <- cgm.data[!duplicated(cgm.data),]
  
  cgm.data
}

DiasendLoadInsulinData <- function(directory) {
  # Loads *_insulin.csv files from the given directory and returns a single dta frame containing insulin data.
  # Any overlapping/duplicate data is removed.
  #
  # Args:
  #  directory: Directory contining input .csv files
  #
  # Returns:
  #  data.frame with insulin data
  #  
  
  insulin.files <- list.files(path=directory, pattern="*_insulin.csv", full.names=TRUE)
  message(paste('Found',length(insulin.files),'insulin files. Loading...'))
  
  insulin.data <- lapply(insulin.files, read.csv)
  insulin.data <- do.call('rbind', insulin.data)
  insulin.data$time <- strptime(insulin.data$time,"%Y-%m-%d %H:%M:%S", tz='GMT')
  
  # Remove duplicates
  insulin.data <- insulin.data[!duplicated(insulin.data),]
  
  insulin.data
}

DiasendProcessCGMWorkbook <- function(input.filename, skip.existing=TRUE) {
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
  
  # Create the output filename
  output.filename <- paste(substr(input.filename, 1, nchar(input.filename)-4), 
                           '_cgm.csv',
                           sep='')

  # Check if the file exists alrady. If it does, skip.
  if(file.exists(output.filename) && skip.existing) {
    message(paste("Not generating", output.filename,"as it already exists"))
  } else {
    if(file.exists(output.filename)) {
      message(paste("Re-extracting to", output.filename,"..."))
    } else {
      message(paste("Extracting to", output.filename,"..."))
    }
    
    # Process CGM data
    cgm.data <- read.xls(input.filename, sheet='CGM', skip=1)
    colnames(cgm.data) <- tolower(colnames(cgm.data))
    cgm.data$time <- strptime(cgm.data$time,"%d/%m/%Y %H:%M", tz='GMT')
    
    write.csv(cgm.data, output.filename, row.names=FALSE)
  }
}

DiasendProcessInsulinWorkbook <- function(input.filename, skip.existing=TRUE) {
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

  # Create the output filename
  output.filename <- paste(substr(input.filename, 1, nchar(input.filename)-4), 
                           '_insulin.csv',
                           sep='')
  
  
  # Check if the file exists alrady. If it does, skip.
  if(file.exists(output.filename) && skip.existing) {
    message(paste("Not generating", output.filename,"as it already exists"))
  } else {
    if(file.exists(output.filename)) {
      message(paste("Re-extracting to", output.filename,"..."))
    } else {
      message(paste("Extracting to", output.filename,"..."))
    }
    
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
    
    write.csv(insulin.data, output.filename, row.names=FALSE)
  }
}
