###############################################################################
#
# Fitbit.R
# Module to provide easy access to the Fitbit v3 API.
# First of all, call FitbitAuthenticate to generate a token which you pass
# into the subsequent functions. 
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(httr)
library(jsonlite)
library(httpuv)
library(data.table)

FitbitAuthenticate <- function(client.id, client.secret) {
  # Performs OAuth2 authentication with Fitbit and returns an authentication token for 
  # future calls.
  #
  # Args:
  #  client.id: ID of the calling application (you get this when you create a new app on Fitbit)
  #  client.secret: Secret for your client. On the settings page of Fitbit
  #
  # Returns:
  #  Authentication token
  #
  
  # App credentials
  # request.url <- 'https://www.fitbit.com/oauth/authorize'
  request.url <- 'https://api.fitbit.com/oauth/request_token'
  authorize.url <- 'https://www.fitbit.com/oauth/authorize'
  access.url <- 'https://api.fitbit.com/oauth/access_token'
  
  fitbit.app <- oauth_app('fitbit', key=client.id, secret=client.secret)
  fitbit.endpoint <- oauth_endpoint(authorize=authorize.url, access=access.url, request=request.url)
  fitbit.token <- oauth1.0_token(fitbit.endpoint, fitbit.app)
  
  fitbit.token
}

FitbitGetAthlete <- function(fitbit.token) {
  # Returns Fitbit athlete details
  #
  # Returns:
  #  Athlete details in data.frame format
  #
  response <- GET('https://www.fitbit.com/api/v3/athlete', 
                  config(token = fitbit.token))
  stop_for_status(response)
  ConvertJsonResponse(response)
}

FitbitGetActivityList  <- function(fitbit.token) {
  # Returns a data.frame of all activities for the authenticated user.
  #
  # Args:
  #  fitbit.token: authentication returned from a call to FitbitAuthenticate
  #
  # Returns:
  #  Data.frame of athlete activities. 
  #
  all.activities <- NULL
  page <- 1
  
  repeat {
    # Requests are paginaged - we ask for them in batches of 100 (100 is arbitrary).
    
    request.url <- paste('https://www.fitbit.com/api/v3/athlete/activities','?per_page=100&page=',page,sep='')
    # message(paste('Loading', request.url))
    response <- GET(request.url, 
                    config(token = fitbit.token))
    stop_for_status(response)
    one.page <- ConvertJsonResponse(response)
    
    # We always end up with an object back, but if it is empty there's no more data to fetch
    if(length(one.page) == 0) {
      break
    }
    
    # Add the page to the previous results. Note that any lists must be flattened 
    # for this to work (see ConvertJsonReponse internals for details)
    all.activities <- rbind.fill(all.activities, one.page)
    page <- page + 1
  }
  
  message(paste('Retrieved',nrow(all.activities),'activities'))
  all.activities
}

FitbitGetActivity  <- function(fitbit.token, activity.id) {
  # Returns a single Fitbit activity
  #
  # Args:
  #  fitbit.token: authentication returned from a call to FitbitAuthenticate
  #
  # Returns:
  #  Data.frame of a single activity 
  #
  request.url <- paste('https://www.fitbit.com/api/v3/activities',
                       activity.id,
                       sep='/')
  
  response <- GET(request.url, 
                  config(token = fitbit.token))
  stop_for_status(response)
  ConvertJsonResponse(response)
}

FitbitSyncActivities <- function(fitbit.token, 
                                 directory, 
                                 skip.existing=TRUE, 
                                 single.id=NULL, 
                                 streams=c('time', 'heartrate')) {
  # Downloads activity streams for all Fitbit activities to CSV files in the specified directory.
  #
  # It checks if the activitiy has been downloaded before by matching filename in the target directory
  # and if so, doesn't download again.
  # Note that if you specified a different set of streams to retrieve then you should set skip.existing
  # to FALSE to force a re-download of all activities.
  # The CSV files contain the streams specified in the streams parameter, along with an absolute time
  # column, derived from the start_date_local of the activity and the incremental time column.
  #
  # Args:
  #  fitbit.token: authentication returned from a call to FitbitAuthenticate
  #  directory: target directory to save CSV files into
  #  skip.existing: if TRUE, checks to see if the activity has been downloaded before and skips if so
  #  single.id: Specifies a single Fitbit activity ID to download, rather than all activities. Handy for debugging.
  #  streams: A vector of names of the streams to retrieve. See Fitbit API for details.
  #
  # Returns:
  #  Nothing
  
  
  # Get a list of all the activities
  message("Downloading list of activities...")
  activities <- FitbitGetActivityList(fitbit.token)
  
  # See if a single activity has been specified
  if(!is.null(single.id)) {
    message(paste('Downloading only a single ID', single.id))
    activities <- activities[activities$id==single.id,]
  }
  
  # Apply the download function to each result
  apply(activities, 1, function(x) { 
    # Grab the start time. Used to form the filename as well as generate absolute times.
    # Also calculate the time difference to GMT - if we throw this away now we can't get it back
    # Note that the timezone field doesn't take into account daylight saving time
    start.time <- as.POSIXct(strptime(x$start_date_local,'%Y-%m-%dT%H:%M:%SZ', tz='GMT'))
    gmt.diff <- round(as.double((strptime(x$start_date_local,'%Y-%m-%dT%H:%M:%SZ', tz='GMT') - strptime(x$start_date,'%Y-%m-%dT%H:%M:%SZ', tz='GMT')), units='hours'))
    
    # Manual entries don't have any streams, so skip
    is.manual <- x$manual
    if(is.manual) {
      message(paste('Skipping activity',x$id,'as it was manually entered'))
    } else {
      FitbitDownloadStream(fitbit.token, 
                           directory,
                           x$id, 
                           streams,
                           skip.existing=skip.existing,
                           start.time=start.time,
                           gmt.diff=gmt.diff)
    }
  } )
}

FitbitLoadActivities <- function(directory) {
  # Loads all Fitbit CSV files from the speciried directory and performs the following processing:
  #  1. Removes the distance column
  #  2. Converts time column to POSIX time
  #
  # Args:
  #  directory: Directory containing the CSV files. All *.csv files will be loaded.
  # Returns:
  #  Returns a list of data frames, with each data frame corresponding to a separate 
  #  Fitbit activity.
  #
  
  # Load all files from the specified directory
  files <- list.files(path=directory, pattern="*.csv", full.names=TRUE)
  message(paste('Found',length(files),'Fitbit files. Loading...'))
  fitbit.data <- lapply(files, read.csv)
  
  # Remove the distance column
  fitbit.data <- sapply(fitbit.data, function(x) {x$distance <- NULL; x})
  
  # Convert abs.time column to POSIX format
  fitbit.data <- sapply(fitbit.data, function(x) {x$abs.time <- strptime(x$abs.time,"%Y-%m-%d %H:%M:%S", tz='GMT'); x})
  
  fitbit.data
}

FitbitDownloadStream <- function(fitbit.token, 
                                 directory, 
                                 activity.id, 
                                 streams, 
                                 skip.existing=TRUE, 
                                 start.time=NULL,
                                 gmt.diff=NULL) {
  # Downloads specified streams for a single activity and saves it as a CSV file.
  #
  # Args:
  #  fitbit.token: authentication returned from a call to FitbitAuthenticate
  #  directory: target directory to save CSV files into
  #  activity.id: Specifies a single Fitbit activity ID to download.
  #  streams: A vector of names of the streams to retrieve. See Fitbit API for details.
  #  skip.existing: if TRUE, checks to see if the activity has been downloaded before and skips if so
  #  start.time:  Optional start time for the activity. If provided, an extra column (abs.time)
  #        is added to the CSV file. 
  #  gmt.diff: Difference to GMT of the specified start time
  #
  # Returns:
  #  Nothing
  #
  
  if(is.null(start.time)) {
    # start time not speciied so build filename in the form activityid.csv
    filename <- sprintf("%s/%d.csv", directory, activity.id)
  } else {
    # Build filename in the form YYYYMMDD_HHMMSS_activityid.csv
    filename <- sprintf("%s/%s_%d.csv", directory, format(start.time, "%Y%m%d_%H%M%S"), activity.id)
  }
  
  # Check if the file exists alrady. If it does, skip.
  if(file.exists(filename) && skip.existing) {
    message(paste("Not downloading activity", activity.id,"as target file already exists"))
  } else {
    
    # Download the streams to a CSV file in the specified directory
    if(file.exists(filename)) {
      message(paste("Re-downloading activity", activity.id,"..."))
    } else {
      message(paste("Downloading activity", activity.id,"..."))
    }
    dt <- data.table(FitbitGetStream(fitbit.token, activity.id, streams))
    
    # If a start date is specified, then add in an additional column with the absolute time
    if(!is.null(start.time)) {
      dt[,abs.time:=time+start.time]
      dt[,gmt.diff:=gmt.diff]
    }  
    
    write.csv(dt, filename, row.names=FALSE)
  }
}

FitbitGetStream <- function(fitbit.token, activity.id, streams) {
  # Returns a data frame of the specified streams
  #
  # Args:
  #  fitbit.token: authentication returned from a call to FitbitAuthenticate
  #  activity.id: Specifies a single Fitbit activity ID to download.
  #  streams: A vector of names of the streams to retrieve. See Fitbit API for details.
  #
  # Returns:
  #  Data frame of the requested streams
  #
  
  # Paste the stream names into a comma-separated list
  streams.text <- paste(streams,collapse=',')
  
  request.url <- paste('https://www.fitbit.com/api/v3/activities',
                       activity.id,
                       'streams',
                       streams.text,
                       sep='/')
  
  # Make the request to Fitbit
  response <- GET(request.url, config(token = fitbit.token))
  stop_for_status(response)
  output <- ConvertJsonResponse(response)
  
  # Extract the data and rename the headers to correspond to the requested types
  df <- as.data.frame(output$data)
  names(df) <- output$type
  df
}

#
# Utility functions
#

ConvertJsonResponse <- function(data) {
  # Converts a response in JSON format to a R data frame.
  #
  # Note that if a cell contains a list, a new column is created (ie, the table is flattened).
  # This is so that tables can be rbind'ed together (not possible with embedded lists).
  # Args:
  #  data: response data from a call to GET
  #
  # Returns:
  #   Data in data.frame format
  # 
  data.content <- content(data)
  jsonlite::fromJSON(toJSON(data.content),flatten=TRUE)
}

GetUTCOffset <- function(fitbit.timezone) {
  # Extracts a UTC offset from a Fitbit timezone.
  #
  # Args:
  #  fitbit.timezone: Fitbit timezone in the format "(GMT+01:00) Europe/Amsterdam"
  #
  # Returns:
  #   UTC offset in hours
  # 
}

