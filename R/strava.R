###############################################################################
#
# Strava.R
# Module to provide easy access to the Strava v3 API.
# First of all, call StravaAuthenticate to generate a token which you pass
# into the subsequent functions. 
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(httr)
library(jsonlite)
library(httpuv)
library(data.table)

StravaAuthenticate <- function(client.id, client.secret) {
	# Performs OAuth2 authentication with Strava and returns an authentication token for 
	# future calls.
	#
	# Args:
	#	client.id: ID of the calling application (you get this when you create a new app on Strava)
	#	client.secret: Secret for your client. On the settings page of Strava
	#
	# Returns:
	#	Authentication token
	#
	
	# App credentials
	request.url <- 'https://www.strava.com/oauth/authorize'
	access.url <- 'https://www.strava.com/oauth/token'
	
	strava.app <- oauth_app('strava', key=client.id, secret=client.secret)
	strava.endpoint <- oauth_endpoint(authorize=request.url, access=access.url)
	strava.token <- oauth2.0_token(strava.endpoint, strava.app)
	
	strava.token
}

StravaGetAthlete <- function(strava.token) {
	# Returns Strava athlete details
	#
	# Returns:
	#	Athlete details in data.frame format
	#
	response <- GET('https://www.strava.com/api/v3/athlete', 
			   config(token = strava.token))
	stop_for_status(response)
	ConvertJsonResponse(response)
}

StravaGetActivityList  <- function(strava.token) {
	# Returns a data.frame of all activities for the authenticated user.
	#
	# Args:
	#	strava.token: authentication returned from a call to StravaAuthenticate
	#
	# Returns:
	#	Data.frame of athlete activities. 
	#
	all.activities <- NULL
	page <- 1
	
	repeat {
		# Requests are paginaged - we ask for them in batches of 100 (100 is arbitrary).

		request.url <- paste('https://www.strava.com/api/v3/athlete/activities','?per_page=100&page=',page,sep='')
		response <- GET(request.url, 
						config(token = strava.token))
		stop_for_status(response)
		one.page <- ConvertJsonResponse(response)
		
		# We always end up with an object back, but if it is empty there's no more data to fetch
		if(length(one.page) == 0) {
			break
		}
		
		# Add the page to the previous results. Note that any lists must be flattened 
		# for this to work (see ConvertJsonReponse internals for details)
		all.activities <- rbind(all.activities, one.page)
		page <- page + 1
	}
	
	message(paste('Retrieved',nrow(all.activities),'activities'))
	all.activities
}

StravaGetActivity  <- function(strava.token, activity.id) {
	# Returns a single Strava activity
	#
	# Args:
	#	strava.token: authentication returned from a call to StravaAuthenticate
	#
	# Returns:
	#	Data.frame of a single activity 
	#
	request.url <- paste('https://www.strava.com/api/v3/activities',
					 activity.id,
					 sep='/')
				 
	response <- GET(request.url, 
			   config(token = strava.token))
	stop_for_status(response)
	ConvertJsonResponse(response)
}

StravaSyncActivities <- function(strava.token, 
								 directory, 
								 skip.existing=TRUE, 
								 single.id=NULL, 
								 streams=c('time', 'heartrate')) {
	# Downloads activity streams for all Strava activities to CSV files in the specified directory.
	#
	# It checks if the activitiy has been downloaded before by matching filename in the target directory
	# and if so, doesn't download again.
	# Note that if you specified a different set of streams to retrieve then you should set skip.existing
	# to FALSE to force a re-download of all activities.
	# The CSV files contain the streams specified in the streams parameter, along with an absolute time
	# column, derived from the start_date_local of the activity and the incremental time column.
	#
	# Args:
	#	strava.token: authentication returned from a call to StravaAuthenticate
	#	directory: target directory to save CSV files into
	#	skip.existing: if TRUE, checks to see if the activity has been downloaded before and skips if so
	#	single.id: Specifies a single Strava activity ID to download, rather than all activities. Handy for debugging.
	#	streams: A vector of names of the streams to retrieve. See Strava API for details.
	#
	# Returns:
	#	Nothing
	#
	
	# Get a list of all the activities
	message("Downloading list of activities...")
	activities <- StravaGetActiviyList(strava.token)
	
	# Simplify the results
	activities <- activities[,c('id','start_date_local', 'manual')]
	
	# See if a single activity has been specified
	if(!is.null(single.id)) {
		message(paste('Downloading only a single ID', single.id))
		activities <- activities[activities$id==single.id,]
	}
	
	# Apply the download function to each result
	apply(activities, 1, function(x) { 
		# Grab the start time. Used to form the filename as well as generate absolute times
		start.time <- as.POSIXct(strptime(x$start_date_local,'%Y-%m-%dT%H:%M:%SZ'))
		
		# Manual entries don't have any streams, so skip
		is.manual <- x$manual
		if(is.manual) {
			message(paste('Skipping activity',x$id,'as it was manually entered'))
		} else {
			StravaDownloadStream(strava.token, 
								 directory,
								 x$id, 
								 streams,
								 skip.existing=skip.existing,
								 start.time=start.time)
		}
	} )
}

StravaDownloadStream <- function(strava.token, 
									directory, 
									activity.id, 
									streams, 
									skip.existing=TRUE, 
									start.time=NULL) {
	# Downloads specified streams for a single activity and saves it as a CSV file.
	#
	# Args:
	#	strava.token: authentication returned from a call to StravaAuthenticate
	#	directory: target directory to save CSV files into
	#	activity.id: Specifies a single Strava activity ID to download.
	#	streams: A vector of names of the streams to retrieve. See Strava API for details.
	#	skip.existing: if TRUE, checks to see if the activity has been downloaded before and skips if so
	#	start.time:	Optional start time for the activity. If provided, an extra column (abs.time)
	#				is added to the CSV file. 
	#
	# Returns:
	#	Nothing
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
		dt <- data.table(StravaGetStream(strava.token, activity.id, streams))
	
		# If a start date is specified, then add in an additional column with the absolute time
		if(!is.null(start.time)) {
			dt[,abs.time:=time+start.time]
		}	

		write.csv(dt, filename, row.names=FALSE)
	}
}

StravaGetStream <- function(strava.token, activity.id, streams) {
	# Returns a data frame of the specified streams
	#
	# Args:
	#	strava.token: authentication returned from a call to StravaAuthenticate
	#	activity.id: Specifies a single Strava activity ID to download.
	#	streams: A vector of names of the streams to retrieve. See Strava API for details.
	#
	# Returns:
	#	Data frame of the requested streams
	#
	
	# Paste the stream names into a comma-separated list
	streams.text <- paste(streams,collapse=',')
	
	request.url <- paste('https://www.strava.com/api/v3/activities',
					 activity.id,
					 'streams',
					 streams.text,
					 sep='/')
	
	# Make the request to Strava
	response <- GET(request.url, config(token = strava.token))
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
	#	data: response data from a call to GET
	#
	# Returns:
	# 	Data in data.frame format
	# 
	data.content <- content(data)
	jsonlite::fromJSON(toJSON(data.content),flatten=TRUE)
}
