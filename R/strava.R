library(httr)
library(jsonlite)
library(httpuv)
library(data.table)



reformat.json <- function(data) {
	json1 <- content(data)
	json2 <- jsonlite::fromJSON(toJSON(json1),flatten=TRUE)
	json2
}

strava.get.athlete <- function(strava.token) {
	req <- GET('https://www.strava.com/api/v3/athlete', 
			   config(token = strava.token))
	stop_for_status(req)
	reformat.json(req)
}

strava.get.activities  <- function(strava.token) {
	all.activities <- NULL
	page <- 1
	
	repeat {
		one.page <- strava.get.activitypage(strava.token, page)
		if(length(one.page) == 0) {
			break
		}
		
		all.activities <- rbind(all.activities, one.page)
		page <- page + 1
	}
	
	all.activities
}

strava.get.activitypage  <- function(strava.token, page) {
	reqUrl <- paste('https://www.strava.com/api/v3/athlete/activities','?per_page=100&page=',page,sep='')
	## message(reqUrl)
	req <- GET(reqUrl, 
			   config(token = strava.token))
	stop_for_status(req)
	reformat.json(req)
}

strava.get.activity  <- function(strava.token, activity.id) {
	req.url <- paste('https://www.strava.com/api/v3/activities',
					 activity.id,
					 sep='/')
				 
	req <- GET(req.url, 
			   config(token = strava.token))
	stop_for_status(req)
	reformat.json(req)
}

strava.sync.activities <- function(strava.token, directory, skip.existing=TRUE, single.id=NULL) {
	## Get a list of all the activities
	message("Downloading list of activities...")
	activities <- strava.get.activities(strava.token)
	
	## Simplify the results
	activities <- activities[,c('id','start_date_local', 'manual')]
	
	## See if a single activity has been specified
	if(!is.null(single.id)) {
		message(paste('Downloading only a single ID', single.id))
		activities <- activities[activities$id==single.id,]
	}
	
	## Apply the download function to each result
	apply(activities, 1, function(x) { 
		start.time <- as.POSIXct(strptime(x$start_date_local,'%Y-%m-%dT%H:%M:%SZ'))
		
		## Manual entries don't have any streams, so skip
		is.manual <- x$manual
		if(is.manual) {
			message(paste('Skipping activity',x$id,'as it was manually entered'))
		} else {
			strava.download.streams(strava.token, 
									directory, 
									x$id, 
									c('time', 'heartrate'), 
									skip.existing=skip.existing, 
									start.time=start.time)
		}
	} )
}



strava.download.streams <- function(strava.token, 
									directory, 
									activity.id, 
									streams, 
									skip.existing=TRUE, 
									start.time=NULL) {
	## Build filename in the form activityid.csv
	filename <- sprintf("%s/%s_%d.csv", directory, format(start.time, "%Y%m%d_%H%M%S"), activity.id)

	## Check if the file exists alrady. If it does, skip.
	if(file.exists(filename) && skip.existing) {
		message(paste("Not downloading activity", activity.id,"as target file already exists"))
	} else {
		
		## Download the streams to a CSV file in the specified directory
		if(file.exists(filename)) {
			message(paste("Re-downloading activity", activity.id,"..."))
		} else {
			message(paste("Downloading activity", activity.id,"..."))
		}
		dt <- data.table(strava.get.streams(strava.token, activity.id, streams))
	
		## If a start date is specified, then add in an additional column with the absolute time
		if(!is.null(start.time)) {
			dt[,abs.time:=time+start.time]
		}	

		write.csv(dt, filename, row.names=FALSE)
	}
}

strava.get.streams <- function(strava.token, activity.id, streams) {
	## Check stream type is allowed

	
	streams.text <- paste(streams,collapse=',')
	
	req.url <- paste('https://www.strava.com/api/v3/activities',
					 activity.id,
					 'streams',
					 streams.text,
					 sep='/')
	
	req <- GET(req.url, 
			   config(token = strava.token))
	stop_for_status(req)
	output <- reformat.json(req)
	
	## Extract the data nad rename the headers to correspond to the requested types
	df <- as.data.frame(output$data)
	names(df) <- output$type
	df
}

strava.authenticate <- function(details) {
	## App credentials
	reqURL <- 'https://www.strava.com/oauth/authorize'
	accessURL <- 'https://www.strava.com/oauth/token'

	strava.app <- oauth_app('strava', key=details$client.id, secret=details$client.secret)
	strava.endpoint <- oauth_endpoint(authorize=reqURL, access=accessURL)
	strava.token <- oauth2.0_token(strava.endpoint, strava.app)
	
	strava.token
}