library(httr)
library(jsonlite)
library(httpuv)

client.details <- list(client.id='1325', 
					   client.secret='35003e7b2c50659526af5e3c22dc09268b245b3e')

reformat.json <- function(data) {
	json1 <- content(data)
	json2 <- jsonlite::fromJSON(toJSON(json1))
	json2
}

strava.get.athlete <- function(strava.token) {
	req <- GET('https://www.strava.com/api/v3/athlete', 
			   config(token = strava.token))
	stop_for_status(req)
	reformat.json(req)
}

strava.get.activities  <- function(strava.token) {
	## TODO - add pagination
	req <- GET('https://www.strava.com/api/v3/athlete/activities', 
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

strava.download.streams <- function(strava.token, directory, activity.id, stream) {
	## Download the streams to a CSV file in the specified directory
}

strava.get.streams <- function(strava.token, activity.id, stream) {
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