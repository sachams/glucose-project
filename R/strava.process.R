## strava.load.activities
## Loads all strava activities from the specified directory into a list of data frames. Optionally downloads
## new activities from Strava.

strava.load.activities <- function(strava.token, directory, skip.existing=TRUE, single.id=NULL, sync=TRUE) {
	## If required, download all the latest activities
	if(sync) {
		strava.sync.activities(strava.token, directory, skip.existing, single.id)
	}
	
	## Now load all files from the specified directory
	files <- list.files(path=directory, pattern="*.csv", full.names=TRUE)
	strava.data <- lapply(files, read.csv)
	
	strava.data
}

strava.process.activities <- function(strava.data, hr.rest, hr.max) {	
	## Remove the distance column
	strava.data <- sapply(strava.data, function(x) {x$distance <- NULL; x})
	
	## Convert abs.time column to POSIX format
	strava.data <- sapply(strava.data, function(x) {x$abs.time <- strptime(x$abs.time,"%Y-%m-%d %H:%M:%S"); x})
	
	## Add trimp data
	strava.data <- sapply(strava.data, function(x) {add.trimp(x, hr.rest, hr.max)})
	
	strava.data
}


## 

## add.trimp
## Calculates trimp.exp and trimp duration for a single activity data frame

add.trimp <- function(activity.data, hr.rest, hr.max) {
	## Generate a vector of durations (ie, differences between one time and the next). have to add a zero at the start to make the 
	## vectors the same length
	message(paste("Processing activity from date/time", activity.data[1,'abs.time']))
	activity.data$trimp.duration <- append(c(0), diff(activity.data$time))
	
	## If there is no heartrate column, then don't calculate trimp.exp
	if(!is.null(activity.data$heartrate)){
		activity.data$trimp.exp <- calc.trimp.exp(activity.data$trimp.duration, 
												  activity.data$heartrate, 
												  hr.rest,
												  hr.max)
	}
	activity.data
}

#
# calc.hr.reserve
# Calculate heart rate reserve
#
calc.hr.reserve <- function(hr, hr.rest, hr.max) {
	hr.reserve <- (hr - hr.rest)/(hr.max - hr.rest)
	hr.reserve
}


#
# calc.trimp.exp
# Calculate exponential TRIMP for a single HR reading
#
calc.trimp.exp <- function(duration, hr, hr.rest, hr.max) {
	hr.reserve <- calc.hr.reserve(hr, hr.rest, hr.max) 
	trimp.exp <- (duration/60) * hr.reserve * 0.64 * exp(1.92 * hr.reserve)
	trimp.exp
}