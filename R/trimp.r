###############################################################################
#
# trimp.R
# Calculates TRIMP for Strava data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

CalculateTrimpData <- function(strava.data, hr.rest, hr.max) {	
	# Adds trimp.exp and trimp.duration columns to Strava data as returned from GetStravaActivities.
	# trimp.exp is described here: http://fellrnr.com/wiki/TRIMP#TRIMPexp_Exponental_Heart_Rate_Scaling
	# trimp.duration is described here: http://fellrnr.com/wiki/TRIMP#Training_Volume
	#
	# Args:
	#	strava.data: Data as returned from GetStravaActivities
	#	hr.rest: Resting heart rate
	#	hr.max: Maximum heart rate
	#	
	# Returns:
	#	Returns the input data with trimp.exp and trimp.duration columns added. Note that if no heart rate
	#	data is present in an input data frame, trimp.exp will be NA
	#
	
	# Add trimp data
	strava.data <- sapply(strava.data, function(x) {AddTrimpToDataFrame(x, hr.rest, hr.max)})
	
	## Rebucket the data
	
	strava.data
}

#
# Utility functions
#

AddTrimpToDataFrame <- function(activity.data, hr.rest, hr.max) {
	# Calculates trimp.exp and trimp duration for a single activity data frame and adds it to the data frame.
	# If heart rate data is not available, trimop.exp will be NA
	#
	# Args:
	#	activity.data: single data.frame of activity data. Expected columns:
	#						time
	#						heartrate (optional)
	#						abs.time
	#	hr.rest: resting heart rate
	#	hr.max: maximum heart rate
	#
	# Returns:
	#	Input data with two new columns: trimp.exp and trimp.duration
	#
	
	message(paste("Processing activity from date/time", activity.data[1,'abs.time']))
	
	# Generate a vector of durations (ie, differences between one time and the next). have to add a zero at the start to make the 
	# vectors the same length
	activity.data$trimp.duration <- append(c(0), diff(activity.data$time))
	
	# If there is no heartrate column, then don't calculate trimp.exp and instead add a columnn of NAs
	if(is.null(activity.data$heartrate)){
		activity.data$trimp.exp <- rep(NA, nrow(activity.data))
	} else {
		activity.data$trimp.exp <- CalculateTrimpExp(activity.data$trimp.duration, 
													 activity.data$heartrate, 
													 hr.rest,
													 hr.max)
	}
	activity.data
}

CalculateTrimpExp <- function(duration, hr, hr.rest, hr.max) {
	# Calculates exponential TRIMP for a single HR reading
	#
	# Args:
	#	duration: duration of the activity in seconds
	#	hr: heart rate
	#	hr.rest: resting heart rate
	#	hr.max: maximum heart rate
	#
	# Returns:
	#	Exponential training impulse as defined here: http://fellrnr.com/wiki/TRIMP#TRIMPexp_Exponental_Heart_Rate_Scaling
	#
	
	hr.reserve <- (hr - hr.rest)/(hr.max - hr.rest) 
	trimp.exp <- (duration/60) * hr.reserve * 0.64 * exp(1.92 * hr.reserve)
	trimp.exp
}