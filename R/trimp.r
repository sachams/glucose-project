#
# We use these packages
#
library(lubridate)
library(zoo)


#
# hrReserve
# Calculate heart rate reserve
#
hrReserve <- function(hr, hr.rest, hr.max) {
	out <- (hr - hr.rest)/(hr.max - hr.rest)
	out
}


#
# trimp.exp
# Calculate exponential TRIMP for a single HR reading
#
trimp.exp <- function(duration, hr, hr.rest, hr.max) {
	hr.reserve <- hrReserve(hr, hr.rest, hr.max) 
	out <- (duration/60) * hr.reserve * 0.64 * exp(1.92 * hr.reserve)
	out
}

#
# trimp.table.exp
# Calculate TRIMP exp for a whole table of data
#
trimp.table.exp <- function(hrDataFrame, hr.rest, hr.max) {
	hrDataFrame$Time <- strptime(hrDataFrame$Time,"%d/%b/%Y %H:%M:%S")
	hrDataFrame$Diff <- append(c(0), diff(hrDataFrame$Time))
	hrDataFrame$trimp.exp <- trimp.exp(hrDataFrame$Diff,hrDataFrame$Heart.Rate,hr.rest,hr.max)
	trimp.exp.num <- sum(hrDataFrame$trimp.exp)
	trimp.exp.num
}


#
# trimp.table.duration
# Calculate TRIMP duration (ie, the aggregate duration) for a whole table of data
#
trimp.table.duration <- function(hrDataFrame) {
	startTime <- strptime(hrDataFrame[[1]][[1]], "%d/%b/%Y %H:%M:%S")
	endTime <- strptime(hrDataFrame[[1]][[length(hrDataFrame[[1]])]], "%d/%b/%Y %H:%M:%S")
	duration <- difftime(endTime, startTime, units="mins")
	duration
}


#
# trimp.aggregate.day
# Aggregates trimp data by day
#
trimp.aggregate.day <- function(in.data) {
	in.data$date <- as.Date(in.data$time)
	out.trimp.exp <- aggregate(in.data$trimp.exp, list(date=in.data$date), FUN = sum)
	out.trimp.duration <- aggregate(in.data$trimp.duration, list(date=in.data$date), FUN = sum)

	out <- data.frame(date=out.trimp.exp$date, trimp.exp=out.trimp.exp$x, trimp.duration=out.trimp.duration$x)
	out
}

#
# trimp.generate
# Calculate all TRIMP data for all files at the given path
#
trimp.generate <- function(directory.mask, hr.rest, hr.max) {
	files <- list.files(pattern=directory.mask)
	input.data <- lapply(files, read.csv)
	trimp.exp.data <- sapply(input.data, function(x) trimp.table.exp(x, hr.rest, hr.max))
	trimp.duration.data <- sapply(input.data, function(x) trimp.table.duration(x))
	time.data <- strptime(sapply(input.data, function(x) x[1,1]),"%d/%b/%Y %H:%M:%S")
	output.data = data.frame(time=time.data, trimp.exp=trimp.exp.data, trimp.duration=trimp.duration.data)
	output.data
}

#
# trimp.generate.aggregate
# Calculate all TRIMP data for all files at the given path, and aggregate per day
#
trimp.generate.aggregate <- function(directory.mask, hr.rest, hr.max) {
	output.data <- trimp.generate(directory.mask, hr.rest, hr.max)
	trimp.aggregate.day(output.data)
}

