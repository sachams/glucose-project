# Load dependent modules
source("trimp.r")
source("diasend.r")


#
# Collate and aggregte TRIMP and basal insulin data per day, and write it as a CSV file
#
process.files <- function(strava.dir, diasend.insulin.filename, output.filename)
{
	# First proces the Strava files.
	print("Processing Strava files...")
	trimp.data = trimp.generate.aggregate(strava.dir, 69, 185)
		
	# Now process the diasend data
	print("Processing Diasend files...")
	diasend.insulin.data = insulin.generate(diasend.insulin.filename)
	
	# Merge the two tables together
	out.data = merge(trimp.data, diasend.insulin.data, by.x="date", by.y="date",all=TRUE)
	
	# and save the output
	print("Saving CSV file...")
	write.csv(out.data, file=output.filename, row.names=FALSE)
	#out.data
}

#
# Function to load the above data back in again (note, we have to specify the class of the 
# date column otherwise it will be loaded as a Factor not a date
#
load.data <- function(input.filename)
{
	read.csv(input.filename,colClasses=c("Date", "double", "double", "double"))
}