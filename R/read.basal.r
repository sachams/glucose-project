read.basal = function(filename) {
	setClass("DiasendDateTime")
	setAs("character","DiasendDateTime", function(from) strptime(from,"%d/%m/%Y %H:%M") )
	data = read.csv(filename, colClasses=c('DiasendDateTime','double'))[,1:2]
  names(data)[2] = "Basal"
  
  # Remove any blank lines due to data in other columns which weren't imported
  data = data[!is.na(data$Basal),]

  # Remove any zeros - this is when the basal profile is being edited. Basal stops.
  data = data[data$Basal != 0,]
 }
 
 process.basal = function(data, profile)
 {
 	# Subtract difference between points in profile from corresponding points in data
 	
 
 }