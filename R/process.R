###############################################################################
#
# process.R
# Top-level processing script for exercise and insulin data.
#
# Author: Sacha Manson-Smith
#
###############################################################################

library(ggplot2)
library(scales) 
library(reshape2)
library(plyr)
library(gridExtra)

source('./R/lib/strava.R')
source('./R/lib/diasend.R')
source('./R/lib/bayer.R')
source('./R/trimp.R')
source('./R/insulin.R')
# source('./R/clientdetails.R')


hr.rest <- 60
hr.max <- 180

GoNoSync <- function()
{
  LoadAllData('~/Dropbox/Strava Data','~/Dropbox/Diasend Data', sync.data=FALSE)
}

LoadAllData <- function(strava.directory, diasend.directory, strava.client.id=NULL, strava.client.secret=NULL, sync.data=TRUE, skip.existing=TRUE) {
  
  # Process latest .xls Diasend files
  if(sync.data) {
    DiasendProcessWorkbooks(diasend.directory)
  }
  
  # Load CGM and insulin data
  diasend.data <- DiasendLoadData(diasend.directory)
  
  if(sync.data) {
    # Authenticate on Strava
    strava.token <- StravaAuthenticate(strava.client.id, strava.client.secret)
    
    # Sync latest activities from Strava
    StravaSyncActivities(strava.token, strava.directory, skip.existing=skip.existing)
  }
  
  # And load them in
  strava.data <- StravaLoadActivities(strava.directory)
  
  # Calculate TRIMP data from the strava data
  trimp.data <- CalculateTrimpData(strava.data, hr.rest, hr.max)

  # Rebucket it into smaller windows
  bucket.width <- 300 # 5 minute windows
  trimp.data <- RebucketTrimpData(trimp.data, bucket.width) 
  
  # Bind together the TRIMP data, adding in a single record at resting heartrate between TRIMP records.
  # This is to indicate that between the periods of exercise, heart rate is at rest.
  # This may not be true, but we don't have any other data and as HR measurements are only done during
  # periods of exercise, this is a reasonable assumption.
  
  trimp.data <- CollateTrimpData(trimp.data, bucket.width, hr.rest, hr.max)  

  
  # Load HBa1c data and calculate rolling BG averages
  hba1c.data <- LoadHba1c('~/Dropbox/Other Diabetes Data/hba1c.csv')
  
  # Load glucose meter data
  meter.data <- LoadBayer("~/Dropbox/Glucose Meter/bayer_20140707.csv")
  
  list(trimp.data=trimp.data, 
       cgm.data=diasend.data$cgm.data, 
       basal.data=diasend.data$basal.data, 
       bolusandcarbs.data=diasend.data$bolusandcarbs.data,
       cgmchange.data=diasend.data$cgmchange.data,
       fillcannula.data=diasend.data$fillcannula.data,
       hba1c.data=hba1c.data,
       meter.data=meter.data)
  

}



MergeData <- function(data){
  
  # Merge the data together, keyed on time (strip out a few columns first and remove empty rows)
  data$insulin.data$notes <- NULL
  data$insulin.data$bolus <- NULL
  data$insulin.data$bolus.type <- NULL
  data$insulin.data$bolus.immediate <- NULL
  data$insulin.data$bolus.extended <- NULL
  data$insulin.data$bolus.duration <- NULL
  data$insulin.data$carbs <- NULL
  data$insulin.data <- data$insulin.data[!is.na(data$insulin.data$basal),]

  # We have to conver to POSIXct to merge based on key. This is a bit rubbish.
  data$insulin.data$time <- as.POSIXct(data$insulin.data$time)
  data$cgm.data$time <- as.POSIXct(data$cgm.data$time)
  data$trimp.data$time <- as.POSIXct(data$trimp.data$time)
  
  # Now merge all the data together and sort it
  merged.data <- merge(data$insulin.data, data$cgm.data, by.x='time', by.y='time', all=TRUE)
  merged.data <- merge(merged.data, data$trimp.data, by.x='time', by.y='time', all=TRUE)
  merged.data <- merged.data[order(merged.data$time),]
  
  # get rid of any rows where all data is NA
  merged.data <- merged.data[!is.na(merged.data$mmol.l) | !is.na(merged.data$basal) | !is.na(merged.data$trimp.exp) | !is.na(merged.data$trimp.duration),]
  merged.data
}

PlotMergedData <- function(merged.data, use.facets=TRUE, start.date=NULL, end.date=NULL){
  # get rid of any unused rows and then rows where all data is NA
  merged.data$mmol.l <- NULL
  merged.data$trimp.exp <- NULL
  merged.data <- merged.data[!is.na(merged.data$basal) | !is.na(merged.data$trimp.duration),]

  # If the start or end date is not null, trim the data accordingly
  if(is.null(start.date)) {
    start.date <- head(merged.data$time,1)
  } else {
    start.date <- as.POSIXct(strptime(start.date, '%Y-%m-%d', tz='GMT'))
    merged.data <- merged.data[merged.data$time >= start.date,]
  }
  
  if(is.null(end.date)) {
    end.date <- tail(merged.data$time, 1)
  } else {
    end.date <- as.POSIXct(strptime(end.date, '%Y-%m-%d', tz='GMT'))
    merged.data <- merged.data[merged.data$time <= end.date,]
  }
  
  # If using facets, we have to convert the data to long format
  
  if(use.facets) {
    melted.data <- melt(merged.data, measure.var=c('trimp.duration', 'basal'))

    # After melting we may have rows where value is NA - delete them
    melted.data <- melted.data[!is.na(melted.data$value),]
    
    # Now plow the data
    f = ggplot(melted.data, aes(x=time,y=value)) + 
      facet_grid(variable~., scales='free') +
      geom_step(stat="identity", subset=.(variable=='basal')) +
      geom_bar(stat="identity", subset=.(variable=='trimp.duration')) +
      xlab("Date") +
      scale_x_datetime(limits=c(start.date, end.date), 
                       breaks=seq(start.date, end.date, by="2 week"), 
                       labels=date_format("%d-%b")) + 
      ylab('') 
    
    #if(!is.null(markers))
    #{
    #  f = f + geom_vline(xintercept=markers, color='red',alpha=.3)
    #}
    
    f
  }
  else {
    p1 <- ggplot(merged.data, aes(x=time, y=trimp.duration)) + 
      geom_bar(stat='identity', colour='black') +
      scale_y_continuous(limits=c(0,500)) +
      scale_x_datetime(limits=c(start.date, end.date))
      
    p2 <- ggplot(merged.data, aes(x=time, y=basal)) + 
      geom_step() +
    scale_x_datetime(limits=c(start.date, end.date))
    
    grid.arrange(p1,p2)  
  }
}

