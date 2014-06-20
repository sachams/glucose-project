library(ggplot2)
library(scales)
library(plyr)
library(reshape2)

plot.it = function(data, startstr, endstr, markers=NULL) {
	
	start = as.Date(startstr)
	end = as.Date(endstr)
	
	trimmed.data = data[(data$date >= start) & (data$date <= end),]

	f = ggplot(trimmed.data, aes(x=date,y=value)) + facet_grid(variable~., scales='free') +
			geom_step(subset=.(variable=='total.basal')) +
			geom_bar(stat="identity", subset=.(variable=='trimp.exp')) +
			xlab("Date") +
			scale_x_date(limits=c(start, end), breaks=seq(start, end, by="2 week"), labels=date_format("%d-%b")) + ylab('') 

	if(!is.null(markers))
	{
		f = f + geom_vline(xintercept=markers, color='red',alpha=.3)
	}

	f
}
