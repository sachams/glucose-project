library(ggplot2)
library(scales)

ggplot() + 
geom_step(data=data, aes(x=date, y=total.basal)) +
geom_bar(stat="identity", data=data, aes(x=date, y=trimp.exp)) + 
xlab("Date") + ylab("Total Basal (units)") + 
scale_y_continuous(limits=c(12,24), breaks=seq(12,24,2)) + scale_x_date(limits=c(as.Date("2014-01-01"), as.Date("2014-05-01")), breaks=seq(as.Date("2014-01-01"), as.Date("2014-05-01"), by="2 week"), labels=date_format("%d-%b"))