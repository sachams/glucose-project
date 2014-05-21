x = trimmed$date
insulin = trimmed$total.basal
exercise = trimmed$trimp.duration


## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)
 
## Plot basal data and draw its axis
plot(x, insulin, pch=16, axes=FALSE, xlab="", ylab="", type="s",col="black", main="Exercise and total basal vs time")
axis(2, col="black",las=1)  ## las=1 makes horizontal labels
mtext("Total daily basal (units)",side=2,line=2.5)
box()
 
## Allow a second plot on the same graph
par(new=TRUE)
 
## Plot the second plot and put axis scale on right
plot(x, exercise, pch=15,  xlab="", ylab="", ylim=c(0,300), lwd=5, lend="square", axes=FALSE, type="h", col="red")

## a little farther out (line=3) to make room for labels
mtext("Total daily exercise (minutes)",side=4,col="black",line=3) 
axis(4, ylim=c(0,300), col="black",col.axis="black",las=1)
 
## Draw the time axis
mtext("Date",side=1,col="black",line=2.5)  
axis.Date(1, x, at=seq(x[1],x[length(x)], by="weeks"), labels=seq(x[1], x[length(x)], by="weeks"), cex.axis=0.7)

## Add vertical grid lines
abline(v=seq(x[1], x[length(x)], by="weeks"), col="gray", lty=3)

## Add Legend
legend("topleft", legend=c("Insulin", "Exercise"), text.col=c("black", "red"), lty=c(1,1), col=c("black", "red"), cex=0.5)
