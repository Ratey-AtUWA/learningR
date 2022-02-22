hist(afs20$Al)

varnames <- names(afs20[,1:38])
varnames

par(mfrow = c(4,4), mar = c(3,3,1,1), mgp = c(1.7, 0.3, 0),
    tcl = 0.25, font.lab = 2)
library(car)
for(i in 25:38){
  qqPlot(log10(afs20[,i]), ylab=NA, main=NA,
         lwd = 1, cex = 1.3)
  mtext(paste("log",varnames[i]), side = 2, 
        line = 1.7, font=2)
}

library(DataExplorer)
par(mfrow=c(1,1))
plot_histogram(afs20[,9:38])
plot_qq(afs20[,9:38])

shapiro.test(afs20$Al)
