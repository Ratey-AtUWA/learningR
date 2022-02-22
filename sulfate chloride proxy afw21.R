par(mar = c(3,8,1,1), las = 1)
boxplot(((afw21$S*(96/32))/(afw21$Na*1.799)), 
        horizontal = T, ylab="", 
        col = "moccasin", cex.axis = 1.2)
abline(v=0.14, col = 12, lty = 2, lwd = 1)
text(0.14 ,0.6, pos=4, col = 12,
     labels="Mean seawater\nSO4/Cl mass ratio = 0.14")
abline(v=0.5, col = 2, lty = 2, lwd = 2)
text(0.5 ,1.4, pos=4, col = 2,
     labels="Acid sulfate trigger\nSO4/Cl mass ratio = 0.5")
par(mar = c(3,3,1,1), las = 1)

which(((afw21$S*(96/32))/(afw21$Na*1.799))>0.5)
which(((afw21$S*(96/32))/(afw21$Na*1.799))>0.14)
