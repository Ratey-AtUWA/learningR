par(mar = c(3,3,1,1), mgp = c(1.6,0.3,0), tcl = 0.3,
    lend = "square", ljoin = "mitre")
plot(afr.utm, removeMargin = FALSE)
lines(afr_map$bound_E, afr_map$bound_N, col = "olivedrab3")
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2)
addnortharrow()
addscalebar(plotepsg = 32750, htin = 0.15, label.cex = 1.4)
box()

cuts5 <- signif(fivenum(afs20$Zn.log, na.rm = TRUE),4)
cuts5[5] <- cuts5[4]+(1.5*(cuts5[4]-cuts5[2]))
cuts5[1] <- cuts5[2]-(1.5*(cuts5[4]-cuts5[2]))
cuts5 <- signif(10^cuts5, 3)
palette(c("black","purple","blue3","darkolivegreen4",
          "goldenrod3","tomato","red2","white"))
sf <- 0.85
cex0 <- c(0.85,0.9,1.05,1.2,1.4,1.8)*sf
points(afs20$Northing ~ afs20$Easting, pch=15, col=2, cex=cex0[1], lwd=2,
       subset=afs20$Zn<cuts5[1])
points(afs20$Northing ~ afs20$Easting, pch=0, col=3, cex=cex0[2], lwd=2,
       subset=afs20$Zn>cuts5[1]&afs20$Zn<cuts5[2])
points(afs20$Northing ~ afs20$Easting, pch=3, col=4, cex=cex0[3], lwd=2,
       subset=afs20$Zn>cuts5[2]&afs20$Zn<cuts5[3])
points(afs20$Northing ~ afs20$Easting, pch=4, col=5, cex=cex0[4], lwd=2,
       subset=afs20$Zn>cuts5[3]&afs20$Zn<cuts5[4])
points(afs20$Northing ~ afs20$Easting, pch=1, col=6, cex=cex0[5], lwd=2,
       subset=afs20$Zn>cuts5[4]&afs20$Zn<cuts5[5])
points(afs20$Northing ~ afs20$Easting, pch=16, col=7, cex=cex0[6], lwd=2,
       subset=afs20$Zn>cuts5[5])
min0 <- signif(min(afs20$Zn, na.rm=T),4)
max0 <- signif(max(afs20$Zn, na.rm=T),4)
legend("topleft", bty="o", pch=c(15,0,3,4,1,19), inset=0.025, 
       cex=1 * sf, pt.cex=cex0, pt.lwd=2, y.intersp=1.25, 
       title="Zn by Tukey boxplot thresholds (range):", 
       legend=c(paste0("Lower outliers (",min0,"-",cuts5[1]," mg/kg)"),
                paste0("Below Q1 (",cuts5[1],"-",cuts5[2]," mg/kg)"),
                paste0("Q1 to median (",cuts5[2],"-",cuts5[3]," mg/kg)"),
                paste0("Median to Q3 (",cuts5[3],"-",cuts5[4]," mg/kg)"),
                paste0("Above Q3 (",cuts5[4],"-",cuts5[5]," mg/kg)"),
                paste0("Upper outliers (",cuts5[5],"-",max0," mg/kg)")), 
       col=seq(2,7))
rm(list = c("sf","cex0","cuts5","min0","max0"))
