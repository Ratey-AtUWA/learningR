library(OpenStreetMap)
library(stringi)
library(stringr)
afs21 <- read.csv(file = "afs21.csv", stringsAsFactors = T)
afw21 <- read.csv(file = "afw21.csv", stringsAsFactors = T)
afs21$uID <- paste0("EA",
                str_trunc(as.character(afs21$Sample.ID),
                1, side = "left", ellipsis = ""))
afs21$uID[34:40] <- paste0("EA",
         str_trunc(as.character(afs21$Sample.ID[34:40]),
                   2, side = "left", ellipsis = ""))
afs21$uID[41:66] <- paste0("AEQ",
         str_trunc(as.character(afs21$Sample.ID[41:66]),
                   1, side = "left", ellipsis = ""))

palette(c("black", rainbow(12, v = 0.75, end = 0.7)))
with(afs21,
     plot(Northing ~ Easting, asp = 1,
          ylim = c(6467960,6468400),
          pch = c(0:9,15,17,19)[Sample.ID],
          col = seq(1,13)[Sample.ID])
)
text(afs21$Easting, afs21$Northing, 
     labels = afs21$uID,
     cex = 0.7, pos = 1, offset = 0.2)

library(OpenStreetMap)
par(mar=c(3,3,1,1),mgp=c(1.7,0.3,0),tcl=0.3,font.lab=2,
    lend="square",ljoin="mitre")
plot(afr.utm, removeMargin = F)
with(afs19, points(Easting, Northing, pch=1, col = 1,
                   cex = 1.2, lwd = 2))
with(afs20, points(Easting, Northing, pch=3, lwd=2, 
                   col = 2, cex=0.85))
with(afs21, points(Easting, Northing, pch=17, col = 11))
axis(1)
axis(2)
box()
mtext("Easting (m, UTM Zone 50)",1,1.7,font=2)
mtext("Northing (m, UTM Zone 50)",2,1.7,font=2)
mtext("Sediment Samples",3,-1.2, cex = 1.2, font=2)
legend("topleft", inset=0.02,
       title = "Sampling Year",
       legend = seq(2019,2021), col=c(1,2,11),
       pch = c(1,3,17), pt.cex = c(1.2,0.85,1),
       pt.lwd = c(2,2,1))

plot(afr.utm, removeMargin = F)
with(afw19, points(Easting, Northing, pch=1, col = 1,
                   cex = 1.2, lwd = 2))
with(afw21, points(Easting, Northing, pch=15, 
                   col = 11, cex = 1.2))
with(afw20, points(Easting, Northing, pch=3, lwd=2, 
                   col = 2, cex=0.85))
axis(1)
axis(2)
box()
mtext("Easting (m, UTM Zone 50)",1,1.7,font=2)
mtext("Northing (m, UTM Zone 50)",2,1.7,font=2)
mtext("Water Samples",3,-1.2, cex = 1.2, font=2)
legend("topleft", inset=0.02,
       title = "Sampling Year",
       legend = seq(2019,2021), col=c(1,2,11),
       pch = c(1,3,15), pt.cex = c(1.2,0.85,1.2),
       pt.lwd = c(2,2,1))
