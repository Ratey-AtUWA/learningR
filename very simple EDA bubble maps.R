afw21 <- read.csv("afw21.csv", stringsAsFactors=TRUE)
afs21 <- read.csv("afs21.csv", stringsAsFactors=TRUE)
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.3)
with(afw21,
     plot(Easting, Northing, pch = 3, asp =1))
with(afw21, 
     symbols(Easting, Northing, add=TRUE,
             circles = sqrt(NOx.N), inches = 0.25,
             fg = "darkcyan", bg = "#00808080"))
mtext("NOx.N concentration",3,-1.2,font=2)
mtext(paste0("min = ",
             min(afw21$NOx.N, na.rm=T),
             ", max = ",
             max(afw21$NOx.N, na.rm=T),
             " mg/L"),3,-2.2, cex = 0.8)
# ________________________________

with(afs21,
     plot(Easting, Northing, pch = 3, asp =1))
with(afs21, 
     symbols(Easting, Northing, add=TRUE,
             circles = sqrt(P), inches = 0.2,
             fg = "purple", bg = "#8000C080"))
mtext("Sediment P concentration",3,-1.2,font=2)
mtext(paste0("min = ",
            min(afs21$P, na.rm=T),
            ", max = ",
            max(afs21$P, na.rm=T),
            " mg/kg"),3,-2.2, cex = 0.8)
