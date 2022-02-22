# Map using base R and coordinate data ####
afs21TEMP <- read.table("clipboard", header = T, 
                        sep = "\t", stringsAsFactors = T)
afw21TEMP$Group <- factor(afw21TEMP$Group, 
                          levels = c("ENVT3361.1","ENVT3361.2","ENVT3361.3",
                                     "ENVT3361.4","ENVT3361.5","ENVT3361.6",
                                     "ENVT3361.7","ENVT3361.8","ENVT3361.9",
                                     "ENVT3361.10",
                                     "ENVT4461.1","ENVT4461.2","ENVT4461.3"))
afr_map <- read.csv("ashfield_map.csv")
require(prettymapr)
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square")
palette(c("black", rainbow(6, v = 0.75, end = 0.1), 
          rainbow(6, v = 0.75, start = 0.5, end = 0.8)))
# change values of xlim, ylim to suit desired map area
with(afr_map, plot(street_E, street_N, asp=1,
                   type = "l", lwd = 6, col = "grey90",
                   xlab = "", ylab = "", 
                   xlim=c(399910,400670), 
                   ylim=c(6467940,6468540)))
mtext("Easting (UTM Zone 50, m)", side=1, 
      line=1.6, cex=1.2, font=2) 
mtext("Northing (UTM Zone 50, m)", side=2, 
      line=1.6, cex=1.2, font=2) 
with(afr_map, polygon(veg_E, veg_N, 
                      border="darkseagreen",
                      col = "darkseagreen", 
                      lwd=1))
with(afr_map, lines(bound_E, bound_N, type="l", 
                    col = "darkseagreen", 
                    lty = 5, lwd = 2))
with(afr_map, lines(drain_E, drain_N, 
                    col="royalblue4", lwd=2))
with(afr_map, polygon(wetland_E, wetland_N, 
                      border="darkcyan",
                      col = "lightblue2", lwd=1))
with(afr_map, polygon(swan_E, swan_N, 
                      border="transparent",
                      col = "#00008040", lwd=1))
with(afr_map, lines(path_E, path_N, 
                    col="ivory4", lwd=2, lty=3))
text(400200, 6467820, labels = "Swan River", 
     col = "royalblue4", font = 3, cex = 1.2, 
     srt = 330)
text(400200, 6468135, labels = "Chapman St Drain", 
     col = "royalblue4", srt = 45)
text(399970, 6468100, labels = "Kitchener St Drain", 
     col = "royalblue4", srt = 300)
text(399900, 6468230, labels = "Hardy Road", 
     col = "grey65", srt = 45)
text(400540, 6468490, labels = "Iveson Place", 
     col = "grey65", srt = 50)
text(400450, 6468180, labels = "Ashfield Flats\nReserve", 
     col = "darkolivegreen", font = 3, cex = 1.2)
addnortharrow(padin = c(0.2,2))
addscalebar(plotepsg=32750, htin = 0.15, label.cex = 1.2)
box()

with(afs21TEMP, 
     points(Easting, Northingcex = 1.4)
     )
mtext("Ashfield Sed 2021\n(Google)", 3, -2, font=2)
legend("bottomright", inset = 0.02,
       legend = levels(afw21TEMP$Group),
       pch = c(0:6,10,12,15,17:19),
       col = seq(1,13),
       pt.cex = 1.4, pt.lwd = 2)
