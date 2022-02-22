require(stringr)
require(stringi)
require(prettymapr)
par(mar =c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25, 
    font.lab = 2, ljoin = "mitre", lend = "square")
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
addnortharrow("topleft",padin = c(0.2,0.2))
addscalebar(plotepsg=32750, htin = 0.15, label.cex = 1.2, widthhint = 0.35)
box()
palette(c("black",colorRampPalette(c("brown4","red3","blue3"))(13)))
with(afw21TEMP, 
     points(Easting, Northing, cex = 1.2, 
            col = seq(2,14)[Group], 
            pch = c(10:19,21,22,24)[Group],
            lwd = c(1,1,1,2,2,1,1,1,1,1,2,2,2)[Group], 
            bg = "white"))
with(afw21TEMP[49:53,], 
     text(Easting, Northing, 
          labels=str_sub(afw21TEMP[49:53,]$Sample.ID,6), 
          cex = .7, pos = 2, offset=0.2, col = "red2"))
with(afw21TEMP[c(1:48,54:70),], 
     text(Easting, Northing, 
          labels=str_sub(afw21TEMP[c(1:48,54:70),]$Sample.ID,6), 
          cex = .7, pos = 4, offset=0.2, col = "blue2"))
legend("bottomright", bty="o", inset = 0.01,
       title = expression(italic("Group")), ncol = 2,
       legend = levels(afw21TEMP$Group),
       pch = c(10:19,21,22,24), col = seq(2,14),
       pt.lwd=c(1,1,1,2,2,1,1,1,1,1,2,2,2))
