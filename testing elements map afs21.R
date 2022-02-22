require(prettymapr)
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25,
    lend = "square", ljoin = "mitre", font.lab = 2)
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
     points(Easting, Northing, pch =3, cex = .8)
)
mtext("Ashfield Sed 2021 (shared class Google Sheet)", 3, .1, cex=0.7)
mtext("As", 3, -2, font=2)
bub0 <- pretty(afs21TEMP$As)[2]
bub1 <- pretty(afs21TEMP$As)[length(pretty(afs21TEMP$As))]
xrng <- par("usr")[2]-par("usr")[1]
sf = (0.04*xrng)/sqrt(bub1) 
# the 0.04 above depends on desired largest bubble size
# 0.04 gives largest bubble ~0.1 * x axis range
with(afs21TEMP, 
     symbols(Easting, Northing, add=T,
             circles = sqrt(As)*sf,
             inches = F, fg = "purple", bg = "#D000FF40"))
symbols(rep(399950,2), c(6468580,6468500), add=T,
        circles = sqrt(c(bub0,bub1))*sf,
        inches = F, fg = "purple", bg = "#D000FF40")
text(rep(399950,2), c(6468580,6468500), 
     labels = c(paste(bub0,"mg As/kg"),paste(bub1,"mg As/kg")),
     pos = 4, offset = 2)
names(afs21TEMP)[10:43]

