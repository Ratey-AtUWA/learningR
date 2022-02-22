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
text(399920, 6468250, labels = "Hardy Road", 
     col = "grey65", srt = 45)
text(399960, 6468340, labels = "Woolcock\nCourt", 
     col = "grey65", srt = 315)
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
mtext("Microplastics\n(blank corrected)", 3, -2, font=2)
sf = 0.3
with(afs21TEMP, 
     symbols(Easting, Northing, add=T,
             squares = sqrt(microplastics_fibres)*sf,
             inches = F, fg = "sienna", bg = "#D0704080"))
with(afs21TEMP, 
     symbols(Easting, Northing, add=T,
             circles = sqrt(microplastics_Other)*sf,
             inches = F, fg = "darkcyan", bg = "#00B0B060"))
with(afs21TEMP, 
     symbols(Easting, Northing, add=T,
             circles = sqrt(microbeads)*sf,
             inches = F, fg = "gold", bg = "#FFD70060"))
symbols(rep(399950,2), seq(6468600,6468550,-50), add=T,
        circles = sqrt(c(1000,10000))*sf,
        inches = F, fg = "gold", bg = "#FFD70060")
text(rep(399950,2), seq(6468600,6468550,-50), 
     labels = c("1000/kg microbeads","10,000/kg microbeads"),
     pos = 4, offset = 1.4)
symbols(rep(399950,2), seq(6468500,6468450,-50), add=T,
        squares = sqrt(c(2000,20000))*sf,
        inches = F, fg = "sienna", 
        bg = "#D0704080")
text(rep(399950,2), seq(6468500,6468450,-50), 
     labels = c("2000/kg microfibres","20,000/kg microfibres"),
     pos = 4, offset = 1.4)
names(afs21TEMP)[1:20]
