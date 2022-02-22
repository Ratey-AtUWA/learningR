afr_map <- read.csv("ashfield_map.csv")
require(prettymapr)
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square")
# change values of xlim, ylim to suit desired map area
with(afr_map,
     plot(street_E, street_N, asp=1,
          type = "l", lwd = 6, col = "grey90",
          xlab = "", ylab = "", 
          xlim=c(399910,400670), 
          ylim=c(6467940,6468540)))
mtext("Easting (UTM Zone 50, m)", side=1, 
      line=1.6, cex=1.2, font=2) 
mtext("Northing (UTM Zone 50, m)", side=2, 
      line=1.6, cex=1.2, font=2) 
with(afr_map, 
     polygon(veg_E, veg_N, 
             border="darkseagreen",
             col = "darkseagreen", 
             lwd=1))
with(afr_map, 
       lines(bound_E, bound_N, type="l", 
            ylim=c(6467650,6468550),
            col = "darkseagreen", 
            lty = 5, lwd = 2))
with(afr_map, lines(drain_E, drain_N, 
                   col="royalblue4", lwd=2))
# with(drainE, lines(Easting,Northing, 
#                    col="royalblue4", lwd=2))
with(afr_map, 
     polygon(wetland_E, wetland_N, 
           border="darkcyan",
           col = "lightblue2", 
           lwd=1))
with(afr_map,
     polygon(swan_E, swan_N, 
             border="transparent",
             col = "#00008040", 
             lwd=1)
)
with(afr_map, lines(path_E, path_N, 
                    col="ivory4", lwd=2, lty=3)
     )
text(400200, 6467820, labels = "Swan River", 
     col = "royalblue4", font = 3, cex = 1.2, 
     srt = 330)
text(400200, 6468135, 
     labels = "Chapman St Drain", 
     col = "royalblue4", srt = 45)
text(399970, 6468100, 
     labels = "Kitchener St Drain", 
     col = "royalblue4", srt = 300)
text(399900, 6468230, 
     labels = "Hardy Road", 
     col = "grey65", srt = 45)
text(400540, 6468490, 
     labels = "Iveson Place", 
     col = "grey65", srt = 50)
text(400450, 6468180, 
     labels = "Ashfield Flats\nReserve", 
     col = "darkolivegreen", font = 3, cex = 1.2)
addnortharrow()
addscalebar(plotepsg=32750, htin = 0.15, 
            label.cex = 1.2)
box()
legend("topleft",
       legend = c("Native vegetation",
                  "Seasonal wetland ponds",
                  "Reserve boundary",
                  "Stormwater drains",
                  "Footpaths",
                  "Roads"),
       cex = 1.2, pch = c(22, 21, NA, NA, NA< NA),
       pt.cex = c(3, 3.6, NA, NA, NA, NA), pt.lwd = 2,
       col = c("darkseagreen", "darkcyan",
               "darkseagreen", "royalblue4",
               "ivory4", "gray90"),
       pt.bg = c("darkseagreen", "lightblue2",
              NA, NA),
       lwd = c(NA, NA, 2, 2, 2, 6),
       lty = c(NA, NA, 5, 1, 3, 1),
       bty = "n", inset = 0.01,
       y.intersp = 1.2)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
ff <- 25 # ======================================
with(afw20,
     symbols(Easting, Northing,
             circles = sqrt(PO4.P) * ff,
             fg = "#B00000C0", bg = "#B0000040",
             add = TRUE, inches = FALSE, lwd = 1)
)
concs <- pretty(afw20$PO4.P, h=5)
i <- NROW(concs)
rect(400290,6467960,400410,6468100,
     col = "#FFFFFF40", border = "transparent")
with(afw20,
     symbols(c(400325, 400325), 
             c(6468050, 6468000),
             circles = c(ff*sqrt(concs[2]),
                         ff*sqrt(concs[i])),
             fg = "#B00000C0", bg = "#B0000040",
             add = TRUE, inches = FALSE, lwd = 1)
)
text(c(400350, 400350, 400350), 
     c(6468100, 6468050, 6468000), 
     labels = c("PO4.P (mg/L)", concs[2], concs[i]), 
     cex = 1, col = 1, pos = c(1,4,4))
