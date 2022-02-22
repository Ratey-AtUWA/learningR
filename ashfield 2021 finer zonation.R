require(OpenStreetMap)
require(prettymapr)
# styles are: mapbox://styles/mapbox/streets-v11
# mapbox://styles/mapbox/outdoors-v11
# mapbox://styles/mapbox/light-v10
# mapbox://styles/mapbox/dark-v10
# mapbox://styles/mapbox/satellite-v9
# mapbox://styles/mapbox/satellite-streets-v11
# make map ####
# apiKey <- paste0("?access_token=",
#                  "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
# baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v11/tiles/256/{z}/{x}/{y}"
# afr.osm <- openmap(upperLeft = c(-31.916,115.941),
#                      lowerRight = c(-31.9205, 115.949),
#                      type=paste0(baseUrl,apiKey),
#                      zoom=17.5)
# 
# afr2.utm <- openproj(afr.osm,
#                      projection = "+proj=utm +zone=50 +south")
# rm(afr.osm)
plot(afr2.utm, removeMargin = F)
axis(1);axis(2)
mtext("Easting (UTM Zone 50, m)", side=1, 
      line=1.6, cex=1.2, font=2) 
mtext("Northing (UTM Zone 50, m)", side=2, 
      line=1.6, cex=1.2, font=2) 
with(afs21, points(Northing ~ Easting, asp = 1, cex = 1.2, 
                 bg = seq(3,7)[Zone2], 
                 pch = seq(21,25)[Zone2]))
legend("bottomright", legend = levels(afs21$Zone2),
       bty = "n", inset = 0.03, pt.cex = 1.2,
       text.col = "white", cex = 1.2, 
       pt.bg = seq(3,7), 
       pch = seq(21,25))
# with(afs21, text(Northing ~ Easting, labels = Zone2, pos = 4, col = seq(3,7)[Zone2]))
# with(afs21, text(Northing ~ Easting, labels = str_sub(Field_ID, 8), pos = 4))
