library(sp)
library(rgdal)
# define common coordinate reference systems (CRS)
webMercator <- 
  CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")
UTM_Zone50S <- CRS("+proj=utm +zone=50 +south")
LongLat <- CRS("+proj=longlat +datum=WGS84 +no_defs") 
# LongLat <- CRS("+init=epsg:4326") # alternative

# make a spatial object from our UTM coordinates
TEMP.utm <- 
  SpatialPoints(cbind(afw20[,c("Easting","Northing")]),
                proj4string = UTM_Zone50S)
# do the conversion to desired CRS
TEMP.LL <- spTransform(TEMP.utm, LongLat)
afw20$Longitude <- TEMP.LL@coords[,1]
afw20$Latitude <- TEMP.LL@coords[,2]
