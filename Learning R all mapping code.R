#    __  __                           _             _____  
#   |  \/  |                         (_)           |  __ \ 
#   | \  / |   __ _   _ __    ___     _   _ __     | |__) |
#   | |\/| |  / _` | | '_ \  / __|   | | | '_ \    |  _  / 
#   | |  | | | (_| | | |_) | \__ \   | | | | | |   | | \ \ 
#   |_|  |_|  \__,_| | .__/  |___/   |_| |_| |_|   |_|  \_\
#                    | |                                   
#                    |_|                                   
#
# load packages and any data we'll need for later
library(OpenStreetMap)
library(prettymapr)
library(rosm)
library(sp)
library(ggmap)

# Use OpenStreetMap to make an initial map object ####
#
# We use the OpenStreetMap R package (Fellows, 2019) to make a plot-able map
# object, based on the coordinates which bound a rectangular area.
#
# The easiest way to get bounding coordinates (latitude, longitude) is by using
# Google Earth or Google Maps. We need the north-west (upper left) and
# south-east (lower right) coordinates of the rectangular map we want to plot.
#
# Note that south latitudes (and west longitudes) are negative, and we want
# decimal degrees rather than degrees-minutes-seconds.

require(OpenStreetMap)
UWA_osm <- openmap(upperLeft = c(-31.974, 115.812), 
                   lowerRight = c(-31.988, 115.828), 
                   zoom = 16,
                   type = "osm")
plot(UWA_osm)

# The map needs some attention. Without some prior knowledge, 
# we don't know where on Earth's surface this map represents, we
# don't know the direction it's oriented in, and we don't know the scale.
# We can add a north arrow to indicate direction, a scale bar to show the
# scale, and axes to show any coordinates which should show the location
# on Earth.

## Plot the UWA campus map with margins, axes, and annotations
require(OpenStreetMap)
require(prettymapr)
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_osm, removeMargin = FALSE)
axis(1); axis(2)
addnortharrow()
addscalebar(plotepsg = 3857) # epsg is for web mercator or "Google mercator"
box()

# The map is now better -- we now know scale and direction,
# but finding our location on Earth is tricky because the OpenStreetMap map
# projection is "Web Mercator" which is not a projection most people are
# familiar with. We need a map in a commonly-used projection such as
# Longitude-Latitude, or Universal Transverse Mercator (UTM). We can change
# projections using the openproj() function in the OpenStreetMap
# package.

## Plot a re-projected UWA campus map in UTM with margins, axes, and annotations

### First convert the projection...

require(OpenStreetMap)
UWA_utm <- openproj(UWA_osm, projection = "+proj=utm +zone=50 +south")
cat("Show converted upper left (p1) and lower right (p2) coordinates\n")
UWA_utm$bbox # show bounding coordinates to check

### ...then plot the map in its new projection.

# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1.2)
addscalebar(plotepsg = 32750, htin = 0.15, label.cex = 1.2)
box()

# The map is now just what we need -- we know scale and
# direction, and we have axes in the Universal Transverse Mercator (UTM)
# projection with units of metres, which we would normally set up our
# field GPS to show.
#
# Very often we would like to add our own information to a map, such as
# the location of samples, often with different sizes or shapes of symbols to
# represent numerical information.

## Making some data to plot on our map

Longitude <-  c(115.8211, 115.8186, 115.8186, 115.8189, 115.8178, 115.8183, 
                115.8209, 115.8204, 115.8199, 115.8194, 115.8198, 115.8181)
Latitude <- c(-31.9822, -31.98303, -31.98265, -31.98109, -31.979, -31.9765, 
              -31.986, -31.97688, -31.97971, -31.97638, -31.97834, -31.97872)
Easting <- c(388624.5, 388387.9, 388383.5, 388415.6, 388308.3, 388354.6, 
             388603.9, 388551.3, 388501.3, 388452.6, 388493.5, 388334)
Northing <- c(6460930, 6460836, 6460878, 6461051, 6461282, 6461559, 
              6460509, 6461519, 6461205, 6461574, 6461357, 6461313)
Name <- c("Matilda Bay", "Barry Marshall", "Science", "Guild", "Reid", "Hackett", 
          "Business", "Music", "Law", "Rec Centre", "UniClub", "Reid")
Type <- c("Cafe", "Library", "Cafe", "Cafe", "Library", "Cafe", 
          "Cafe", "Library", "Library", "Cafe", "Cafe", "Cafe")
Usage <- c(10,10,8,4,2,7,1,0.1,0.1,1,2,4)
places <- as.data.frame(cbind(Longitude, Latitude, Easting, Northing, Usage))
places$Name <- Name
places$Type <- as.factor(Type)
# str(places)
rm(list = c("Longitude", "Latitude", "Easting", "Northing", 
            "Name", "Type", "Usage"))
print(places)

## Plot simple data on our best map

# An OpenStreetMap plot is essentially an x-y plot with a map image as the plot
# background. We can use standard R graphics functions to add points, lines,
# polygons, and text (including legends) to the map.

require(OpenStreetMap)
require(prettymapr)

# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1.2)
addscalebar(plotepsg = 32750, widthhint = 0.2, htin = 0.1, label.cex = 1.2)
box()
#
# plotting our data
points(places$Easting, places$Northing,
       pch = c(15, 19)[places$Type],
       col = c("red3", "blue2")[places$Type],
       cex = c(1.3, 1.5)[places$Type])
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type),
     cex = 0.8,
     pos = c(4,2,4,4,2,2,4,4,4,4,4,4),
     col = c("red3","blue2","red3","red3","blue2","red3",
               "red3","blue2","blue2","red3","red3","red3"))
legend("right",
       legend = levels(places$Type),
       pch = c(15, 19),
       col = c("red3", "blue2"),
       pt.cex = c(1.3, 1.5),
       cex = 0.9,
       inset = 0.03)

## Plot numerical data on our best map One way of representing quantities at
# different spatial locations is to use a "bubble plot", using the
# symbols() function in R. We calculate the square root of the
# quantity being represented (see code below), to make our circle area
# proportional to quantity -- area is more intuitive for human perception of
# amounts than are radius or diameter (bubOSM).

require(OpenStreetMap)
require(prettymapr)

# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4, 
    lend = "square", ljoin = "mitre")
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1)
addscalebar(plotepsg = 32750, widthhint = 0.18, htin = 0.1, 
            label.cex = 1, padin = c(0.4, 0.2))
box()
#
# plotting our data
points(places$Easting, places$Northing, pch = 3)
symbols(places$Easting, places$Northing,
        circles = sqrt(places$Usage),
        fg = "purple", bg = "#b0009940",
        inches = 0.2, lwd = 2,
        add = TRUE) # 'add = TRUE' required to overplot map
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type, round(places$Usage)),
     pos = c(4,4,2,4,2,2,4,4,4,4,4,4),
     cex = 0.8, col = "purple3",
     offset = (places$Usage/8)) 

# Try adding a legend manually (tricky, optional!) -- see below!

# plot the map in desired projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4, 
    lend = "square", ljoin = "mitre")
plot(UWA_utm, removeMargin = FALSE)
axis(1); axis(2); box()
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1)
addscalebar(plotepsg = 32750, widthhint = 0.18, htin = 0.1, 
            label.cex = 1, padin = c(0.4, 0.2))
# -=-=-=-=-=-=- Manual legend -=-=-=-=-=-=-
# set up your bubble sizes
#[fudgf is a scaling factor- adjust to suit by trial-and-error]
fudgf <- 80
b0 <- round(fudgf/(max(sqrt(places$Usage), na.rm=T)),2)
if (pretty(places$Usage)[1] < 0.001) {
  bublo <- pretty(places$Usage)[2]
} else {
  bublo <- pretty(places$Usage)[1]
}
bubhi <- pretty(places$Usage)[NROW(pretty(places$Usage))]
# draw the bubble symbols
symbols(places$Easting,places$Northing, 
        circles=b0*sqrt(places$Usage), add=T,
        lwd=1, inches=F, fg="#8000B0", bg="#8000B040")
# optionally plot sample locations as symbols
points(places$Northing ~ places$Easting, pch=3, cex=0.7)

# optionally draw a box around where your legend will be
# positioning the rectangle takes more trial-and-error!
rect(388780,6461080,389180,6461480,
     border = "lightskyblue3", col = "#E0E0E080", lwd = 2)
text(388980,6461380,labels="Relative usage", 
     col=1, font=2, cex=0.9, pos = 3) # add a legend title
# choose coordinates where legend bubbles go (trial-and-error!)
symbols(c(388980,388980),c(6461340,6461180), circles=b0*sqrt(c(bublo,bubhi)), add=T,
        lwd=1, inches=F, fg="#8000B0", bg="#8000B040")
text(c(388980,388980),c(6461340,6461180), 
     labels=c(bublo,bubhi), cex=0.85)
# add place name labels
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type),
     pos = c(4,4,2,4,2,2,4,4,1,4,3,4),
     cex = 0.7, col = "purple3", offset = 1)
# remove temporary objects
rm(list = c("fudgf","b0","bublo","bubhi"))

## Plotting the basemap without OpenStreetMap ####

# Sometimes the OpenStreetMap package will not work (usually the
# problems relate to the Java installation on your computer, and/or the need for
# the XQuartz application on MacOS). In this case we can sometimes use base R
# graphics to plot a base map, so long as we have digitised data for the map
# features we require. For our base map the requirements are separately
# digitised map features such as road, boundaries, water bodies, and so on -- we
# have done this for you. The code below shows how to use this using a
# purpose-written function in R (you'll need an active internet connection to
# download and run the function code itself, and the digitised data as a csv
# file). The function code can be downloaded
# from the URL in the source() function below.

# first run this line of code (need working internet!):
source("https://raw.githubusercontent.com/Ratey-AtUWA/spatial/main/afr_map_function.R", 
       echo = FALSE)
# Then, run this line to draw the map
# try the function with no options first (empty parentheses)
afrMap()
afrMap(xlim = c(400000, 400400), ylim = c(6467900,6468300))

# add some points to this map! - - - or skip down a bit
with(afs20, points(Northing ~ Easting, 
                   pch = c(0,1,2)[Zone],
                   col =c(2,4,6)[Zone],
                   lwd = 2))
legend("bottom", legend = levels(afs20$Zone),
       pt.lwd = 2, pch = c(0,1,2), col=c(2,4,6))
text(400300,6467900,pos=4,labels = "Not bad for a rough map!")

# add 'bubbles'
afrMap(xlim = c(400000, 400400), ylim = c(6467900,6468300))
symbols(afs20$Easting, afs20$Northing, add = TRUE, 
        circles = sqrt(afs20$Zn)*0.2, inches = FALSE, 
        bg = "#8000B080", fg = "purple")
rect(400140, 6467920, 400270, 6468025, col="lightsteelblue1")
symbols(c(400200,400200),c(6467975,6467940), add = TRUE, 
        circles = sqrt(c(200, 5000))*0.2, inches = FALSE, 
        bg = "#8000B080", fg = "purple")
text(rep(400200,3),c(6468020,6467975,6467940), pos = c(1,4,4),
     labels = c("Zn (mg/kg)", "200", " 5000"))

# The possible options for the afrMap() function are: \newline
#   afrMap(proj = ..., xlim = ..., ylim = ..., maplabels = ...)
# Where:
#    proj can be either "utm" (the default) or "longlat"
#    xlim is a vector of upper and lower x-axis (E-W or horizontal) limits
#    ylim is a vector of upper and lower y-axis (S-N or vertical) limits
#    maplabels can either be TRUE (default) or FALSE and controls if 
#      selected map features are labelled

# The next line of code shows the effect of changing some options for the
# afrMap function.

# For example, try: (output in Figure 8)
afrMap(proj = "longlat", maplabels = T, 
       xlim = c(115.94,115.9485), ylim = c(-31.9205, -31.9165))

## Other tile-based map options -- rosm and ggmap

## The rosm R package ####

# The rosm package in R allows tile-based maps in several styles to be
# produced. If we run the functions osm.types() (for conventional maps) and
# bmaps.types() (for aerial photo maps), we get a list:

require(rosm)
osm.types()
bmaps.types()

# [Note that some of these styles may be unavailable or require an API key for
# which a cost may be involved.]

# To use rosm maps, we first define map limits:

require(rosm)
require(prettymapr)
# specify limits of map in order N, E, S, W
afr.rosm <- makebbox(-31.915,115.952,-31.922,115.939)
afr.rosm   # view the object contents

# Plot rosm map
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)
osm.plot(afr.rosm, type="osm", zoomin=0)

# plot an osm-type map
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)
osm.plot(afr.rosm, type="osm", zoomin=0)

# need to draw axes manually
axis(1); axis(2); box()
```

# We can see from the map that the coordinates used by
# rosm are not latitude-longitude or UTM. The rosm package uses the "Web
# Mercator" coordinate system, so to draw axes and points we need to either
# convert to our preferred coordinate systems or use a rosm function that does
# it for us.

# For axes, the only option seems to be to convert coordinates manually, and
# for this we can use the powerful sp package that underlies numerous spatial
# analyses in R. Before we do that, we use proj4 strings to define
# coordinate reference systems (CRS). Both proj4 and CRS are widely used
# in processing spatial data (and sometimes EPSG codes, but we only use EPSG in
# the prettymapr function addscalebar() function -- every CRS has a unique
# EPSG code).

### making and transforming map axes
require(sp) # needed for spTransform() function

# define the coordinate systems we might need
webMercator <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 
      +a=6378137 +b=6378137 +units=m +no_defs")
UTM_Zone50S <- CRS("+proj=utm +zone=50 +south")
LongLat <- CRS("+proj=longlat +datum=WGS84 +no_defs") 

# make x and y axis tick positions and coordinate pairs based on these
# the pretty() function makes a set of nicely spaced values based on 
# a less tidy range or vector of values - try it! e.g. pretty(c(37,98))
xtixLL <- pretty(afr.rosm[1,],4)
xaxLL <- SpatialPoints(cbind(xtixLL, rep(afr.rosm[2,1], length(xtixLL))), 
                       proj4string = LongLat)
ytixLL <- pretty(afr.rosm[2,],4)
yaxLL <- SpatialPoints(cbind(rep(afr.rosm[1,1], length(ytixLL)), ytixLL), 
                       proj4string = LongLat)

# do the conversions using the spTransform() function
xaxWM <- spTransform(xaxLL, CRSobj = webMercator)
yaxWM <- spTransform(yaxLL, CRSobj = webMercator)
xtixLL_WM <- xaxWM@coords[,1]
ytixLL_WM <- yaxWM@coords[,2]

xtixUTM <- pretty(spTransform(xaxLL, CRSobj = UTM_Zone50S)@coords[,1])
ytixUTM <- pretty(spTransform(yaxLL, CRSobj = UTM_Zone50S)@coords[,2])
xaxUTM <- SpatialPoints(cbind(xtixUTM, rep(ytixUTM[1], length(xtixUTM))), 
                        proj4string = UTM_Zone50S)
yaxUTM <- SpatialPoints(cbind(rep(xtixUTM[1], length(ytixUTM)), ytixUTM), 
                        proj4string = UTM_Zone50S)
xaxUTM_WM <- spTransform(xaxUTM, CRSobj = webMercator)
yaxUTM_WM <- spTransform(yaxUTM, CRSobj = webMercator)
xtixUTM_WM <- xaxUTM_WM@coords[,1]
ytixUTM_WM <- yaxUTM_WM@coords[,2]

# Now we can use the vectors of transformed x- and y-coordinates
# we've made to plot axes manually, using the at = and labels =
# options in the axis() function in base R. We'll draw a map with
# longitude--latitude axes, but you could edit the code to
# produce UTM axes.

# Plot the rosm map using transformed axes

# plot an osm-type map
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)
osm.plot(afr.rosm, type="osm", zoomin=0)

# need to draw axes manually . . .
axis(1, at = xtixLL_WM, labels = xtixLL)
axis(2, at = ytixLL_WM, labels = ytixLL)

# . . . and add axis titles manually
mtext("Longitude \u00B0 E)", side = 1, line = 1.8, font = 2)
mtext("Latitude (-\u00B0 S)", side = 2, line = 1.8, font = 2)
box()

# With the rosm package we can include prettymapr package annotations such as
# scale bar and north arrow by embedding rosm function(s) inside a prettymap()
# function (this is different from how we use these functions with
# OpenStreetMap):

par(mar=c(3,3,1,1), mgp = c(1.7,0.3,3), xpd = TRUE)
prettymap({osm.plot(afr.rosm, type="hotstyle", zoomin = 0)
    osm.points(c(115.945, 115.947, 115.943, 115.948, 115.947), 
             c(-31.9185, -31.9202, -31.918, -31.916, -31.9218), 
             col="red", lwd=3, pch = 15, cex=1.5)
    osm.text(115.945, -31.9185, 
           labels = "a point", pos = 4, 
           font = 3, col = "red")
    osm.text(115.948, -31.916, 
           labels = "another point", pos = 4, 
           font = 3, col = "red")},
  drawbox = TRUE,
  oma = c(3,3,1,1), drawarrow = T, arrow.scale = 1.2, 
  scale.htin = 0.15, scale.label.cex = 1.2, scale.pos = "bottomright"
)
axis(1, at = xtixUTM_WM, labels = xtixUTM, tick = FALSE)
axis(2, at = ytixUTM_WM, labels = ytixUTM, tick = FALSE)
mtext("Easting (m, UTM Zone 50)", side = 1, line = 1.8, font = 2)
mtext("Northing (m, UTM Zone 50)", side = 2, line = 1.8, font = 2)
legend(par("usr")[1] + 0.1*(par("usr")[2]-par("usr")[1]),
       par("usr")[3] + 0.3*(par("usr")[4]-par("usr")[3]), 
       legend = "some points", bg = "white",
       pch = 15, col = "red", inset = 0.02, cex = 1.2, pt.cex = 1.5)

# reset graphics pars
par(mar=c(4,4,1,1), mgp = c(1.7,0.3,0), xpd = FALSE) # reset

# For the functions osm.points() and osm.text, we specify coordinates in
# (longitude, latitude) which are then automatically converted to Web Mercator.
# Note that all the osm functions are together enclosed in curly brackets { }
# In addition, once we have closed the round brackets for the prettymap()
# function, then we can add a legend and other plot elements.

## The ggmap package ####

# We cover ggmap very briefly here... Knowledge of ggplot helps!

# make ggmap object for UWA Albany campus
require(ggmap)
register_google(key = "AIzaSyDU7QiTWE4RGFFQNmhWy51n7e4RBeHKjc0")
uwaAlb.gg <- get_googlemap(center=c(115.946,-31.919), 
                            zoom = 16, maptype = "satellite", color = "color")

# plot ggmap object using ggplot grammar
ggmap(uwaAlb.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_point(aes(x= Latitude, y=Longitude),
             data=afs21, colour="yellow", size=3) + 
  geom_text(aes(x = 117.888, y = -35.0275, label = "UWA Albany",
                fontface = "italic", family="sans"), 
            size = 4.5, vjust = 0.5, hjust = 0, color="#B00000") + 
  geom_text(aes(x = 117.875, y = -35.035, label = "Princess Royal\nHarbour",
                fontface = "italic", family="sans"), 
            size = 5, vjust = 0.5, hjust = 0.5, color="steelblue") + 
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14,face="bold"))

## References

# Dunnington. Dewey (2019). rosm: Plot Raster Map Tiles from Open Street Map and
# Other Sources. R package version 0.2.5.
# https://CRAN.R-project.org/package=rosm

# Dunnington, Dewey (2017). prettymapr: Scale Bar, North Arrow, and Pretty
# Margins in R. R package version 0.2.2.
# https://CRAN.R-project.org/package=prettymapr
  
# Fellows, Ian and using the JMapViewer library by Jan Peter Stotz (2019).
# OpenStreetMap: Access to Open Street Map Raster Images. R package version
# 0.3.4. https://CRAN.R-project.org/package=OpenStreetMap

# Kahle, D., Wickham, H. (2013) ggmap: Spatial Visualization with ggplot2. The R
# Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# Pebesma, E., Bivand, R., 2020. sp: Classes and Methods for Spatial Data. R
# package version 1.4-4. https://CRAN.R-project.org/package=sp.

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- 

## Appendix - a few things which might need explaining ####

# the ; character separates 2 or more functions in the same line of code

# the cat() function concatenates objects as text strings, useful for text output

# \n inserts a line break

# the source() function loads an R code file and runs all the code in the file

# the @ operator, as in xaxWM@coords extracts one of the parts of a spatial object
