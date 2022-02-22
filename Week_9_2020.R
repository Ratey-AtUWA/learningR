#### Use OpenStreetMap to make an initial map object ####
#
# The easiest way to find the upper left (north-west) and
# lower right (south-east) corners of our intended map is
# to use Google Maps or (if you have it) Google Earth.
#
require(OpenStreetMap) # R package needed for the maps we will make
# the 'zoom = ' option sets the resolution (higher for more detail)

UWA_osm <- openmap(upperLeft = c(-31.974, 115.812), 
                   lowerRight = c(-31.988, 115.828), 
                   zoom = 16,
                   type = "stamen-terrain")
plot(UWA_osm)
#
# You could also try type = "bing",  OR 
# "stamen-toner", "stamen-terrain", "stamen-watercolor", 
# "esri", "esri-topo", "opencyclemap", "osm-transport", 
# "osm-public-transport"
# You can even sign up at https://www.mapbox.com/maps/ to get more options...
#
# Whatever style you choose...
# This map needs some attention! Without some prior knowledge, 
# we don't know where on Earth's surface this map represents, we don't 
# know the direction it's oriented in, and we don't know the scale. 
#
# With the prettymapr package we can add a north arrow to indicate 
# direction, a scale bar to show the scale, and axes to show any 
# coordinates which should show the location on Earth.

#### Plot the UWA campus map with margins, axes, and annotations ####
#
require(OpenStreetMap) # just in case it's not loaded yet
require(prettymapr) # required for map annotations
# set graphics options for better use of plotting space
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
# plot the map using removeMargin = FALSE so we have room for axes
plot(UWA_osm, removeMargin = FALSE)
# manually add x and y axes
axis(1) # bottom (x) axis
axis(2) # left (y) axis
addnortharrow() # just using the default settings
addscalebar() # just using the default settings
box()

# text(12893150,-3760700,labels="plot(UWA_osm, removeMargin = FALSE)\naxis(1)\naxis(2)\naddnortharrow()\naddscalebar()\nbox()",pos=4, cex=1., family = "mono")

#
# The map is now better - we now know scale and direction, 
# but finding our location on Earth is tricky because the OpenStreetMap 
# map projection is "Google Mercator" which is not a projection most people 
# are familiar with. We need a map in a commonly-used projection such as 
# Longitude-Latitude, or Universal Transverse Mercator (UTM). We can change 
# projections using the openproj() function in the OpenStreetMap package.

#### Plot a re-projected UWA map in UTM + margins, axes, and annotations ####

# First convert to the desired projection...
#
require(OpenStreetMap) # just in case it's not loaded yet
UWA_utm <- openproj(UWA_osm, projection = "+proj=utm +zone=50 +south")
{cat("Show upper left (p1) and lower right (p2) coordinates\n")
UWA_utm$bbox} # show bounding coordinates to check
#
### ...then plot the map in its new projection.
require(prettymapr) # just in case it's not loaded yet
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1.2)
# add scale bar - we need to specify plotepsg = 32750 for UTM ZOne 50
addscalebar(plotepsg = 32750, htin = 0.15, label.cex = 1.2)
box()
#

text(388650,6461200,
     labels="# plot the UTM map\nplot(UWA_utm,\n    removeMargin = FALSE)\naxis(1)\naxis(2)\naddnortharrow()\naddscalebar()\nbox()",
     pos=4, cex=1., family = "mono")

# The map is now just what we need - we know scale and direction, 
# and we have axes in the Universal Transverse Mercator (UTM) 
# projection with units of metres, which we would normally set up 
# our field GPS to show.
# 
# Very often we would like to add our own information to a map, 
# such as the location of samples, often with different sizes 
# or shapes of symbols to represent numerical information.

#### Making some data to plot on our map ####
# 
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
Usage <- c(10,10,8,4,2,7,1,0.1,0.1,1,2,4) # frequency of use by unit coordinator
places <- as.data.frame(cbind(Longitude, Latitude, Easting, Northing, Usage))
places$Name <- Name
places$Type <- as.factor(Type)
# str(places)
rm(list = c("Longitude", "Latitude", "Easting", "Northing", 
            "Name", "Type", "Usage"))
print(places)
# 

#### Plot simple data on our best map ####
# An OpenStreetMap plot is essentially an x-y plot with a map image 
# as the plot background.
# We can use standard R graphics functions to add points, lines, 
# polygons, and text (including legends) to the map.
# 
# just in case packages are not loaded yet...
require(OpenStreetMap)
require(prettymapr)
#
# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), 
    mgp = c(1.6, 0.3, 0), 
    tcl = 0.4)
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, 
      line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, 
      line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1.2)
# another variation of addscalebar() options...
addscalebar(plotepsg = 32750, widthhint = 0.1, 
            htin = 0.1, label.cex = 1.2)
box()
#
# plotting our data
points(places$Easting, places$Northing,
       pch = c(15, 19)[places$Type],
       col = c("red3", "blue2")[places$Type],
       cex = c(1.3, 1.5)[places$Type])
# plot some text to explain the data points
# note how we use paste() to include data from 2 columns
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type),
     cex = 0.8,
     pos = c(4,2,4,4,2,2,4,4,4,4,4,4),
     col = c("red3","blue2","red3","red3","blue2","red3",
               "red3","blue2","blue2","red3","red3","red3"))
# add a legend (not really needed with the added text above)
legend("right",
       legend = levels(places$Type),
       pch = c(15, 19),
       col = c("red3", "blue2"),
       pt.cex = c(1.3, 1.5),
       cex = 0.9,
       inset = 0.03)
# 
#### Plot numerical data on our best map ####
# One way of representing quantities at different spatial locations 
# is to use a "bubble plot", using the symbols() function in R.
# We calculate the _square root_ of the quantity being represented 
# (see code below), to make our circle _area_ proportional to quantity - 
# area is more intuitive for human perception of amounts than are 
# radius or diameter.
# _________________________________________________
# just in case packages are not loaded yet...
require(OpenStreetMap)
require(prettymapr)
#
# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4, 
    lend = "square", ljoin = "mitre")
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1)
# yet another variation of addscalebar() options...
addscalebar(plotepsg = 32750, widthhint = 0.18, htin = 0.1, 
            label.cex = 1, padin = c(0.4, 0.2))
box()
#
# plotting our data
points(places$Easting, places$Northing, pch = 3)
symbols(places$Easting, places$Northing,
        circles = sqrt(places$Usage),
        fg = "purple", 
        bg = "#b0009940",
        inches = 0.2, 
        lwd = 2,
        add = TRUE) # 'add = TRUE' required to overplot map
# we can include some text to add information to the 'bubbles'
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type, round(places$Usage)),
     cex = 0.8,
     pos = c(4,4,2,4,2,2,4,4,4,4,4,4),
     col = "purple3",
     offset = (places$Usage/8)) 
# _______________________________________________

#### Try adding a legend manually (tricky, optional!) ####
# 
# just in case packages are not loaded yet...
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
# manual legend
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
#
# optionally plot sample locations as symbols
points(places$Northing~places$Easting,
       pch=3, cex=0.7)
#
# optionally draw a box around where your legend will be
# positioning the rectangle takes more trial-and-error!
rect(388720,6461080,389220,6461480,
     border = "lightskyblue3", col = "gray92", lwd = 2)
# add a legend title
text(388980,6461380,labels="Relative usage", 
     col=1, font=2, cex=0.9, pos = 3)
#
# choose coordinates where legend bubbles go (trial-and-error!)
symbols(c(388980,388980),c(6461340,6461180), circles=b0*sqrt(c(bublo,bubhi)), add=T,
        lwd=1, inches=F, fg="#8000B0", bg="#8000B040")
text(c(388980,388980),c(6461340,6461180), 
     labels=c(bublo,bubhi), 
     cex=0.85)
# add place name labels
text(places$Easting, places$Northing,
     labels = paste(places$Name, places$Type),
     cex = 0.7,
     pos = c(4,4,2,4,2,2,4,4,1,4,3,4),
     col = "purple3",
     offset = 1)
# All done!
# remove temporary objects
rm(list = c("fudgf","b0","bublo","bubhi"))
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# 
#### References ####
# Dunnington, Dewey (2017). prettymapr: Scale Bar, North Arrow, and Pretty 
#   Margins in R. R packageversion 0.2.2. 
#   https://CRAN.R-project.org/package=prettymapr
# Fellows, Ian and using the JMapViewer library by Jan Peter Stotz (2019).
#   OpenStreetMap: Access to Open Street Map Raster Images. 
#   R package version 0.3.4.
#   https://CRAN.R-project.org/package=OpenStreetMap

