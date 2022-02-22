# afs21TEMP <- read.csv("afs21TEMP.csv", stringsAsFactors = T)
afs21TEMP$MPtotal <-  rowSums(afs21TEMP[,c("MPfibre","MPfrag",
                                           "MBeads","MPother")], 
        na.rm = T)
#### check for zeros and replace with NA ####
# first look at the variable
print(afs21TEMP$MBeads)
# find row numbers of zeros
which(afs21TEMP$MBeads == 0)
# find how many zero values
length(which(afs21TEMP$MBeads == 0))
# use this info to replace zero values with NA
afs21TEMP[which(afs21TEMP$MBeads==0),"MBeads"] <- 
  rep(NA,length(which(afs21TEMP$MBeads == 0)))
# check it
print(afs21TEMP$MBeads)

# or you can do this by hand using 'fix(afs1)' - try it

afs21TEMP$mp.log <- log10(afs21TEMP$MPtotal)

# correct a typo in one of the coordinates
afs21TEMP[65,"Northing"] <- 6468030

# read in the base-R map info
afr_map <- read.csv("ashfield_map.csv", stringsAsFactors = TRUE)
require(prettymapr) # just in case :D

# so let's plot a base map for the afs21TEMP data
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square")
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
addnortharrow(padin = c(0.2,0.2))
addscalebar(plotepsg=32750, htin = 0.15, label.cex = 1.2)
box()

# and plot the afs21TEMP points 
# with a bit of trial and error to find our "Zones"
with(afs21TEMP, points(Northing[1:55] ~ Easting[1:55], 
                   col = "darkorchid", pch=3, 
                   lwd = 2, cex = 0.7))
with(afs21TEMP, text(Easting[1:55], Northing[1:55],
                 labels = afs21TEMP$Sample.ID[1:55], cex=.6, pos=4))
with(afs21TEMP, points(Northing[56:66] ~ Easting[56:66], 
                   col = "gold3", pch=16, 
                   lwd = 2, cex = 0.7))
with(afs21TEMP, text(Easting[56:66], Northing[56:66],
                 labels = afs21TEMP$Sample.ID[56:66], cex=.6, 
                 pos=4, col = "navy"))

# OK now we're set...
afs21TEMP$Zone <- as.factor(c(rep("East",55),
                          rep("Woolcock",6),
                          rep("Southwest",5)))
# just to check:
with(afs21TEMP, plot(Northing ~ Easting,
                 pch = c(3,1,0)[Zone],
                 col = c(2,4,6)[Zone],
                 asp = 1))
with(afr_map, lines(drain_E, drain_N, col = "darkcyan", lwd = 2))

# ok so now calculate minimum distance from ANY part of drain:

# first the 'bend' in the middle
lm_bend <- with(afr_map, lm(drain_N[9:12] ~ drain_E[9:12]))
print(lm_bend$coefficients, digits = 7)
abline(lm_bend, col = "grey")

# then the straight section closest to the Swan River
lm_chap <- with(afr_map, lm(drain_N[1:9] ~ drain_E[1:9]))
print(lm_chap$coefficients, digits = 7)
abline(lm_chap, col = "thistle")

# minimum distance from bend part of Chapman Drain
a <- as.numeric(lm_bend$coefficients[2])
b <- -1
c <- as.numeric(lm_bend$coefficients[1])
a;b;c # check them!
# the formula:
afs21TEMP$dist.bend = with(afs21TEMP,
                       (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))

# minimum distance from Swan River part of Chapman Drain
a <- as.numeric(lm_chap$coefficients[2])
b <- -1
c <- as.numeric(lm_chap$coefficients[1])
a;b;c # check them!
# the formula:
afs21TEMP$dist.chapS = with(afs21TEMP,
                        (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))

# calculate minimum distance from any part of the drain
# need to use pmin (not min) function to calculate across columns
afs21TEMP$dist.drain <- pmin(afs21TEMP$dist.bend, afs21TEMP$dist.chapS, na.rm=T)

# then look at your microplastics! or other variables!
# this is with Ba
# with(afs21TEMP, plot(Ba ~ dist.drain, log = "y"))
# lm_Ba_dist <- lm(log10(afs21TEMP$Ba) ~ afs21TEMP$dist.drain)
# summary(lm_Ba_dist)
# abline(lm_Ba_dist, col = "darkorchid")

fix(afs21TEMP)

with(afs21TEMP, plot(MPtotal ~ dist.drain, log = "y",cex=0.2,
                     xlab = "Distance from Chapman Drain (m)",
                     ylab = "Microplastics (counts/kg)",
                     font.lab = 2, cex.lab = 1.4))
with(afs21TEMP, points(MPfibre ~ dist.drain, pch = 8, col = "blue2"))
with(afs21TEMP, points(MPfrag ~ dist.drain, pch = 17, 
                       col = "grey65", cex = 1.3))
with(afs21TEMP, points(MBeads ~ dist.drain, pch = 21, 
                       bg = "gold", cex = 1.4))
with(afs21TEMP, points(MPother ~ dist.drain, pch = 7))

lm_fibre_dist <- with(afs21TEMP, lm(log10(MPfibre) ~ dist.drain))
summary(lm_fibre_dist)
abline(lm_fibre_dist, col = "blue2")

lm_frag_dist <- with(afs21TEMP, lm(log10(MPfrag) ~ dist.drain))
summary(lm_frag_dist)
abline(lm_frag_dist, col = "grey60")

lm_mbead_dist <- with(afs21TEMP, lm(log10(MBeads) ~ dist.drain))
summary(lm_mbead_dist)
abline(lm_mbead_dist, col = "sienna")

lm_other_dist <- with(afs21TEMP, lm(log10(MPother) ~ dist.drain))
summary(lm_other_dist)
abline(lm_other_dist, col = 1, lty=2)

legend("bottomright", inset = 0.02, title = "Microplastics", 
       legend = c("Fibres","Fragments","Microbeads","Other"),
       pch = c(8,17,21,7), pt.bg = "gold",
       col = c("blue2","grey60",1,1),
       pt.cex = c(1,1.4,1.4,1), cex = 1.4)

afs21TEMP$drain.prox <- cut(afs21TEMP$dist.drain, breaks=c(0,50,99999), 
    labels = c("less than 50m","greater than 50m"))
with(afs21TEMP, boxplot(Ba ~ drain.prox, log = "y", notch=T))
with(afs21TEMP, t.test(log10(Ba) ~ drain.prox))
# or multiple regression 
# MPtotal ~ dist.drain + Fe + Ba + P + ...

