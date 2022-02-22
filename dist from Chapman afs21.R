afr_map <- read.csv("ashfield_map.csv")
require(prettymapr)
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0),
    tcl = 0.3, ljoin = "mitre", lend = "square")
palette(c("black", rainbow(6, v = 0.75, end = 0.1),
          rainbow(6, v = 0.75, start = 0.5, end = 0.8)))

with(afr_map, plot(street_E, street_N, asp=1, type = "l", lwd = 6, col = "grey90",
                   xlab = "", ylab = "",
                   xlim=c(399910,400670), ylim=c(6467940,6468540)))

mtext("Easting (UTM Zone 50, m)", side=1, line=1.6, cex=1.2, font=2)
mtext("Northing (UTM Zone 50, m)", side=2, line=1.6, cex=1.2, font=2)
with(afr_map, lines(drain_E, drain_N, col="royalblue4", lwd=2))
with(afr_map, lines(drain_E[9:12], drain_N[9:12], col="red", lwd=2))

lm_bend <- with(afr_map, lm(drain_N[9:12] ~ drain_E[9:12]))
print(lm_bend$coefficients, digits = 7)
abline(lm_bend, col = "grey")

lm_chap <- with(afr_map, lm(drain_N[1:9] ~ drain_E[1:9]))
print(lm_chap$coefficients, digits = 7)
abline(lm_chap, col = "thistle")

# minimum distance from bend part of Chapman Drain
a <- as.numeric(lm_bend$coefficients[2]); b <- -1; c <- as.numeric(lm_bend$coefficients[1])
a;b;c # check them!
# the formula:
afs21$dist.bend = with(afs21,
                       (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))

# minimum distance from south part of Chapman Drain
a <- as.numeric(lm_chap$coefficients[2]); b <- -1; c <- as.numeric(lm_chap$coefficients[1])
a;b;c # check them!
# the formula:
afs21$dist.chapS = with(afs21,
                       (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))
min(afs21$dist.bend, afs21$dist.chapS, na.rm=T)

# calculate minimum distance from any part of the drain
afs21$dist.drain <- pmin(afs21$dist.bend, afs21$dist.chapS, na.rm=T)

# regression of variable vs minimum distance from any part of Chapman drain
# # # # may need to remove zero values from concentration variables ! ! # # # #
afs21$Cu.log <- log10(afs21$Cu)
with(afs21, plot(Cu.log ~ dist.drain))
lm_Cu_drain <- with(afs21, lm(Cu.log ~ dist.drain))
summary(lm_Cu_drain)
abline(lm_Cu_drain, col = "purple")
