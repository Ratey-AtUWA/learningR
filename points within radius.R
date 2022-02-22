afw21TEMP$dist_eDNA <- rep(NA,NROW(afw21))
afw21TEMP$which_eDNA <- rep(NA,NROW(afw21))
radius <- 20.000001
j <- 1;i <- 4
for (j in 1:6){
  for (i in 4:NROW(afw21)){
    dist <- sqrt((eDNAxy$Easting[j]-afw21TEMP$Easting[i])^2 +
                   (eDNAxy$Northing[j]-afw21TEMP$Northing[i])^2)
    if(dist<radius) afw21TEMP$dist_eDNA[i] <- dist
    if(dist<radius) afw21TEMP$which_eDNA[i] <- j
  }
}
rm(list = c("i", "j", "dist"))
print(afw21TEMP[,c("dist_eDNA", "which_eDNA")], digits=2)
