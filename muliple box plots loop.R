# create a vector of column names to label y-axes later
names.of.cols <- names(afs20)
#
# generate matrix of comma separated values
# and calculate new variables
#
# define starting and ending columns
c1 <- 9
cn <- 37
par(mar = c(3,3,1,1), mfrow = c(4,4), mgp = c(1.6,0.3,0), tcl = 0.25,
    cex.lab = 1.2, font.lab = 2)
# start loop that draws box plots
for (i in c1:cn) {
  boxplot(afs20[,i] ~ afs20$Zone,
          xlab = "Ashfield Wetlands Zone", ylab = "",
          col = c("#A1E0E0","#9DC6DB","#9CB0D8"))
  mtext(names.of.cols[i],2,1.6, font = 2) # y-axis label
}
#
par(mar = c(3,3,1,1), mfrow = c(4,4), mgp = c(1.6,0.3,0), tcl = 0.25,
    cex.lab = 1.2, font.lab = 2)
# start loop that draws box plots for log10-transformed variables
for (i in c1:cn) {
  boxplot(log10(afs20[,i]) ~ afs20$Zone,
          xlab = "Ashfield Wetlands Zone", ylab = "",
          col = c("#EFBFA7","#E5CFA2","#E2E2A1"))
  mtext(paste0("log10(",names.of.cols[i],")"),2,1.6, font = 2)
}
#
# remove temporary objects
# to keep R workspace tidy
rm(list=c("c1","cn","names.of.cols","i"))
# end code