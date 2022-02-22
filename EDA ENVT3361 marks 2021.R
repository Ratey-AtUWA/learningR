marks <- read.table("clipboard", header = T, sep = "\t", stringsAsFactors = T)
with(marks, hist(Overall, breaks = 21))
colnames(marks)[19] <- "Country"
marksTEMP <- na.omit(marks)
marksCLR <- cbind(clr(marksTEMP[,2:9]), marksTEMP[,c(1,10:20)])
names(marksCLR)
rm(marksTEMP)

with(marks, hist(Overall, breaks = 21, main="", xlab = "Mark in ENVT3361"))


PCA3361 <- prcomp(marksCLR[1:8], scale. = T)
plot(PCA3361)

par(mar = c(3,3,3,3), mgp = c(1.7,0.3,0), tcl = 0.25,
    lend = "square", ljoin = "mitre", font.lab = 2)
biplot(PCA3361, col = c("transparent","grey60"), 
       xlim = c(-0.3,0.3), ylim = c(-0.3,0.3))
box()
sf <- 1.5
points(PCA3361$x[,1]*sf, PCA3361$x[,2]*sf,
       pch = c(22,1)[marksCLR$Major1],
       col = c(1,4)[marksCLR$Major1],
       bg = c("#B0000080",4)[marksCLR$Major1],
       cex = c(1.2,1.6)[marksCLR$Major1],
       lwd = 2, lend = "square")
legend("bottomleft", legend = levels(marksCLR$Major1),
       title = "Major1", cex = 1.2,
       pch = c(0,1),
       col = c(2,4),
       pt.cex = c(1.2,1.6),
       pt.lwd = 2)
str(marksCLR)
# PCA3361$rotation

marksCLR$Grade <- cut(marksCLR$Overall, breaks = c(60,65,70,75,80,100), 
                      labels = c("CR_60-65","CR_65-70","D_70-75","D_75-80","HD_80-99"))

palette(c("black",rainbow(5, end=0.8, v=0.75, alpha=0.5)))

par(mar = c(3,3,3,3), mgp = c(1.7,0.3,0), tcl = 0.25,
    lend = "square", ljoin = "mitre", font.lab = 2)
biplot(PCA3361, col = c("transparent","grey60"), 
       xlim = c(-0.3,0.3), ylim = c(-0.3,0.3))
box()
sf <- 1.5
points(PCA3361$x[,1]*sf, PCA3361$x[,2]*sf,
       pch = c(15,19,17,18,25)[marksCLR$Grade],
       col = seq(2,6)[marksCLR$Grade],
       bg = seq(2,6)[marksCLR$Grade],
       cex = c(1.4,1.6,1.4,1.8,1.2)[marksCLR$Grade],
       lwd = 2)
legend("bottomleft", legend = levels(marksCLR$Grade),
       title = "Grade", cex = 1.,
       pch = c(15,19,17,18,25),
       col = seq(2,6),
       pt.bg = seq(2,6),
       pt.cex = c(1.4,1.6,1.4,1.8,1.2),
       pt.lwd = 2)
