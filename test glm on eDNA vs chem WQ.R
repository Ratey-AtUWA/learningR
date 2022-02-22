glm_nSpecies <- with(af_eDNA_20, 
                     glm(nSpecies ~ pH + EC + Ba + Fe, 
                         family = poisson, na.action = na.exclude)
                     )
summary(glm_nSpecies)
pred_nSpecies <- predict.glm(glm_nSpecies, type="response")
as.numeric(pred_nSpecies)
plot(af_eDNA_20$nSpecies, pred_nSpecies)
abline(0,1,col = "red", lty = 2)

windowsFonts(nar = "Arial Narrow")
par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25,
    font.lab = 2, cex.lab = 1.4)
with(af_eDNA_20, 
     boxplot(nSpecies ~ Sample))
with(af_eDNA_20, 
     stripchart(nSpecies ~ Sample, pch = 18, col = 4, cex = 2, 
                xlim = c(0.4,6.6), ylim = c(0,7), vertical=T))
text(1,6.28,labels = paste("pH",af_eDNA_20$pH[1]), col = "darkblue")
text(seq(2,6),rep(6.3,5),labels = af_eDNA_20$pH[seq(5,24,4)], col = "darkblue")

text(1,6.55,labels = paste("EC",af_eDNA_20$EC[1]))
text(seq(2,5),rep(6.55,4),labels = af_eDNA_20$EC[seq(5,20,4)])
text(6,6.55,labels = paste(af_eDNA_20$EC[24],"\u00B5S/cm"))

text(1,6.8,labels = paste("Ca",af_eDNA_20$Ca[1]), col = "purple")
text(seq(2,5),rep(6.8,4),labels = af_eDNA_20$Ca[seq(5,20,4)], col = "purple")
text(6,6.8,labels = paste(af_eDNA_20$Ca[24],"mg/L"), col = "purple")

text(1,7.05,labels = paste("Fe",af_eDNA_20$Fe[1]), col = "darkred")
text(seq(2,5),rep(7.05,4),labels = af_eDNA_20$Fe[seq(5,20,4)], col = "darkred")
text(6,7.05,labels = paste(af_eDNA_20$Fe[24],"mg/L"), col = "darkred")
#
with(af_eDNA_20, 
     stripchart(nSpecies ~ Sample, pch = 18, col = 4, cex = 2, 
                xlim = c(0.4,6.6), ylim = c(0,6.4), vertical=T))
text(1,6.3,labels = paste("Ca",round(af_eDNA_20$Ca[1],1)))
text(seq(2,5),rep(6.3,4),labels = round(af_eDNA_20$Ca[seq(5,20,4)],1))
text(6,6.28,labels = paste(round(af_eDNA_20$Ca[24],1),"mg/L"))
with(af_eDNA_20, 
     boxplot(nSpecies ~ Sample))
text(1,6.1,labels = paste("Fe",round(af_eDNA_20$Fe[1],2)))
text(seq(2,5),rep(6.1,4),labels = round(af_eDNA_20$Fe[seq(5,20,4)],2))
text(6,6.08,labels = paste(round(af_eDNA_20$Fe[24],2),"mg/L"))
