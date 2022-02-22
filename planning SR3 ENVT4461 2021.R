require(car)
png(file = "spm_4461_SR3_sv18.png", height = 2000, width = 2000)
spm(~ pH + Al.log + Ca.log + Fe.log + K.log + S.log + As.pow + 
      Cr.log + Cu.log + Gd.log + Ni.log + P.log + Pb.log + Zn.log | Type, 
    data = sv18, col = c("blue2","red3","darkcyan"), cex=2, smooth=F,
    cex.lab = 3, cex.axis = 1.5)
dev.off()

# POSSIBILITIES (afs20):
## S ~ Fe
## Cr ~ Fe
## As ~ Al
#
# POSSIBILITIES (sv18):
## Gd.log ~ Fe.log - grouping improves
## P.log ~ Fe.log - grouping improves
## Zn.log ~ Fe.log - grouping DOES NOT improve

sp(P.log ~ Fe.log |Type, data= sv18, smooth=F, col = c("blue2","red3","darkcyan"),
   pch = c(19,15,17), cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
   legend = list(coords = "topleft", cex = 1.5))

lm0 <- lm(P.log ~ Fe.log, data = sv18)
summary(lm0)

lm1 <- lm(P.log ~ Fe.log * Type, data = sv18)
summary(lm1)

anova(lm0, lm1); rm(list = c("lm0","lm1"))
