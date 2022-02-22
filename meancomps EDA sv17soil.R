library(PMCMRplus)
library(rcompanion)
library(multcompView)

with(sv17soil, kruskal.test(IPI ~ Zone7))

kwpw <- with(sv17soil, kwAllPairsConoverTest(IPI ~ Zone7, p.adjust.method = "holm"))
print(kwpw)

pw_pv <- fullPTable(kwpw$p.value)
multcompLetters(pw_pv)

with(sv17soil, tapply(IPI, Zone7, mean, na.rm = T))
