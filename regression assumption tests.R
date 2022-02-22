library(car)
library(lmtest)

# simple linear model ####
lm0 <- with(sv18, lm(log10(Gd) ~ log10(Fe)))
summary(lm0)
par(mfrow = c(2,2));plot(lm0);par(mfrow = c(1,1))
# car::ncvTest(lm0) # homoscedasticity
# car::durbinWatsonTest(lm0) # autocorrelation (independence)
car::outlierTest(lm0) # influential observations (outliers)

lmtest::bgtest(lm0) # autocorrelation (independence)
lmtest::bptest(lm0) # homoscedasticity
lmtest::raintest(lm0) #linearity

# grouped model ####
lm1 <- with(sv18, lm(log10(Gd) ~ log10(Fe) * Type))
summary(lm1)
par(mfrow = c(2,2));plot(lm1);par(mfrow = c(1,1))
# car::ncvTest(lm1) # homoscedasticity
# car::durbinWatsonTest(lm1) # autocorrelation (independence)
car::outlierTest(lm1) # influential observations (outliers)

lmtest::bgtest(lm1) # autocorrelation (independence)
lmtest::bptest(lm1) # homoscedasticity
lmtest::raintest(lm1) # linearity
