#### Steps in running a multiple regression model ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
# Multiple regression models predict the value of one variable 
# (the 'dependent variable') from two or more predictor variables 
# (or just 'predictors'). They can be very useful in environmental 
# science, but there are several steps we need to take to make sure  
# that we have a valid model.
#   In this example we're going to develop a regression model to 
# predict barium (Ba) concentrations from several predictors. 
# It makes sense to choose predictors that represent bulk soil properties 
# that could <u>plausibly</u> control trace element concentrations. So, 
# we choose variables like pH, EC, organic carbon, cation exchange 
# capacity, and the major elements as predictors (but not other 
# trace elements, as their concentrations are probably 
# too low to control the concentration of any thing else!)
# Since we don't have organic carbon or cation exchange capacity 
# in this dataset, our initial 
# predictors will be Al, Ca, Fe, K, Mg, Mn, Na, pH, EC, and S. 
# Both the predictors and dependent variable need to be appropriately 
# transformed before we start!
#   Also, some of our initial predictors may be highly correlated 
# (co-linear) with each other. In multiple regression, we don't want to 
# include co-linear predictors, since then we'll have two (or more) 
# predictors which effectively contain the same information - see below.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
#
#### Read input data ####
#
afw20 <- read.csv("afw20_export.csv", stringsAsFactors = T)
print(afw20[1:5,2:11], row.names = FALSE)
#
#### Assess collinearity between initial set of predictors ####
#
#   First we inspect the correlation matrix. It's useful to include the 
# dependent variable as well, just to see which predictors are 
# most closely correlated.
# 
# (We try to generate a 'tidier' table by restricting numbers to 3 
#  significant digits, based on the smallest r value in each column.)

#### Basic EDA ###
namz <- c("pH","EC","Ca","K","Mg","Mn","Na","Rb","S","Si","Sr","Zn","Ba")
par(mfrow=c(7,2), mar = c(4,4,1,1), mgp=c(1.7,0.3,0), tcl = 0.3)
for (i in 1:13){stripchart(afw20[,namz[i]], method = "jitter", col = i, log = "x");mtext(namz[i],3,-1)}
par(mfrow=c(1,1))

#### test variables for normality ####
# this version just tests all [un]transformed variables for normality
#
# load required packages
require(car)
# create temp object with names of variables to be transformed
names.of.cols <- names(afw20)
#
# generate matrix of comma separated values
# and calculate new variables
#
# define starting and ending columns
# c1 <- 4
# cn <- 38
# or make list of desired columns
colz <- c(5:9,11:13,19,21,23:25,27,30,32,33,35,36,38)
# make initial output data frame
transf_results <- data.frame("Variable"=rep(NA,NROW(colz)),
                             "W_orig"=rep(NA,NROW(colz)),
                             "p_orig"=rep(NA,NROW(colz)), 
                             "W_log_tr"=rep(NA,NROW(colz)),
                             "p_log_tr"=rep(NA,NROW(colz)), 
                             "W_pow_tr"=rep(NA,NROW(colz)),
                             "p_pow_tr"=rep(NA,NROW(colz)), 
                             "Pow_term"=rep(NA,NROW(colz)))
# start loop that assesses variable distributions and creates new variables
j <- 1
for (i in colz) {
  pt1 <- powerTransform(afw20[, i])
  # afw20[paste0(names.of.cols[i],".log")]<-log10(afw20[i])
  # if ... else applies factor of -1 to
  # power transforms with negative terms
  # if (as.vector(pt1$lambda) > 0) {
  #   afw20[paste0(names.of.cols[i], ".pow")] <-
  #     afw20[i] ^ as.numeric(unlist(pt1$lambda))
  # }
  # else {
  #   afw20[paste0(names.of.cols[i], ".pow")] <-
  #     -1 * ((afw20[i]) ^ as.numeric(unlist(pt1$lambda)))
  # }
  # generate and save test statistics
  sw0 <- shapiro.test(afw20[, i])
  sw1 <- shapiro.test(log10(afw20[, i]))
  sw2 <- shapiro.test((afw20[, i]) ^ as.vector(pt1$lambda))
  transf_results[j,] <- c(names.of.cols[i], signif(sw0$statistic, 4),
                          signif(sw0$p.value, 4), signif(sw1$statistic, 4),
                          signif(sw1$p.value, 4), signif(sw2$statistic, 4),
                          signif(sw2$p.value, 4), signif(as.vector(pt1$lambda), 4))
  j <- j + 1
}
#
# output to console (screen)
cat("Table. Shapiro-Wilk statistics and p-values for untransformed (_orig) and transformed
(_log, _pow) variables from soil and sediemnt analysis at Smith's Lake Reserve.\n\n")
print(transf_results, row.names = FALSE)

##
# export results to clipboard for Excel (if desired)
write.table(transf_results, file = "clipboard", row.names = FALSE, sep = "\t")
# remove temporary objects
# to keep R workspace tidy
rm(list=c("names.of.cols","pt1","sw0","sw1","sw2","i","j"))

# 
# Note that all variables should be appropriately transformed!
afw20$Al.log <- log10(afw20$Al)
afw20$Ca.log <- log10(afw20$Ca)
afw20$Fe.log <- log10(afw20$Fe)
afw20$K.log <- log10(afw20$K)
afw20$Mg.log <- log10(afw20$Mg)
afw20$Na.log <- log10(afw20$Na)
afw20$EC.log <- log10(afw20$EC)
afw20$S.log <- log10(afw20$S)
afw20$Si.log <- log10(afw20$Si)
afw20$Sr.log <- log10(afw20$Sr)
afw20$Mn.log <- log10(afw20$Mn)
afw20$Rb.log <- log10(afw20$Rb)
afw20$Ba.log <- log10(afw20$Ba)
afw20$Zn.log <- log10(afw20$Zn)
afw20$pH.pow <- afw20$pH^4
#
for(i in c(5,6,9,12,13,19,21,24,25,27,33,39:53)) {
  cat(names(afw20)[i],": p = ",signif(shapiro.test(afw20[,i])$p.value,3),"\n", sep="")
  }
# 
require(Hmisc)
cor0 <- rcorr(as.matrix(na.omit(afw20[,c("pH.pow","Al.log","Ca.log","Fe.log","K.log","Mg.log","Na.log",
                                 "EC.log","S.log","Mn.log","Si.log","Sr.log","Rb.log",
                                 "Ba.log","Zn.log")])))
print(cor0$r, digits=3)
rm(cor0)
#
#    The rule of thumb we use is that if predictor variables are
# correlated with Pearson's r > 0.8 or r < -0.8, then the 
# collinearity is too large and one of the correlated predictors 
# should be omitted. In the correlation table above this applies 
# to the large positive correlations between [transformed] Ca, K, 
# Mg, Na, EC, S, Sr, and Rb, all with r>=0.94. 
#     In this example we will run two versions of the model, one 
# keeping both Ca, K, Mg, Na, EC, and S, and one including EC but 
# omitting Ca, K, Mg, Na, and S (the rationale being that EC 
# represents generalized salinity).
#     In either case, whether we run the model with or without 
# omitting predictors, it's a good idea to calculate 
#'Variance Inflation Factors' on the predictor 
# variables in the model (see below) which can tell us if 
# collinearity is a problem.
#
#### generate mutiple regression model for Ba (co-linear predictors NOT omitted) ####
#
# make new data object containing relevant variables with no missing values
#
afw20_multreg <-
  na.omit(afw20[c("pH.pow","Al.log","Ca.log","Fe.log","K.log","Mg.log","Na.log",
                  "EC.log","S.log","Mn.log","Si.log", "Ba.log")])
# run model using correctly transformed variables
lm_multi <- lm(Ba.log ~ pH.pow + Al.log + Ca.log + Fe.log + 
                 EC.log + K.log + S.log + Na.log + Mn.log, 
               data=afw20_multreg)
summary(lm_multi)
#
# Note that the null hypothesis probability Pr(>|t|) for several predictors 
# is >0.05, so we can't reject the null hypothesis - that this predictor 
# has no effect on the dependent variable.
#
# calculate variance inflation factors (VIF) ####
#     for the predictors in the 'maximal' model 
require(car)
cat("Variance Inflation Factors\n"); vif(lm_multi)
#
# A general rule of thumb is that if VIF > 4 we need to do some further 
# investigation, while VIFs > 10 are signs of serious multicollinearity requiring 
# correction (Hebbali, 2018). As we probably expected from the correlation 
# coefficient (above), VIFs for both highly correlated  variables are >>10
# in this model, so we should try a model which omits all except one 
# (we'll choose EC)...
#
# generate mutiple regression model for Ba omitting co-linear predictors ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
# make new data object containing relevant variables with no missing values
afw20_multreg <-
  na.omit(afw20[c("pH.pow","Al.log","Ca.log","Fe.log","K.log","Mg.log","Na.log",
                  "EC.log","S.log","Mn.log", "Ba.log")])
# run model using correctly transformed variables (omitting co-linear predictors)
lm_multi2 <- lm(Ba.log ~ pH.pow + Al.log + Fe.log + 
                  K.log + Mn.log, 
                data=afw20_multreg)
summary(lm_multi2)
#
# Note that again the null hypothesis probability Pr(>|t|) for some 
# predictors is >0.05, so we can't reject the null hypothesis - that 
# this predictor has no effect on the dependent variable.

# calculate variance inflation factors (VIF) for the model omitting 
#   co-linear predictors
require(car)
cat("Variance Inflation Factors\n");vif(lm_multi2)
#
# With the co-linear variable(s) omitted (on the basis of |Pearson's r| > 0.8), 
# we now have no VIFs > 4. So we can move on to stepwise refinement of our 
# [new] 'maximal' model...
#
# stepwise refinement of maximal multiple regression model ####
    # (omitting co-linear predictors)
lm_stepwise <- step(lm_multi2, direction="both", trace=0)
summary(lm_stepwise)
require(car)
cat("Variance Inflation Factors\n");vif(lm_stepwise)
    #
    # try just a forward model
    lm_min = lm(Ba.log ~ 1, data = afw20_multreg)
    lm_fwd = step(lm_min, direction = "forward", 
                     scope=(~ Ba.log ~ pH.pow + Al.log + Fe.log + 
                              K.log + Mn.log))
    summary(lm_fwd)
    #
# In the optimised model, we find that the stepwise procedure has 
# generated a new model with fewer predictor variables. You should 
# notice that the p-values (Pr(>|t|)) for intercept and predictors 
# are all now <0.05, so we can reject the null hypothesis for all 
# predictors (i.e. none of them have 'no effect' on Ba). 
# Our VIFs are now all close to 1, meaning negligible collinearity 
# between predictors.
# 
# It's always a good idea to run diagnostic plots (see below) on a 
# regression model (simple or multiple), to check for (i) any 
# systemaic trends in residuals, (ii) normally distributed residuals, 
# and (iii) any unusually influential observations.
#   
# regression diagnostic plots ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
par(mfrow=c(2,2), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0), 
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
plot(lm_stepwise)
par(mfrow=c(1,1))
#
# multiple regression effect plots ####
require(effects)
plot(allEffects(lm_stepwise, confidence.level=0.95))
#
# scatterplot of observed vs. fitted values ####
par(mar=c(4,4,1,1), mgp=c(2,0.5,0), font.lab=2, cex.lab=1.2, 
    lend="square", ljoin="mitre")
plot(afw20_multreg$Ba.log ~ lm_stepwise$fitted.values,
     xlab="Ba.log predicted from regression model",
     ylab="Ba.log measured values", pch=3, lwd=2, 
     cex=0.8, col="blue3")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", legend=c("Observations","1:1 line"), col=c("blue3","red"), 
       text.col=c("blue3","red"), pch=c(3,NA), lty=c(NA,2), pt.lwd=2, lwd=2, 
       box.col="grey", box.lwd=2, inset=0.02, seg.len=2.7, y.intersp=1.2)
mtext(side=3, line=-5.5, adj=0.05, col="blue3",
      text=paste("Multiple R\u00B2 =",
                 signif(summary(lm_stepwise)$r.squared,3)))
#
lm_simple <- lm(Ba.log ~ EC.log, 
               data=afw20_multreg)
summary(lm_simple)

anova(lm_simple, lm_stepwise)

#### Final checks with gvlma package ####
gvlma(Ba.log ~ Al.log + Fe.log + EC.log + Mn.log, data = afw20_multreg)
#    ...or...
gvlma.lm(lm_stepwise)
plot.gvlma(gvlma(Ba.log ~ Al.log + Fe.log + EC.log + Mn.log, data = afw20_multreg))

# brief interpretation ####
#1. The adjusted R-sqared value of the final model is 0.586, meaning 
#   that 58.6% of the variance in Ba is explained by variance in the 
#   model's predictors. [The remaining 41.4% of variance must therefore 
#   be due to random variations, or 'unknown' variables not included 
#   in our model.]
#
#2. From the model coefficients and the effect plots we can see that 
#   Ba increases as EC and Mn increase. This agrees with the 
#   individual relationships from correlation analysis.
#
#3. Although we can't attribute a causal relationship to correlation 
#   or regression relationships, the observed effects in our model 
#   may be consistent with real phenomena. 
#   For example, we might expect Ba concentrations in water to increase 
#   with increasing EC, if Ba behaves like or associates with Ca and Sr,
#   since this probably represents dilution of seawater. The concentration 
#   of Ba is pretty low in seawater though at <0.01 mg/L! (Chester 2009). 
#   An positive correlation of Ba and Mn could represent co-occurrence of
#   both elements in steel products and consequent co-contamination by 
#   human activities.
#
# references ####
# Chester, R., 2009. Marine Geochemistry, Second ed. Wiley/Blackwell Science, 
#   Hoboken, USA, p. 522.
# Hebbali, A. (2018). Collinearity Diagnostics, Model Fit & Variable Contribution. 
#   Vignette for R Package 'olsrr'  Retrieved 2018.04.05, from 
#   cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html.
# Sposito, G., 2008 The Chemistry of Soils, Second Edition. Oxford University 
#   Press, New York, 329 pp.
# 
# [end code]