

#  The inspiration for this example was some code written by Michael Renton.
#  Michael is a joint appointment between the School of Agriculture and
#  Environment and the School of Biological Sciences.  He teaches some advanced
#  modeling courses here at UWA

#  The example illustrates the way the R-squared value is calculated
#  and the decomposition of values to the various sum of squares components

#  Read in the data

Brushdata <- read.csv("Brush_Tailed_Phascogale.csv")

head(Brushdata)
str(Brushdata)

#  Context:  we have some Brush-tailed Phascogales in captivity.  We are
#  interesting in understanding how they behave, and we also have three
#  different feed types we are trialing: An expensive commercial feed, a new
#  product from CSIRO researchers, and our own home blend on cornflakes and
#  veggies.  Ultimately we want to re-populate an area with marsupials, but we
#  need to get some understanding about them first.

#  In the data frame we have:
#  weight = animal weight, they are pretty small
#  amount eaten = how much food each animal ate in a 24hr period
#  time active = average recorded amount of movement per hour over a 24 hr period

# Q. is there a relationship between the weight of an animal and the amount of
# time for which it is active?
# 

plot(weight.g. ~ time.active, data = Brushdata)     

#  The plot looks linear so let's work with that

## Let's fit a linear model using the R lm function

myfittedmodel <- lm(weight.g. ~ time.active, data = Brushdata)

## note the thing that we are predicting (the dependent or 'y' variable) comes
## first... ...and the thing we are using to predict with (the independent or
## 'x' variable) comes last i.e. here we are predicting time active from weight
## the ~ symbol means 'predicted by' or 'modelled with' we can now look at that
## fitted model in a few ways

summary(myfittedmodel)

#  In terms of fitting a trend line, we would say: on average a one unit (min)
#  increase in time active is assocaited with a reduction in weight of 4.1 grams

#  From the p-value on the test we also know that this effect is statistically
#  significant

#  Based on the R2 value we would also say that the model explains 98.6% of 
#  the observed variation in the data

#  to describe the relationship we would then fit a trend line 

abline(myfittedmodel,col='darkblue',lwd=2,lty=2)

#  Without a linear model, the best 'model' is just the mean 

#  We know the slope is statistically significant but this presentation helps us
#  understand the model theory The alternate to our null is that a line at the
#  sample mean is an appropriate fit to the data So let's add that to the plot

abline(mean(Brushdata$weight.g.),0,col='purple',lty=2,lwd=2)

#  If the null is reasonable the predicted values for all the points 
#  would also just be the mean - so let's put them on the plot too
#  We need the x values to = time active
#  we need the y values to equal the sample mean

#  don't be scared of the code it is your friend!!!!!!

points(Brushdata$time.active, rep(mean(Brushdata$weight.g.), 
                                  length(Brushdata$time.active)), 
       col='purple',pch=16)

#  The difference between the observed data and the points assuming the mean is
#  the best fit to the data can then be calculated as:

differences <- Brushdata$weight.g.-mean(Brushdata$weight.g.)

#  The difference between the observed data and the data assuming the garnd mean
#  is a good fit to the data data can then be illustated

segments(x0=Brushdata$time.active,
         y0=Brushdata$weight.g.,
         x1=Brushdata$time.active,
         y1=Brushdata$weight.g.-differences )

#  So the black lines can be thought of as the errors between the actual data
#  and a model where we assume that the grand mean is a suitable description of
#  the data

pred.amounts <- myfittedmodel$fitted

## and put them on the plot

points(Brushdata$time.active, pred.amounts,col='red',pch=5,cex=1)

## and calculate their differences between the trend line and the actual data

differences.remaining <- Brushdata$weight.g.-pred.amounts

## and show these on the plot
##  the -.1 is just to nudge the values a little so they can be seen

segments(x0=Brushdata$time.active-.1,y0=Brushdata$weight.g.,
         x1=Brushdata$time.active-.1,y1=Brushdata$weight.g.-differences.remaining,
         col='red',lwd=3)

## and calculate their differences to the overall average

differences.explained <- mean(Brushdata$weight.g.)-pred.amounts

##  and show these on the plot
##  again the +.1 bit is just so the values can be seen

segments(x0=Brushdata$time.active+.1,y0=mean(Brushdata$weight.g.),
         x1=Brushdata$time.active+.1,y1=pred.amounts,col='green',lwd=2)

## lets add a legend to help clarify what everything means
legend('bottomleft',
       c('actual points','fitted points','average',
         'total differences','explained by model','not explained by model'),
       pch=c(1,5,16,-1,-1,-1),
       lwd=c(0,0,0,1,2,2),
       lty=c(0,0,0,1,1,1),
       col=c('black','red','purple','black','green','red'),
       bty="n",
       cex=.6 )


#  This is a fair amount of code. Do we know what is going on?

#  The black points equal the actual data observations
#  The red points represent the points predicted by the linear model fitted to the data
#  The purple points represent the points predicted if we assume no linear trend

#  The black lines represent the observed variability in the data The green
#  lines represent the extent of variability explained by the fitted model The
#  red lines represent the extent of observed variability in the data not
#  explained by the model

#  If we sum the squared explained differences 
#  And then sum the error and the total variation i nthe data we have

explained <-  sum(differences.explained^2)
errors <-  sum(differences.remaining^2)
total <- sum((mean(Brushdata$weight.g.) - Brushdata$weight.g.) ^2  )

#  Now to understand R^2 note what we get when we look at the proportion
#  of the variation in the data explained by the model

explained/total

#  Now look at the R^2 value in the summary output.  What do you see?

summary(myfittedmodel)

#  We have been able to work through the mechanics of fitting a linear model
#  and have the looked at the observed (total) variation in the data,
#  which can be divided into explained and unexplained variation,
#  and seen the relationship between the two measures


#  Now let's mix the data up a bit and see what happens
#  Basically this messes up the functional relationship we had found

old_data <- Brushdata$weight.g.
Brushdata$weight.g. <- old_data+differences.remaining*12 +8

#  so given i have messed things up we should find:
#  (i)  the relationship between weight and time active is less clear
#  (ii) the extent of variation in the dta explained by the model is reduced

#  Let's just run through the options
#  Basic plot looks much less clear

plot(weight.g. ~ time.active ,
     las= 1,
     xlab = "Time active (m)",
     ylab = "Weight (g)",
     pch=19, data = Brushdata)     

#  Fit linear model
myfittedmodel <- lm(weight.g.~time.active,data=Brushdata)

#  Look at the summary output
summary(myfittedmodel)

#  In terms of fitting a trend line, we would say: on average a one unit (min)
#  increase in time active is associated with a reduction in weight of 4.1 grams
#  BUT from the p-value on the test we also know that this effect is NOT
#  statistically significant

#  Based on the R2 value we would also say that the model explains 19.7% of the
#  observed variation in the data

#  To describe the relationship we would then fit a trend line 

abline(myfittedmodel,col='orange',lwd=2,lty=6)

abline(mean(Brushdata$weight.g.),0,col='purple',lty=2,lwd=2)
points(Brushdata$time.active, rep(mean(Brushdata$weight.g.), 
                                  length(Brushdata$time.active)), 
       col='purple',pch=16)
Brushdata$weight.g.- mean(Brushdata$weight.g.)
differences <- Brushdata$weight.g.-mean(Brushdata$weight.g.)
segments(x0=Brushdata$time.active,
         y0=Brushdata$weight.g.,
         x1=Brushdata$time.active,
         y1=Brushdata$weight.g.-differences )

###  Note time active was 6.6 for two observations.  Can you see how that is
###  illustrated in the plot?

pred.amounts <- myfittedmodel$fitted
points(Brushdata$time.active, pred.amounts,col='red',pch=2)
differences.remaining <- Brushdata$weight.g.-pred.amounts
segments(x0=Brushdata$time.active-.1,y0=Brushdata$weight.g.,
         x1=Brushdata$time.active-.1,y1=Brushdata$weight.g.-differences.remaining,
         col='red',lwd=3)
differences.explained <- mean(Brushdata$weight.g.)-pred.amounts
segments(x0=Brushdata$time.active+.1,y0=mean(Brushdata$weight.g.),
         x1=Brushdata$time.active+.1,y1=pred.amounts,col='green',lwd=2)

## lets add a legend to help clarify
legend('bottomleft',
       c('actual points','fitted points','average','total differences',
         'explained by model','not explained by model'),
       pch=c(19,2,16,NA,NA,NA), # -1 also works like NA
       lwd=c(0,0,0,1,2,2),
       lty=c(0,0,0,1,1,1),
       col=c('black','red','purple','black','green','red'),
       bty="n",
       cex=.6 )

legend('topright',
       c('data mean','trend line'),
       lwd=c(2,2),
       lty=c(5,2),
       col=c('purple','orange'),
       bty="n",
       cex=.8 )


############################################

#  Non-linear examples for regression

#  A quadratic model

W <- read.csv('winequality.csv')

#  Look at the data structure

head(W)
str(W)

# Temp = average daily temperature during the growing season
# quality = index measure of final wine quality

#  Now plot the data

plot(quality~Temp, data=W)

#  Now we need to do some thinking

#  At low temperatures quality is low
#  At high temperatures quality is low
#  In the middle temperatures quality is generally higher but
#  but there is also greater variability in quality

#  This makes sense:
#  With high and low temperatures the fruit quality is low
#  so the wine quality is low
#  with the medium temperatures fruit quality is high
#  Good wine makers get good wine out of these grapes
#  Very good wine makers get great wine out of their grapes
#  We have low quality variability with high and low temperatures
#  But we have more variability with medium temperatures

#  This also suggests, i think, that  hetro will be a problem

#  Based on our plot it would be crazy to fit a line
#  but lets do it anyway

#  Say we had not plotted our data first but just listen to a
#  friend from Germany that told us quality increases with temperature.
#  This would actually make sense for Germany where it is really a bit to cold
#  for grapes in many parts, and growers would always observe a positive
#  relationship between quality and temperature
#  German grape growers will benefit from (a little bit) of global warming!

#  Let's fit a linear model to the data even though we know it is incorrect

plot(quality~Temp , data=W)
lm1 <- lm(quality~Temp , data=W)
abline(lm1)

#  And let's look at the model output
summary(lm1)

#  Look at the Temp coefficient. What does it say? 

#  Look at the R^2 value.  What does it say

#  Apparently there is no relationship between temperature and quality

#  Gladstones J, (1992) Viticulture and the Environment,
#  however tells me that is nonsense
#  As does the plot!

#  The key lesson here is the need to plot the data as a first step.

#  Based on the plot we think a quadratic model might fit well
#  Lets, try a quadratic model

#  Note the format.  We need the 'I' as an instruction to treat ^ as
#  a mathematical operation  

lm2 <- lm(quality~Temp + I(Temp^2), data=W)

summary(lm2)

#  Look at the summary information -- R^2 and coefficients
#  What does this information tell you?
#####  Turning point comparison

#  We can find the turning point = optimal temperature 

#  For the moment we will not put uncertainty bounds around these estimates

##  For the robust covariance method we have:

B1 <- summary(lm2)$coefficients[2, 1] # grab the coefficient for T
B1  #  make sure of the value you have grabbed
B2 <- summary(lm2)$coefficients[3, 1]  # grab the coefficient for T^2
B2  # make sure of the value you have grabbed

# Find and the peak
TurningPoint <- -B1/(2*B2)

TurningPoint #  Print the turning point

###  Plotting the model

#  With this kind of model we can no longer just use the abline function
#  Rather we need to generate predicted values for a smooth series of x-values

#  Make sure you understand how this works!

preds.temp <- predict(lm2, data.frame(Temp=14:24))
lines(14:24,preds.temp)


#  We then clean up the plot

plot(quality~Temp, 
     xlab = "Temperature C",
     ylab = "Wine quality",
     pch = 16,
     las =1,
     data=W)

#  To make sure the lines are smooth, with high frequency x-values

preds.temp <- predict(lm2, data.frame(Temp=seq(from= 14, to =25, by=.1)))
lines(seq(from= 14, to =25, by=.1), preds.temp, lwd =2, lty =2, col = "grey")


#  add legend
legend("topleft", title= "Wine quality", legend = c("Observation", "Trend"),
       lty=c(NA,2), lwd =c(NA,2), pch=c(16,NA), col=c("black", "grey"), bty= "n" )

# add some text about the maximum point
legend("topright", legend = c("Maximum = 19.4 \u00B0C"), bty= "n" )
#  or here
legend("bottom", legend = c("Maximum = 19.4 \u00B0C"), bty= "n" )

########################################################
