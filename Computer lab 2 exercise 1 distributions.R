
rm(list = ls())
# Many processes can be closely approximated by the normal distribution
#  For the normal distribution 68% 95%, and 99.7%  are the  
# probabilities for falling within 1, 2 and 3 standard deviations of the mean

#  Reference point for looking at the standard normal distribution

plot(density(rnorm(100000, mean =0, sd= 1) ,adjust=1.5), lwd=2, las=1, 
     xlim=c(-4,4), main="Standard normal distribution" , xlab = "Standard deviations from zero") 
segments(x0=-1, y0=0 ,x1=-1, y1=.25 )
segments(x0=1, y0=0 ,x1=1, y1=.25 )
segments(x0=-2, y0=0 ,x1=-2, y1=.08, lty=2  )
segments(x0=2, y0=0 ,x1=2, y1=.08, lty=2 )
segments(x0=-3, y0=0 ,x1=-3, y1=.05, lty =3  )
segments(x0=3, y0=0 ,x1=3, y1=.05, lty =3 )

legend("topleft", legend =c("one SD", "two SD", 'three SD'), lty =c(1,2,3), bty ="n", cex=0.8)

# This provides a quick reference for thinking about the normal distribution
# we can see that by two standard deviations from the mean (which is zero) we have almost
# all the distribution covered.  

# There are several inbuilt R funtions that allow you to look at distributions
#  Let's explore some functionality

#  all probability area up to one st. dev above the mean
#  we can look up this as follows:
pnorm(q = 1, mean = 0, sd = 1, lower.tail=T)

#  all probability area up to one st. dev below the mean 
pnorm(q = -1, mean = 0, sd = 1, lower.tail=T)

#  The probability area between plus and minus one sd from the mean
pnorm(q = 1, mean = 0, sd = 1, lower.tail=T) - pnorm(q = -1, mean = 0, sd = 1, lower.tail=T)

#  The area between plus and minus two sd from the mean
pnorm(q = 2, mean = 0, sd = 1, lower.tail=T) - pnorm(q = -2, mean = 0, sd = 1, lower.tail=T)

#  The area between plus and minus three sd from the mean
pnorm(q = 3, mean = 0, sd = 1, lower.tail=T) - pnorm(q = -3, mean = 0, sd = 1, lower.tail=T)

# the same logic carries over to distribution that are not standard normal, but just normal

#  as discussued in the distributions chapter of the textbook
# brushtail possums follow a nearly normal distribution
# mean 92.6 mm and standard deviation 3.6 mm

# Reference plot to help you visualise what this might look like
poss.sample <- rnorm(10000, mean =92.6, sd= 3.6)
length(poss.sample)
mean(poss.sample)
sd(poss.sample)

hist(poss.sample, freq = F)
lines(density(poss.sample))

# You can smooth out the plots or make them more jagged with by 
#  adjusting the smoothing parameter

plot(density(poss.sample, adjust=1.5 )) #  more smooth looking
plot(density(poss.sample, adjust=0.5 )) #  more jaggard

# you can put the plots together using the lines function
plot(density(poss.sample, adjust=1.5 ), lwd=2, lty=1, xlim =c(75,110), ylim=c(0,0.12), col="grey")
lines(density(poss.sample, adjust=0.5 ), lwd=2, lty=2 )

#  For the brushtail possums distribution, 
#  What is the approximate probability of observing a value of 100 or less 
pnorm(q = 100, mean = 92.6, sd = 3.6, lower.tail=T)

#  For the brushtail possums distribution, 
#  What is the approximate probability of observing a value above 100 
pnorm(q = 100, mean = 92.6, sd = 3.6, lower.tail=F)


#  The approximate probability of observing a possum that lies between 
#  plus and minus one standard deviation of the mean
pnorm(q = 92.6+3.6, mean = 92.6, sd = 3.6, lower.tail=T) - pnorm(q = 92.6-3.6, mean = 92.6, sd = 3.6, lower.tail=T)
#  The area between plus and minus one sd is what we would expect

#  Edit the text to calculate the probability of being between plus/minus two?

#  What is the (approx.) probability of observing a possum of at least 96mm?


#  What is the (approx.) probability of observing a possum smaller than 86mm?


#  There are several in-built random number generators

#  generate values from a normal distribution
#  set the mean and the sd
values.normal <- rnorm(n = 5000, mean = 100, sd =10 )

#  generate values from a log normal distribution
#  set the mean and the sd, on the back log transformed scale
values.lnorm <- rlnorm(n=5000, meanlog = 0, sdlog = 1)

#  generate values from a uniform distribution
#  set the max and min range to draw vaues from

values.uniform = runif(n = 5000, min = -20, max =20 )

#  For the poisson distribution the mean = sd
#  and so there is a single value to set
values.poisson = rpois(5000, lambda = 5)


#  Visualise the various plots

hist(values.normal)
hist(values.normal, breaks = "scott") #  change the bin size

plot(density(values.normal, adjust=1.5))

hist(values.lnorm, breaks = "scott")
plot(density(values.lnorm))

mean(values.lnorm)
mean(log(values.lnorm)) #  should be approximately zero
sd(log(values.lnorm))  #  should be approximately one
plot(density(log(values.lnorm), adjust=1.5)) #  should look like a normal curve

#  undo the log transformation with the exp() transformation
plot(density(exp(log(values.lnorm)), adjust=1.5)) #  should look like a normal curve

hist(values.uniform)
plot(density(values.uniform))

###############################################################################

#  sometimes we are interested in formal tests of whether a
#  distribuion is normally distributed.

#  Generally, the type of statement we make is that we have 
#  either sufficent or insufficent evidence to reject the null,
#  where the null is that the sample can be considered as a
#  normal distribution.

#  One such test for normality is the Shapiro-wilk test
#  Let us start by checking the help pages for this test

?shapiro.test

#  So the test needs: "a numeric vector of data values".
#  If you scroll to the bottom of the help pages you will find
#  some example material. This example is very helpful.  
#  For this kind of test we have a p-value
#  decision rule.  If the test value is less than 0.05, we reject the null.
#  A high (above 0.05) p-value means that we have insufficent evidence to reject the null
#  But what is the null?  If we are unsure we can run the example code, which i have pasted below  

#  Example 1.  We know this data comes from a normal distribution
#  If we run the test we get a relatively high p-value
shapiro.test(rnorm(100, mean = 5, sd = 3))

# Example 2.  We know this data comes from a uniform distribution
#  If we run the test we get a relatively low p-value
shapiro.test(runif(100, min = 2, max = 4))

#  So with this approach of just trying differnt examples, we have been
#  able to work out how the test works.  Low p-values indicate sufficient evidence
#  to reject the null that the sample data is drawn from a normal distribution.  

# Let's  now check this with the values we have previously created.

#  normal distribution
shapiro.test(values.normal) 
#  High p-value, so insufficent evidence to reject the null that the data is from a 
#  normal distribution 

#  log normal distribution
shapiro.test(values.lnorm)
#  very small p-value, so sufficent evidence to reject the null that the data is drawn from a
#  normal distribution

#  uniform distribution
shapiro.test(values.uniform)
#  very small p-value, so sufficent evidence to reject the null that the data is drawn from a
#  normal distribution

#  poisson distribution
shapiro.test(values.poisson)
#  very small p-value, so sufficent evidence to reject the null that the data is drawn from a
#  normal distribution


#  The qq-plot is another visual aid to look at distributions
#  For these plots, the data should fall approximately along the line,
#  if the data is normally distributed.

#  observations basically along the line
qqnorm(values.normal, col="grey"); qqline(values.normal, col = 2)

#  Shows some very extreme variation, so not a normal disribution
qqnorm(values.lnorm, col="grey"); qqline(values.lnorm, col = 2)

#  Again data not close to the line, not a normal distribution
qqnorm(values.uniform, col="grey"); qqline(values.uniform, col = 2)

#  This shows the data is very different
qqnorm(values.poisson, col="grey"); qqline(values.poisson, col = 2)

#  Note the Poisson distribution contains only integers,
#  you can see this if you look at the first ten values
head(values.poisson, n=10)
#  That is why the plots look the way that they do

#####  Warning on tests #########
# 
# When you have a small sample, statistical tests do not have much power
# to detect a difference even if there is a difference 

# This issue can be made concrete with an example. The Shapiro-Wilk
# test is probably the best performing test to identify departures from
# normality, so I use this test to illustrate. 

# Simulation studies have shown that if we draw data from a uniform distribution --
# ie something that is very different to a normal distribution --
# for a sample size of 20, the power of the test to detect that the
# sample is not normal, with alpha=0.05, is only 0.20. 

# Recall that test power is a measure of the chance we have of detecting a difference
# if there really is a difference. If we have a sample of 30, then the
# power of the test with alpha=0.05 is still only 0.38. So when
# we have small samples, we have very limited ability to detect that
# we have an issue. With small samples, in general, even when we know
# that the data is not normally distributed we will in many cases not reject
# the null hypothesis of normally distributed errors. With a sample
# size of 50, and data drawn from a uniform distribution the test power is
# 0.74, and by the time we have increased the sample size to 100, test
# power is approaching one.

#  The issue is illustarted below, where we know in all three cases
#  the data comes from a uniform distribution, and 
#  not a normal distribution, but only for the sample of 50
#  do we conclude that we have sufficent evidence to reject the
#  null

set.seed(123)
uniform10 = runif(n = 10, min = -10, max =10 )
shapiro.test(uniform10)

#  Look at the test p-value.  do you understand how to interpret the result? 

set.seed(123)
uniform20 = runif(n = 20, min = -10, max =10 )
shapiro.test(uniform20)

#  Look at the test p-value.  do you understand how to interpret the result? 

set.seed(123)
uniform50 = runif(n = 50, min = -10, max =10 )
shapiro.test(uniform50)
#  Look at the test p-value.  do you understand how to interpret the result 


#  For small samples qq plots are also not that helpful.
#  there just is not enough data to reach strong conclusions 
#  about anything.  Such plots can still be a useful complement
#  to looking at the data

#  Hard to tell anything
qqnorm(uniform10); qqline(uniform10, col = 2)

#  OK sort of looks like a problem in the tails, but it still 
#  only a few data points

qqnorm(uniform20); qqline(uniform20, col = 2)

#  starting to see an issue, but still not convincing
qqnorm(uniform50); qqline(uniform50, col = 2)

#  Good old fashioned historgrams or density plots, while
#  not a formal test are also helpful.

#  none look like approaching the bell curve shape
hist(uniform10)
hist(uniform20)
hist(uniform50)

#  none look like approaching the bell curve shape
plot(density(uniform10, adjust=0.8)) 
plot(density(uniform20, adjust=0.8))
plot(density(uniform50, adjust=0.8)) 

########################################################################

