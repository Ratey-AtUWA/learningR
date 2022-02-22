  
#  Remember to set your working directory to wherever you store your 

#####################################################
#       Read the data and look at it  #

tuna.data <- read.csv("TunaMF.csv", stringsAsFactors = T)
head(tuna.data)
str(tuna.data)
summary(tuna.data)
tuna.data
#####################################################################

#  Guided example 1

#  Non-parametric comparison of two groups: unpaired

#  The issue is that we have 7 observation for male tuna and
#  5 observations for female tuna.  We are therefore worried
#  about appealing to the central limit theorem as the basis
#  for a two sample t-test.

#  As such, we choose to use a non-parametric (np) method.
#  For two groups we use the Mann-Whitney test

#  We read in the data and look at it in the usual fashion.

#  Note that even though we only have two groups the data is still in long format 

#  A standard form for a boxplot might be as follows

with(tuna.data,boxplot(Fish.cm~Type,
                       names =c("Females", "Males"),
                       ylab = "Tuna length (cm)",
                       xlab = "Tuna",
                       main = "Don't add title as you will double up with Word caption!",
                       boxwex=0.6,
                       col = "grey",
                       horizontal = F,
                       cex.axis=0.9, cex.lab=1.0,cex.main=0.7))

#  Based on the plot we should be quite sure that there are 
#  real differences between males and females.  Do you agree?
#  If so why do you agree?

#  Although the plot looks pretty clear still want to use a formal test.

#  For non-parametric methods we do not have any assumption about the group
#  variances, so we jump straight to our test

#  The null for our test is:
#  The length of male and female tuna is the same

#  The alternative is:
#  The length of male and female tuna is not the same

#  Note: we do not mention the mean when we state the null and alternate
#  hypotheses!!

#  We conduct the test using the wilcox command
#  The test format is very similar to the t-test format

with(tuna.data,wilcox.test(Fish.cm ~ Type))

#  The test p-value = 0.018, so we reject the null.
#  We conclude that the length of male and female tuna are not the same

#  Based on the boxplot we conclude that male tuna are larger
#  Recall we use the test to establish if there is a difference
#  And then the plot, or some other metric, to determine the direction of the difference

################################################################################

#  With modern computer power some people suggest non-parametric methods 
#  are no longer required.  The 'Bootstrap method' is the alternative.

#  The idea with the Bootstrap is that we draw 'samples' from our initial sample,
#  and then use these 'samples' to generate an implied distribution of sample mean.

#  When I make up data in the course I sometimes use these methods.

#  You can download pre-baked code for R in the form of packages.
#  The 'boot' package will do some of the things the below code will do
#  but not everything..

#  The below is an application of data science: combining computer code smartly to
#  solve an applied statistics problem. Actually, the teaching team doesn't have that
#  much in computer science smarts, so I am pretty sure there are more efficient ways to
#  code this.  But it will work fast enough for any number crunching we will do.

#  First let's create a dataframe to work with. 
#  Here we just grab the Male fish values from the data we have loaded.

#  Select just the male fish length values
test.data <- with(tuna.data, Fish.cm[Type == "Males"]) 

#  Look at the data.frame you have created
test.data

#  We can use the length() function to work out how many values we have
#  in the data object
length(test.data)

#  The sample() function takes randomly selected values from
#  the base data set that you tell the function to sample from

# Draw ten values from the data we have added. What do you see?
sample(test.data, size =10, replace = TRUE )

#  You can also specific the size of the sample as a multiplication, 
#  so if you need 25 values you can write 5*5. Sometimes this is easier.
sample(test.data, size =5*5, replace = TRUE )

#  You can create blocks to hold numbers with the matrix() function

#  Create a block with four rows and seven columns 
matrix( nrow = 4, ncol = 7, byrow = TRUE)

#  A three by three matrix with the value 0
matrix(0, nrow = 3, ncol = 3, byrow = TRUE)

#  A three by three matrix with values filled by row. What values are we using?
matrix(seq(1,9,1), nrow = 3, ncol = 3, byrow = TRUE)

#  A three by three matrix with the value filled by column
matrix(seq(1,9,1), nrow = 3, ncol = 3, byrow = FALSE)

#  Let's go back to our test.data.
#  As we have seven observations in the test.data, we would fill a 
#  four by seven matrix with the same values in each row
matrix(test.data, nrow = 4, ncol = 7, byrow = TRUE)

#  We can then save the matrix as: 
test.samples.row <- matrix(test.data, nrow = 4, ncol = 7, byrow = TRUE)

#  The apply() function can be used to calculate means  
apply(test.samples.row, 1, mean) # calculate the mean of each row
apply(test.samples.row, 2, mean) # calculate the mean of each column

#  Now we can combine functions.
#  This code chunk says, draw 700 random values, from the test.data,
#  and then fill up a matrix that is 100 rows long and 7 columns wide 
#  with the sample values.

m.test.samples <- matrix(sample(test.data, size = 7*100, replace = TRUE), 
                         nrow = 100, ncol = 7,  byrow = TRUE) 

# Check the size (dimensions of the matrix)                                  
dim(m.test.samples)

#  Next we create a series of the mean of each row. How do you show the 
#  means in the Console window?
test.sample.mean <- apply(m.test.samples, 1, mean)

#  Now we can plot the histogram of the means we just saved
hist(test.sample.mean)

#  A density plot is a histogram where the width of the bins tends to zero. 
plot(density(test.sample.mean), main = "Bootstap density curve ")

#  Derive a 95% confidence interval as: 
quantile(test.sample.mean, probs = c(0.025, 0.975))

#  In terms of testing, we would not reject any values in the
#  the CI as plausible values

############# Now with a different set of data ##############################
#  We can use the bootstrap concept with all the values hardwired as follow
#  ('hardwired', i.e. we define the values ourselves)

#  add the values and run the sampling to fill in the matrix
f.samples.hardwired <- matrix(sample(c(178,173,168,165,163), 
                       size = 5 * 1000,
                       replace = TRUE),
                       nrow =1000,
                       ncol = 5,
                       byrow = TRUE)

#  check the dimension are correct
dim(f.samples.hardwired)          

#  Find the mean of each row
hardwired.mean <- apply(f.samples.hardwired, 1, mean)

#  check what the bootstrap distribution looks like.
hist(hardwired.mean)
plot(density(hardwired.mean))

#  Find a 95% Confidence interval
quantile(hardwired.mean, probs = c(0.025, 0.975))

#  But the real power is when you do not hardwire the values, but instead use the 
#  raw data values....

#  First set the samples= size you want, so you can change this easily later if needed.
#  Run this line
no.samples=1000

#  Now do the coding to work with some real data values
f.samples <- with(tuna.data, matrix(sample(Fish.cm[Type == "Females"], 
                 size = length(Fish.cm[Type == "Females"])*no.samples,
                 replace = TRUE),
                 nrow = no.samples,
                 ncol= length(Fish.cm[Type == "Females"]),
                 byrow=TRUE))

#  Here I do the same thing for the Male data, which has different dimensions
#  but that doesn't matter, because I am setting the number of column to be 
#  the same as the Male data length
m.samples = with(tuna.data, matrix(sample(Fish.cm[Type == "Males"], 
                size = length(Fish.cm[Type == "Males"])*no.samples,
                replace = TRUE),
                nrow = no.samples, 
                ncol= length(Fish.cm[Type == "Males"]),
                byrow=TRUE))

#  Now I get the mean of each row all my samples
#  The code says read across each row of the data matrix and calculate the mean
#  value, and save those value in an object called female.means and male.means 

female.means <- apply(f.samples, 1, mean)
male.means <- apply(m.samples, 1, mean)

#  Now for each sample I calculate the difference in the mean for each row
boot.stat <- male.means - female.means
 
#  Here I derive the 95% Confidence Interval for my bootstrapped samples.
#  This defines the do not reject zone.  If zero is in this range I will
#  not reject the null of no difference in the means of these sample

quantile(boot.stat, probs = c(0.025, 0.975))

#  Because zero is not in the 95% CI, I reject the null.
#  Looking at teh means, Male tuna are the larger fish

#  Now I create a plot to show my distribution, the 95% confidence 
#  interval, and the non-reject zone

#  You really just want to run the code here as a block and look at the 
#  plot.  This is starting to get advanced (i.e. scary for some)
#  I first generate a bunch of objects, and then plot those
quant <- quantile(boot.stat, probs = c(0.025, 0.975))
boot.den <- density(boot.stat)
low.ci <- quant[1] 
hi.ci<- quant[2]
x1b <- min(which(boot.den$x >= low.ci))  
x2b <- max(which(boot.den$x <  hi.ci))
plot(density(boot.stat), 
     main = "The bootstrap distribution",
     xlab = "Difference (Males - Females in cm")
with(boot.den, polygon(x=c(x[c(x1b,x1b:x2b,x2b)]), 
                       y= c(0, y[x1b:x2b], 0), 
                       border = "gray70",
                       col="gray70"))
legend("topleft", legend ="Do not reject zone", fill ="grey", bty ="n", border="grey", cex=0.8)

#  You can do the same thing for the medians
#  You just change the requested statistic

female.meds = apply(f.samples, 1, median)
male.meds = apply(m.samples, 1, median)
boot.meds = female.meds - male.meds
quantile(boot.meds, probs = c(0.025, 0.975))
#  conclude from the 95% CI that we would reject the Null

##########################################################

#  Note that for medians you can also check the result
#  using some additional features in the wilcox.test function.

#  We are not using this test as a test of  medians in the other computer
#  lab exercises, but you could do this and would then get the 95% CI as follows
with(tuna.data,wilcox.test(Fish.cm ~ Type,
                           conf.level=0.95,
                           alternative="two.sided",
                           correct=F, 
                           conf.int=T))

#  Essentially the same result as the bootstrap, but there we covered some computing.
