
###############  Exercise 1   ###########################
#
#
#  The worked example is for a single sample t-test
#
########################################################################

#  Read in the Overburden2 file and give it a name in R

#  Remember you need to set the working directory and save the files somewhere 

dirt <- read.csv("overburden2.csv", stringsAsFactors = T)

# First, we check to see what we have read into R

str(dirt)

#  This tells me that i have 85 sample measurements
#  the column heading name for the data is 'sample'.
#  Knowing the number of observations is helpful.
#  With 85 observations we can be confident that
#  the sample mean distribution is approximately normal
#  regardless of the shape of the sample data distribution

summary(dirt)  
length(dirt$sample)
#  This gives me a good summary of the data

#  We can see that the mean is 44.6; the highest measurement is
#  122; and the lowest is 21.

#  We can quickly create a histogram using
#  the base histogram setting

with(dirt,hist(sample))

#  Does the distribution look like a normal distribution? (i would say no)
#  but for our test that is not a problem: recall the CLT and teh sample size

#  As part of preparing structured answer you will need to create nice plots.

#  Now edit your histogram so that it has the following features:

#  Hint: if you go back to the script from last week you will find
#  an example that you can follow

#  (i) the figure title is "Coal mine overburden example"
#  (ii)   the x-axis range is set to run from 0 to 140
#  (iii)  the y-axis range is set to run from 0 to 30
#  (iv)   the figure colour is grey (ie the bars are grey)
#  (v)    the border of the columns is black
#  (vi)   the x-axis label reads "Depth to coal (m)"
#  (vii)  the y-axis label reads "Frequency of observation"  

###################################

#  Now let us move to a formal test t-test.

#  We know that the sample mean is 44.6 metres
#  We also know that the sample distribution does not look normal

#  The specific claim we want to test is the claim made
#  by the mine proponents that the average depth to the
#  coal is 40 metres.

#  From our histogram we can see there are many measurements below
#  40 m, but there is also a long tail of measurements above 40m 

#  To formally test the claim we follow a set formula.

#  Step 1.  Set out the Null and Alternate hypothesis

#  Step 2.  Set the alpha level (always 0.05)

#  Step 3. Implement the single sample t-test

#  Step 4. Interpret the results

#  So for this specific case we have

#  Step 1 
#  Null hypothesis: The mean is 40m
#  Alternate hypothesis: The mean is not 40m 

# Step 2
#  We set the alpha level to 0.05

# Step 3.  Implement the test

with(dirt, t.test(sample, mu =40))

#  Step 4. Interpret the test results

#  The p-value = 0.07881 > 0.05,
#  therefore we do not reject the null.

#  Formally this result means that we can not reject the null hypothesis
#  that the sample is drawn from a population with mean 40m.

#  In practice it means that we have insufficent evidence to dispute the claim
#  being made by the mine proponents.

#  Now, instead of 40m, assume the mine proponents claimed that the
#  average depth to the coal was 35m.

#  Set up an appropriate null and alternate hypothesis to test this claim
#  and work through the steps involved to conduct the test 
#  you should find that the test p-value = 0.0003733

with(dirt, t.test(sample, mu =35))

############################################################
#                                                          #
#     \       \      ___I_   is it a tree test or a        #
#    / \     / \    /\- --\          t-test...             #
#    / \     / \   /  \_-__\                               #
#     |       |    |[]| [] |                               #  
#                                                          #
############################################################


########  Exercise 2 ########################
#

#  This is a two sample t-test example
#
#####################################################################

#  We start by getting the data into R, and then getting a quick summary

coal <- read.csv("coal_test.csv")
str(coal)
summary(coal)

#  The data measure the the energy content of
# the coal in Calories per kg from the point of
#  view of the power station.

#  From the summary we know that, on average, the Australian coal has a 
#  higher energy content than the Chinese coal.
#  Make sure you can find the relevant information in the Summary

#  It is possibly easier to do this in Excel
#  but if we want the exact difference in the means we can use

with(coal,mean(Australia-China))

#  So, the calorific content for the Australian sample is, 
#  on average, 361 Calories per kg higher than for the Chinese sample

#  The first question we ask is: 
#  - is this difference statistically different. 

#  The second question we ask is:
#  - is the difference of practical importance.

#  We are really only interested in the statistical test
#  because we are interested in the question of whether
#  changing the coal source would generate environmental 
#  gains through lower GHG emmissions.

#  As always, we first create some plots
#  to make sure we understand the data
#  we could look at some quick histograms

with(coal,hist(China))
with(coal,hist(Australia))

#  But this does not give us the best comparison

#  We could create a frequency polygon but that is a bit tricky.
#  Let's use a boxplot instead.

with(coal,boxplot(China, Australia))
with(coal,boxplot(China, Australia, names =c("Australia","China")))

#  Make sure you understand the above lines

#  Is there anything wrong with this plot?
#  Let's look at the summary again
summary(coal)

#  Based on the summary we should expect to see that the plot for Australia is
#  higher than for China.  At the moment the lables are in the wrong place.
#  Can you change the category labels such that they are in the right order?

with(coal,boxplot(China, Australia, names =c("China","Australia")))


#  If you wanted to clean up the picture you might uses some of the following

with(coal,boxplot(China, Australia, 
                  names =c("China","Australia"),
                  main ="Comparison of coal energy content",
                  ylab = "Energy content (kj per kg)",
                  xlab = "Source of the Coal",
                  ylim = c(5600, 8000),
                  boxwex=0.6,
                  col ="grey",
                  cex.main=1.2, cex.lab=1.2))

#  From the figure what do you learn?
#  To me it looks like there might be some difference
#  but it is not certain.

#  In this instance, based just on looking at the plots
#  I am not convinced there is a difference.
#  We need a formal approach to testing.


#  Before we conduct the two sample t-test to test
#  for a difference in the means we conduct the variance ratio test

#  We use the result of the variance ratio test to decide which type of
#  t-test to use.

#  We can find the variance of each sample as:

with(coal,var(China))
with(coal,var(Australia))

#  The ratio of the variances can be found as:

#  Let's implement the variance ratio test formally:

# Step 1.  Set out the Null and Alternate hypothesis

#  Null hypothesis: Variance of the Australia sample is equal 
#                   to the variance of the China sample

#  Alternate hypothesis: Variance of the Australia sample is not equal 
#                   to the variance of the China sample

# Step 2.  Set the alpha level 

#  We will use an alpha level of 0.05 as our decision criteria
#  so if the test p-value is less than 0.05 we reject the null

#  It is important that we set the alpha level
#  before we actually conduct the test.

#  Step 3. Implement the test 

with(coal,var.test(Australia,China))

#  Step 4.  Interpret the test result

#  From the test result we see that the ratio of the variances is not equal

#  We know this because the p-value = 8.076e-06 < 0.05

#  With a low p-value we reject the null hypothesis of equal group variances

#  We therefore use the t-test with unequal variance formula to test for a difference
#  in the group means

##  Note.

#  What about the order of the data in the test?
#  If we change the order the data is read in
#  that will change the ratio. 
#  However, the test p-value will not change

#  we can check as follows:

with(coal,var.test(China, Australia))

#  So the ratio is now 0.4 (45,671/113,984/ = 0.40) 
#  but the p-value is unchanged. (p-value = 8.076e-06)
#  So, as long as we focus on the p-value
#  we don't need to worry about the order we
#  enter the data.  We always reach the same conclusions.
#  Although there are many limitation with the 
#  p-value decision rule, the simplicity of application
#  is one of the attractive feature of using this approach


#  Having completed the variance ratio test we now
#  implement the two sample t-test.  

#  Formally we set this out as follows.

#  Step 1.  Set out the Null and Alternate hypothesis

#  Null hypothesis: Mean of the Australia sample is equal 
#                   to the mean of the China sample

#  Alternate hypothesis: Mean of the Australia sample is not equal 
#                   to the mean of the China sample

#  Step 2.  Set the alpha level -- always 0.05

#  Step 3. Implement the t-test with unequal variance 

with(coal,t.test(Australia, China, var.equal = FALSE))

#  Step 4.  Interpret the test result

#  The test p-value is a very small number (p-value = 3.835e-16)
#  the conclusion we draw is that the energy content of the
#  Australian coal and the Chinese coal are different.

##  The confidence interval information

#  The Confidence Interval information tells us that
#  we think the true difference in the energy content
#  between Australian and Chinese coal is probably somewhere between 
#  282.21 Calories per kg and 439.97 Calories per kg

#  If we test any null hypothesis between these to values
#  we will not reject the null.

#  Let's try the following test: 
#  Null hypothesis: difference = 300
#  Alternate hypothesis: difference not = 300

with(coal,t.test(Australia, China, var.equal = FALSE, mu =300))

#  What is the p-value for this test?
#  Do you reject the null? 

#  Experiment with different values for mu and
#  think about the way the test result changes 

#  So, we have established that the calorific content
#  of the China coal and Australian Coal are different.

#  In this example the difference is about 5%.

#  There is no natural base period for the percent change 
#  calculation comparison. So i would use the average as the base.
#  This means i use the following to calcualte the % change

# (mean aust - mean china)/ (average of china and australia) 

(6888-6527)/((6888+6527)/(2))*100

#  Do you understand where these values have come from?

#Log difference

#(log(6888)-log(6527))*100    

#  In practice there are many factors to think about:
#  - what are the extra transport emissions to ship the 
#    coal from Australia to China?
#  - what is the actual power plant technology?
#  - what is the the actual energy transformation process?
#  - are there any fugitive emmisions during the mining process?

#  But given (i) the significant role for coal in the electricty mix
#            (ii) the size of the generation sector in China
#   A 5% improvement in the energy content of the base feed stock
#   seems a a meaningful improvement.

#  We we have used a formal test to to establish something useful

#########################################################
#                                                       #
#    .-"""-.      ... coal keeps the lights on          #
#   /* * * *\             ... but the mushroom          #
#  :_.-:`:-._;                ... prefers the dark      # 
#      (_)                                              #
#   \|/(_)\|/   source: chris.com                       #  
#                                                       #
#########################################################

#######  Excersie 3 

#  We start by reading in the data
#  and looking at what we have

salt <- read.csv("salt_test.csv", stringsAsFactors = T)

str(salt)

#  The observations are matched pairs.  
#  the measurements are taken at fixed locations
#  along the river system.
#  There are 42 observations.
#  The data is a record of the saline level of the water
#  the unit of measurement is Electrical Conductivity

#  "locationID" is a record of where the sample is from
#   along the river system.
#  "before" is the measurement at a specific location before
#   the technology intervention
#  "after" is the measurement at a specific location after
#  the technology intervention.


#  Making a mental note of the number of observations is important.
#  Recall that once we have a reasonable sample size,
#  such as a sample of 42, we know that the
#  sample mean distribution will be approximately normal.
#  so our testing appraoch is OK.

#  The context for the test is as follows.

#  The power company is discharging saline water into the river system.
#  Even though the company has approval to do this, there has been a public
#  campaign to highlight the damage that the discharge is having on the
#  the environment.

#  The company is willing to spend $5 million on an intervention
#  to lower the saline level of the water being discharged 
#  if the intervention drops the Electrical Conductivity
#  of the water being discharged by 50 units.

#  This is the reduction that the company thinks would justify
#  spending the $5 million in capital required for the intervention.

#  At the moment they are trialling the intervention.
#  If it works they will then spend the money to make the
#  intervention permanent.

#  We find the sample means as:

with(salt,mean(before))
with(salt,mean(after))

#  and the difference between the means as:

with(salt,mean(before - after))

#  The difference is approximately 40 EC units.
#  So we know that, on average, the measurements 
#  are lower after the intervention.
#  But is this a real effect or just random
#  variation due to sampling error?

#  Now we can quickly look at a histogram of each sample
#  Because we are just checking things out, not preparing a proper plot
#  It is OK to just use the default settings

with(salt, hist(before)) 
with(salt,hist(after))

#  Neither sample looks like a normal distribution
#  but for testing purposes that does not matter.
#  Why does it not matter?  
#  -- if this question has you confused, ask me what is going on.

#  One way to look at the difference before and after
#  the intervetion is through a boxplot

with(salt,boxplot(before, after))

with(salt,boxplot(before, after, names =c("Before", "After"),
                  main ="Comparison of water salinity ",
                  ylab = "Electrical Conductivity units (EC)",
                  xlab = "The effect of the intervention",
                  ylim = c(300, 420),
                  boxwex=0.6,
                  col ="grey"))

###########################################################

####  Important point.

#  Because the data are paired, a boxplot of two groups is not the most
#  effective plot to use.  The plot does not make use of
#  the paired information.

#  A more appropriate plot is either a histogram of the difference series
#  or a boxplot of the difference series, which is a boxplot with only one group  

#  We first create the difference series

diff.series <- with(salt, before - after)

#  Then we look at the values

diff.series

#  We can see that generally the water saline content 
#  was higher at the sample points before the intervention
#  but in a few cases the the measurement after the intervention
#  is actually higher (a negative value)

#  We can also look at the distribution of this difference series

hist(diff.series)

#  we can also calculate the differences as part of the histogram function

with(salt, hist(before-after, col ="grey"))

###############
#  Again, this difference series does not look like a normal
#  distribution, but that is OK for testing because we have a reasonably large sample.

#  To establish whether or not the intervention
#  had any effect we use a paired t-test.

#  Becuase this test works on the difference series there is no need for a variance ratio test

# Step 1.  Set out the Null and Alternate hypothesis

#  Null hypothesis: Mean of the difference series is zero

#  Alternate hypothesis: Mean of the difference series is not zero 

#  Step 2.  Set the alpha level -- always 0.05

#  Step 3. Implement the paired t-test
#         Pay attention to the format of the test commands

with(salt,t.test(before,after, paired =TRUE))

# Step 4. Interpret the results

#  As the p-value = 1.762e-10 < 0.05, we reject the null.
#  That means we conclude the intervention had an effect.

#  Note that the paired t-test is the same as a single sample t-test on
#  the difference series.  We can confirm this using:

with(salt,t.test(diff.series))

#  note the p-value for the test is the same

#  The specific test the company was interested in was
#  if the difference was 50 EC units.

#  We can investigate this issue as follows.

# Step 1.  Set out the Null and Alternate hypothesis

#  Null hypothesis: Mean of the difference series is 50

#  Alternate hypothesis: Mean of the difference series is not 50 

#  Step 2.  Set the alpha level -- always 0.05

#  Step 3. Implement the paired t-test

with(salt,t.test(before,after, paired =TRUE, mu =50))

# Step 4. Interpret the results

#  As the p-value = 0.04086 < 0.05, we reject the null.

#  That means the company is not willing to install the technology.

#  However, let us think carefully about the 0.05 decision rule.
#  we set this threshold before we conducted the test
#  and so we have to follow the decision rule.
#  but the result is very close to 0.05

#  If our null hypothesis had been 49 rather 50
#  we would have

with(salt,t.test(before,after, paired =TRUE, mu =49))

#  and with our p-value decision rule we would
#  not reject the null that the mean of the difference 
#  series is 49.(p-value = 0.06436 > 0.05)

#  We would then conclude that the power company can install 
#  the technology.

#  So if we had a slightly different threshold 
#  for the improvement we are seeking then we would
#  conclude that we should install the technology.

#  There is not necessarily any one correct
#  way to proceed in such a situation,
#  but it is important to not think just about the 
#  p-value rule but also look at the context.

#  One appraoch that some people prefer is to report the
#  95% confidence interval. 

#  The 95% confidence interval is also reported as part of the 
#  standard test result.

#  For this case we could say that the 95% confidence interval for the effect
#  of the intervention is between 30.4 EC units and 49.6 EC units.

#  If we report the result this way the company can think about this information and
#  come to their own conclusion about the effect and whether it is 
#  a worthwhile intervention. 


###################################################################
#   _____________________                                         #
#  |  _________________  |                                        #
#  | | JO           0. | |                                        #
#  | |_________________| |  R, better than a calculator...        #
#  |  ___ ___ ___   ___  |                                        #
#  | | 7 | 8 | 9 | | + | |                                        #
#  | |___|___|___| |___| |                                        #
#  | | 4 | 5 | 6 | | - | |                                        #
#  | |___|___|___| |___| |                                        #
#  | | 1 | 2 | 3 | | x | |                                        #
#  | |___|___|___| |___| |                                        #
#  | | . | 0 | = | | / | |                                        #
#  | |___|___|___| |___| |  source: J.J Olson                     #
#  |_____________________|                                        #
#                                                                 #
################################################################### 

