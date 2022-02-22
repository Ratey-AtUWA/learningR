#  Exercise 1. ANOVA and multiple comparisons 

#  When we have multiple groups we start with an ANOVA test, and then move on to 
#  a 'family' of formal t-tests of the individual groups.

#  The fish data file contains measurements on the weight at harvest for three 
#  different strains of Tilapia.  The strains are under consideration as part
#  of an aquaculture project to decrease the pressure on wild fish stocks.

#  The three Tilapia strains are all new strains that have been developed as part 
#  of an industry funded fish breeding program.  The researcher is interested in 
#  identifying which of the  strains has the highest average yield (weight). The fish  
#  strain with the highest average yield (weight) will then be used in a large scale trial.

#  Read in the data and have a look at what you have:

fish <- read.csv("Tilapia1.csv", stringsAsFactors = T)

str(fish)   

#  The summary tells us we have a factor variable "Strain" with 3 levels.

#  The three levels of the factor are our three fish varieties A, B, and C.

#  We then have matching values in the weight column of the data.
#  These values in the weight column record the yield in kg per fish.

# We can see the data is in long format from:

head(fish)

#  The formula for our boxplot says plot the data in the 
#  result column grouped by treatment (which are our three varieties) 

#  Note the formula is different to when we have several individual columns of data

with(fish,boxplot(weight~Strain))  

#  Our boxplot tells us that Strain A might be different 
#  to Strain B but there are no striking differences   

##################################################

#  As we have multiple groups we use a test to establish
#  whether or not it is OK to assume constant variance for each group

#  There are many options for testing the constant variance assumption
#  I found a review paper that had 14 different test options!

#  With these tests the null is that the variance of each group is the same.
#  With a low p-value we reject the null of equal variance across the groups

#  Although there are many options we will just use one test

####  Constant variance test with multiple groups: the  bartlett test

#  Formally the test relies on the chi-square distribution and  
#  the way log based index numbers for dispersion can be calculated.

#  If there are large differences in the variance of each group,
#  the test statistic will be a large number.

#  When the test statistic is large the p-value is low and
#  we reject the null of equal group variance.

#  Formally we set up a Null hypothesis and Alternate hypothesis as:

#  Null: The group variances are all equal
#  Alternate: The group variances are not equal

#  Set alpha at 0.05 (which controls the type I error rate) 

#  The formula for the test says look at the weight column and these values
#  are grouped by the value listed in the Strain column.

with(fish,bartlett.test(weight~Strain))

#  The test p-value (0.7769) is greater than 0.05, therefore we do not reject the null of 
#  equal variance across the groups.  

#  STOP.  Look at the boxplot.  Are you surprised by the test result?

#  If we actually look directly at the boxplot
#  we can see the distributions are pretty similar.

#  It should not be surprising that we do not reject the null of equal
#  group variances.

#  We now conduct our ANOVA test.
#  We implement the test using the oneway.test() function 
#  "oneway" stands for one way ANOVA.

#  We will use the test format that uses the equal 
#  group variance assumption (because we did not reject the null for the
#  bartlett test)

#  Formally we need a Null and Alternate hypothesis

#  Null: the group means are all the same
#  Alternate:  at least one of the group means is different

#  Next we set the alpha level at 0.05.

with(fish,oneway.test(weight~Strain, var.equal=TRUE))

#  The null for the ANOVA test is that the mean for each group is the same

#  The p-value = 0.005597 is less than 0.05, therefore we reject the null.

#  We conclude that there is at least one difference in the groups.  

#  From the boxplot it was hard to be sure there was a difference.
#  The formal test confirms there is actaully a difference

#  As we have multiple comparisions we need to be aware of the impact 
#  of multiple comparisons on the type 1 error rate for t-tests. 

#  When using the pairwise.t.test() command it includes
#  an automatic adjustment for multiple comparisons.

#  There is no single correct adjustment for multiple comparisons.

#  The default option with the pairwise.t.test is the 'holm' adjustment.

#  This adjustment is fine as an option to use unless otherwise directed.

#  We implement the multiple comparison test as follows. 
#  Because we did not reject the null of equal group variances
#  we set the pooled sd to TRUE.
#  This is the same as setting var.equal = TRUE in the t.test() formula.

#  What is an appropriate null and alternate hypothesis for the t-test?

with(fish, pairwise.t.test(weight,Strain, pool.sd = TRUE))

#  As the comparisons are symmetric, only results are printed in the
#  the lower triangle

#  The table gives the summary p-values for the pair-wise tests

#  The results of the adjusted t-test say:
#  (i)   Strain A is different to Strain B (p-value = 0.004)
#  (ii)  Strain A is not different to Strain C (p-value = 0.16)
#  (iii) Strain B is not different to Strain C (p-value = 0.16)

#  There are other adjustment options for multiple comparison testing, 
#  including no adjustment at all!

#  It is however safe to alway use the holm adjustment (which is the default)

###########################################################################
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#          .               `         /
#                           .    ,../...       .
#           .                .  /       `\  /  .
#      \    .        o         < '  )     =<
#      /\  .                    \ \      /  \   .  __
#    >=)'>                       `'\'"'"'         /o \/
#      \/ .    /         o              /,        \__/\    .:/
#      /   .  /--\ /         /         <')=<     .      ,,///;,   ,;/
#            <o)  =<      . / \         \`         .   o:::::::;;///
#             \__/ \       <')_=<                     >::::::::;;\\\
#              \            \_/            .            ''\\\\\'' ';\
#     (                      \              .   __
#      )                                       <'_><          (
#     (          (                ,/..          `              )
#      )     (    )             <')   `=<                )    (
#     (       )  (               ``\```                 (      )  source: chris.com
# _____)_____(____)______________________________________)____(_______
#
#########################################################################################

#  ANOVA example 2

#  Read in the data

snails <- read.csv("Snails1.csv", stringsAsFactors = T)

#  We have placed sea snails in a climate controlled environment.
#  The research question is to understand how changes in water temperature
#  impact the amount of time snails spend active in a 12 hour window.

#  From the str() and summary() and head() commands we can see that there are two columns 
#  of data.  We have a factor variable "Treatment" that has four levels
#  The levels are different water temperatures. "14 degree C" etc.
#  For each temperature there are 30 observations. 

str(snails)
summary(snails)
head(snails)

#  Look at the group means and group variances / sd

with(snails,tapply(Active.time,Treatment, mean))
with(snails,tapply(Active.time,Treatment, sd))

#  The first step in the analysis is to create a boxplot:

boxplot(Active.time~Treatment, data = snails)

#  Depending on your plot window size you may not see all the group labels
#  If you use the option to copy the figure to the clipboard you will see
#  that you can adjust the size so that the labels can all be seen

#  The time active is in mins per 12 hours. 

#  Based on the plot (i)  do you think the group variances are equal?
#                    (ii) do you think the group means are equal?

#  What are the null and alternate hypothesis for the test of 
#  equal group variances?

#  Conduct the bartlett test for equal group variances

with(snails, bartlett.test(Active.time~Treatment))

#  The p-value (1.693e-06) is less than 0.05, therefore we reject the null of 
#  equal variance across the groups.

#  Note that for small p-values we typically just report p-value < 0.001 rather 
#  than p-value = 1.693e-06 

#  We now conduct our ANOVA test.

#  What are the null and alternate hypothesis for the ANOVA test?

#  Implement the test using the one.way function, and unequal variance

with(snails,oneway.test(Active.time~Treatment, var.equal=FALSE ))

#  The p-value is < 0.001, therefore we reject the null hypothesis that the 
#  the group means are all the same.

#  For the pairwise t-tests We also set pooled.sd to FALSE to reflect that  
#  we have unequal group variances 

with(snails,pairwise.t.test(Active.time,Treatment, pool.sd=FALSE))

#  From the results table we conclude

#  14 degrees is different to 18 degrees (p-value <0.001)
#  14 degrees is different to 22 degrees (p-value <0.001)
#  14 degrees is not different to 26 degrees (p-value = 0.9)

#  18 degrees is different to 22 degrees (p-value <0.001)
#  18 degrees is different to 26 degrees (p-value <0.001)

#  22 degrees is different to 26 degrees (p-value <0.001)


#############################################################
#
#                         .----.   @   @
#                        / .-"-.`.  \v/
#                        | | '\ \ \_/ )... bloop!
#                      ,-\ `-.' /.'  /
#                     '---`----'----'
t################################################################


#   Single factor example with some editing features 

#  Step 1.  Read data and check the data structure

smoke.2.data <- read.csv("Smoke2.csv", stringsAsFactors = T)
str(smoke.2.data)
summary(smoke.2.data)
head(smoke.2.data)

#  The data describe plant germination success rates
#  when subject to different smoke concentration rates in the glass house
#  The smoke concentrations were 10%, 30%, 50%, and 70%

#  Step 2.  Create a  basic plot

boxplot(germination~smoke, data = smoke.2.data)

#  Reorder factor levels and create a quality plot

smoke.2.data$smoke <- factor(smoke.2.data$smoke, levels = c("ten", "thirty", "fifty", "seventy"))

#  Experiment with changing values for some of the settings below
#  to see if you can work out what what each setting controls

boxplot(germination~smoke, 
        names = c("10%" , "20%" , "50%" , "70%"), 
        xlab = "Smoke Application Rate",
        ylab = "Germination rate (%)",
        main = "",
        boxwex=0.6,
        outcol = "grey",
        outpch = 19,
        whisklty = 1,
        whiskcol = "grey",
        staplelty = 2, 
        staplecol = "grey",
        col = "grey", las =1,
        data = smoke.2.data)

#  This is unnecessary, but sometimes the group names are long and will not fit
#  in the space on the x-axis.  For such cases you then may want to use 
#  abbreviated names and add the key top the plot.
#  Note the way the cex command works. 

legend("topleft", legend = c("10% = 10% smoke","20% = 20% smoke"), bty="n" ,cex=0.8)
legend("top", legend = c("30% = 30% smoke","40% = 40% smoke","50% = 50% smoke" ), 
       bty="n", cex =0.9 )

#  In this example it makes sense to group by smoke level as
#  there is meaning to the factor levels.
#  In other applications there is less meaning to the factor levels, and
#  in such cases it can make sense to order the factor levels by group mean
#  or some other metric, such as median or SD.
#  You can find the values you need to oder the factor as follows.

with(smoke.2.data,tapply(germination,smoke, mean))
with(smoke.2.data,tapply(germination,smoke, median))
with(smoke.2.data,tapply(germination,smoke, sd))

#  Step 3. Test for constant group variance

#  Test set up
#  Null:  The group variances are equal
#  Alternate: The group variances are not all equal

#  Decision rule: If p-value,0.05 reject the null 

with(smoke.2.data, bartlett.test(germination,smoke))

###  Degrees of freedom adjustment anova ### 
############################################

#  Step 4a.  Anova allowing for non-constant group variance

#  Test set up
#  Null:  The group means are equal
#  Alternate: The group variances are not all equal

#  Decision rule: If test p-value,0.05 reject the null 

with(smoke.2.data,oneway.test(germination~smoke, var.equal = F))

#  Based on the p-value we reject the null, and proceed to pair-wise comparisons

#  Step 5a.  pair-wise comparisons, unequal variance

#  Test set up
#  Null:  The mean of group i is equal to the mean of group j 
#         (for group i not equal to group j)
#  Alternate: The mean of group i is not equal to the mean of group j 
#         (for group i not equal to group j)

with(smoke.2.data,pairwise.t.test(germination,smoke, pool.sd = F))

#  Note: The holm adjustment for pair-wise comparisons has been made
#  Note: Make sure you understand where the differences are between groups

#####################################################################

#  Example to be completed in class, if there is time
#  Conduct a structured analysis using the Mosquito1 data set

#  1. Create a boxplot that has the following features
#  (i) The factors are ordered from smallest sd to largest
#  (i) The factor level names are replaced with A, B, C... etc
#  (iii) A legend is added to the plot of the form
#        A = Fenitrothion, B = Malathion ... etc. 

#  2. Using you understanding of the save file options
#  save a copy of the plot in an appropriate scale and resolution.

#  3. Conduct a group variance test
#  save a copy of the plot in an appropriate scale and resolution.


#  4. Using the one.way function conduct an appropriate ANOVA test

#  5. Conduct appropriate pair-wise testing.

#  6.  Collate your results into a word document where you have:
#  (i)   specified an appropriate null and alternate hypothesis for each test
#         (group variance test, ANOVA test and pairwise tests)
#  (ii)  added your figure, including adding a caption label.  
#        Note: the figure should first be saved as a separate png file. 
#  (iii) Included an appropriate summary table of your pairwise results
#  (iv)  make a statement about the which insecticide(s) you would recommend 
#        Note: Lower values are preferred
#   (v)  Comment on any notable aspect of the results 


