#####  Exercise 1  ##############

##  Working with a linear-log model 

#  Step 1: read in the data file 

b.mass <- read.csv("Biomass_data1.csv") 

#  Step 2: look at what you have read in.  There are still three basic options
#  for looking at the data structure

head(b.mass)
summary(b.mass)
str(b.mass)

#  Step 3.
#  With grouped data the next step was to create a boxplot
#  Here the next step is to create a scatter plot 

#  What we do is first plot the data on the original scale
#  If, on the original data scale, the data does not look linear
#  we then look at some log transformation options

par(mfrow = c(2, 2))

plot(Biomass~Time, pch= 1,data = b.mass) # does not look linear

plot(log(Biomass)~Time, pch=2,data = b.mass) # does not look linear

plot(log(Biomass)~log(Time), pch=3,data = b.mass) # plausible, but not great

plot(Biomass~log(Time), pch=4,data = b.mass) # The best option, i think...

par(mfrow = c(1, 1))

#  So now we make the plot look nice.

plot(Biomass~log(Time), 
     las =1,
     pch = 19,
     col = "grey",
     ylab = "Biomass (T)",
     xlab = "Time (log scale)",
     data = b.mass)

#  We estimate the linear-log model as follows:

lm.1 <- lm(Biomass~log(Time), data = b.mass)  #  the intercept term is automatic

#  The above script line says fit a trend line to the data in the object 'b.mass'
#  where the y-values (Biomass) are on the original scale and the x-axis values
#  (Time) are on a log scale.
#  As we are fitting a line R knows to add the intercept term automatically

#  we look at the results with the summary() command

summary(lm.1)

# The coefficents, st. error t-value and p-value information sets up the following tests

#  1. Intecept 
#  H0: The intercept is zero.
#  H1: The intecept is not zero.

#  -- Do you reject the null hypothesis?

#  We use the same decision rule we have been using all along
#  With a p-value > 0.05, you do not reject the null 

#  Generally we are not interested in the intercept
#  and regardless of the test we will always have an 
#  intercept in the models we estimate.

#  2. Slope
#  H0: The slope is zero.
#  H1: The slope is not zero.

#  -- Do you reject the null hypothesis?

#  Think about the p-value rule. Ask a demonstrator if you don't
#  understand things and are feeling lost.  Note the p-value is very very small. 

#  As we have a linear-log model, the convention is to use the following approach and say:
#  A one percent change in Time leads to an expected increase 
#  in biomass of (0.832/100) Tonnes.  So, 0.00832 Tonnes or 8.3 kg.

#  The final bit of summary information we want from our model is the
#  multiple R2 value.

#  Here the value is Multiple R-squared:  0.7246, and we can
#  say that the model explains 72.4 percent of the 
#  observed variation in the data.

######################################################################

#  The final step is to create an appropriate data plot that 
#  we can use in a report and then transfer the summary information 
#  to a summary table (see R web notes )


plot(Biomass~log(Time), 
     las =1,
     pch = 19,
     col = "grey",
     ylab = "Biomass (T)",
     xlab = "Time (log scale)",
     data = b.mass)

abline(lm.1, lty=2, lwd=1, col = "red")

legend("topleft", legend=c("observations", "trend line"), 
       lty=c(NA,2), lwd=c(NA,1), col =c("grey", "red"), pch = c(19,NA), bty ="n") 

summary(lm.1)

#######################################################################
#                             .-.
#                       __   /   \   __
#                      (  `'.\   /.'`  )
#                       '-._.(;;;)._.-'
#                       .-'  ,`"`,  '-.
#                      (__.-'/   \'-.__)/)_
#                            \   /\    / / )
#                             '-'  |   \/.-')
#                             ,    | .'/\'..)
#                             |\   |/  | \_)
#                             \ |  |   \_/
#                              | \ /
#                               \|/    _,    ...flower are biomass too..
#                          jgs   /  __/ /
#                               | _/ _.'
#                               |/__/
#                                \
######################################################################

#####  Exercise 2.  A log-linear model complete details

#  Read the data into R

Y.data <- read.csv("Yield_change_data1.csv")

#  Look at the data structure

head(Y.data)
str(Y.data)

#  Now let's look at some basic plot options
par(mfrow=c(2,2))
plot(Yield~Year, data = Y.data)  #  not linear
plot(log(Yield)~Year, data = Y.data)  #  looks approx. linear
plot(Yield~log(Year), data = Y.data)  #  not liner
plot(log(Yield)~log(Year), data = Y.data)  #  also approx. linear
par(mfrow=c(1,1))


#  Often, there is  more than one option for a model
#  but here we pick the  log(Y)-X model

plot(log(Yield)~Year, 
     ylab = "Yield (log scale)",
     xlab = "", # to suppress the xlab as with time it is clear what is going on
     data = Y.data)  

#  Now we estimate the linear model using the lm() function
#  We use the same format as we have for our plot
#  so the form is log(Y)~X

lm2 <- lm(log10(Yield)~Year, data = Y.data)

summary(lm2)

#  Formally the summary conducts the following tests:

#  1. Intercept 
#  H0: The intercept is zero.
#  H1: The intercept is not zero.

#  The 'point' estimate for the intercept is -57.2 and 
#  as the p-value associated with the intercept is very small
#  (p-value <0.001) therefore we reject the null.


#  2. Slope (which is what we are interested in)
#  H0: The slope is zero.
#  H1: The slope is not zero.

#  The point estimate on Year is 0.03
#  as the p-value associated with the slope is very small
#  (p-value <0.001) we reject the null.

#  As we have a log-linear model we can say the following:

# A one unit increase in years results in an
# expected increase in yield of 0.0304 x 100 =
# 3.04 percent increase in yield.

#  Note that as the Multiple R-squared value =  0.9155,
#  we can say, the model explains 91.6% of the variation in the data

#  We make our plot publication quality by:

#  (i)  adding a trend line, and
#  (ii) adding a legend


plot(Yield~Year, 
     log = "y",
     ylab = "Yield",
     xlab =""  ,  
     las=1,
     data = Y.data)  

abline(lm2, lty =1, lwd=1, col ="grey")

legend("topleft", legend=c("Observations","Trend line"), lty=c(NA,1), pch =c(1,NA), 
       lwd=c(NA,1), col =c("black","grey"), bty="n")

#  We also create a summary table for our output from select values taken 
#  from the summary 
#  If you can not rcall the number of observations you can use the str() function
summary(lm2)
str(Y.data)

############################################################################
#  //\
#  V  \
#   \  \_                               ... go on, have a banana
#    \,'.`-.
#     |\ `. `.       
#     ( \  `. `-.                        _,.-:\
#      \ \   `.  `-._             __..--' ,-';/
#       \ `.   `-.   `-..___..---'   _.--' ,'/
#        `. `.    `-._        __..--'    ,' /
#          `. `-_     ``--..''       _.-' ,'
#            `-_ `-.___        __,--'   ,'
#               `-.__  `----"""    __.-'
#                    `--..____..--'
#
########################################################################

#  Exercise 3

#  This example covers working with a log-log model 

#  First read in the data

diversity <- read.csv("distance_and_diversity1.csv")

#  Now have a look at the data structure

head(diversity)
str(diversity)
summary(diversity)

#  This time we have additional information in the data file

#  We are interested in modelling the relationship between
#  the diversity index measure and the altitude.
#  Specifically, we are interested in understanding if 
#  diversity varies with altitude.

#  Step 1. look at some plots quickly

plot(diversity~altitude, data = diversity)  # not linear
plot(log(diversity)~altitude, data = diversity)  # not great
plot(diversity~log(altitude), data = diversity)  # not linear
plot(log(diversity)~log(altitude), data = diversity)  # closest to a line, so we go with this option

#  Not everything will look perfect at this stage we are really just getting the
#  hang of the basics It is possible to subsequently develop more complex
#  models.

#  Now let's build up a reasonably complex plot of the type we might want for a
#  research report or for  an assignment question.

#  Note that as we will have a figure caption
#  we will add the title information in the figure caption
#  here I just add a title as part of demonstrating the R functions

plot(log(diversity)~log(altitude), data = diversity, type = "p", pch=19, col ="grey",
     ylab = "Diversity Index (log scale)", 
     xlab ="Altitude above sea level (log scale)", 
     main ="Relationship between diversity and altitude",
     xlim = c(4,5),
     ylim = c(-4.1,0),
     font.main=4,
     las=1)

#  We now estimate a linear model to derive a trend line

lm3 <-lm(log(diversity)~log(altitude), data = diversity)

#  Next we look at the estimation result to see 
#  what the slope coefficient and intercept values are

summary(lm3)

#  The literal interpretation:
#  A one unit change in log altitude is expected to result in a
#  fall in the log of the diversity index of 2.5.

#  The way we report the result in practice, for log-log models
#  A one percent increase in altitude is associated with an expected fall in
#  the diversity index of 2.53 percent.

#  Then we add the trend line, to our plot.
#  As we have grey dots we use the default implementation in R
#  which will add a straight black line

abline(lm3)

# Finally, we add the legend to the plot.  You can guess where to add the plot

legend("bottomleft", legend=c("trend","observation"), lty=c(1,NA), pch =c(NA,19), 
       lwd=c(1,NA), col =c("black","grey"), bty="n")

##  But now say we decide that the legend is in the wrong place.
##  Say we want the legend to be in the top right.

#   Redraw the whole plot but change: 

#  (i)   the dots to be blue triangles (look back to exercise 1)
#  (ii)  the trend line such that it is a thick red dotted line
#  (iii) the legend location to be in the topright position


####################################################################### 
#   
#                                      /\       ..diversity and altitude
#                                 /\  //\\
#                          /\    //\\///\\\        /\
#                         //\\  ///\////\\\\  /\  //\\
#            /\          /  ^ \/^ ^/^  ^  ^ \/^ \/  ^ \
#           / ^\    /\  / ^   /  ^/ ^ ^ ^   ^\ ^/  ^^  \
#          /^   \  / ^\/ ^ ^   ^ / ^  ^    ^  \/ ^   ^  \       *
#         /  ^ ^ \/^  ^\ ^ ^ ^   ^  ^   ^   ____  ^   ^  \     /|\
#        / ^ ^  ^ \ ^  _\___________________|  |_____^ ^  \   /||o\
#       / ^^  ^ ^ ^\  /______________________________\ ^ ^ \ /|o|||\
#      /  ^  ^^ ^ ^  /________________________________\  ^  /|||||o|\
#     /^ ^  ^ ^^  ^    ||___|___||||||||||||___|__|||      /||o||||||\       |
#    / ^   ^   ^    ^  ||___|___||||||||||||___|__|||          | |           |
#   / ^ ^ ^  ^  ^  ^   ||||||||||||||||||||||||||||||oooooooooo| |ooooooo  |
#   ooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 
#
###########################################################################


#  Practice example

#  Read in the data

Wing <- read.csv("Wing_length.csv")

#  Check the data
head(Wing)
#  The column headings are going to be a pain to work with
#  As such we rename things to something simple

names(Wing) <- c("age", "length") 
str(Wing)

#  We know that" 
# age = the age of birds in days
# length = wingspan measurement in cm

#  Using the above examples try and fit a trend line to the data
#  and obtain estimates for the slope and intercept.


####################################################################################

par(mfrow=c(2,2))
plot(length ~ age, data = Wing)
plot(log10(length) ~ age, data = Wing)
plot(length ~ log10(age), data = Wing)
plot(log10(length) ~ log10(age), data = Wing)
par(mfrow=c(1,1))

lm4 <- lm(length ~ age, data = Wing)
summary(lm4)
plot(length ~ age, data = Wing)
abline(lm4, col = "magenta")

par(mfrow=c(2,2))
plot(lm4)
par(mfrow=c(1,1))

lm4$residuals
lag(lm4$residuals,1)
