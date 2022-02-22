

# Exercise 1. Create a histogram

#  Although it is straight forward to create a bar chart or pie chart in Excel
#  It is much harder to create a histogram or boxplot in Excel

#  Histograms and Boxplots are key plots in science
#  This is one reasons for introducing to R Studio

#  Typing data directly into R can be time consuming
#  For this exercise we will read in the data from an Excel file.

#  The specific format of the file we will use is an Excel .csv extension file
#  This approach will save us a lot of time

#  To do this you need to:

#  SET THE WORKING DIRECTORY

#  If you don't know how to do this consult the recording on the LMS
#  or ask a lab demonstrator

#  The below line only works if the file "wingspanMALES.csv" has been 
#  saved into your working directory

WingSpan <- read.csv("wingspanMALES.csv")

#  The file contains measurements on the wing span of different birds.
#  All the birds are Male.
#  In R Studio summary details of the data set will appear in the data window

#  We now have an 'object' called WingSpan that we can work with

#  To look at the structure of the data we have read into R 
#  you can use the 'str' command

str(WingSpan)

# If you look at the information in the console window you will see that  
# we have a 'dataframe'; 
# there are 87 observations; and 
# the values are numbers.  The console window should show something like:

#  'data.frame':	87 obs. of  1 variable:
# $ males: num  54.8 63.1 63.4 68.9 62.1 ...

#  We can look at the first few observations in the data using

head(WingSpan)  # head() shows the first six observations
                # and the column heading

#  If we type the dataframe name we will see all the observations

WingSpan

#  When we have a sample set of observations such as we have here
#  we want some way to look at the distribution of the data

#  At the moment we have a dataframe called WingSpan and a column of data within the
#  dataframe called 'males'

#  Our instructions need to tell R:
#  (i)   which data.frame to grab -- we want the 'WingSpan' data.frame;  
#  (ii)  the data column within the data.frame to look at -- we want the 'males' column; and
#  (iii) the 'function' to apply  -- we want the histogram function which we call with 'hist'

#  We get a basic histogram using the following commands:

with(WingSpan, hist(males))

#  We can then set the x-axis range with the xlim() parameter

with(WingSpan, hist(males,  xlim = c(0,100)))

#  We can start to make edits to the plot:

#  If you feel lost, please consult the R chapter for histograms
#  The guide provides lots of information

#  Look at each line of code to make sure you understand what is going on

with(WingSpan, hist(males,  xlim = c(0,100),  
                    col = "lightgreen", border = "magenta",
                    main ="Male wingspan histogram", 
                    xlab = "Wingspan (cm)", 
                    ylab ="Frequency of observation"))


## Reinforcement activity

#  copy the above example below and:
#  change the fill colour to grey 
#  change the border colour to black
#  change the x-axis back to the default setting

#  can you create a density plot for this data?

#  conduct a formal test for whether the data is from a normal distribution.  What do you conclude?

#  note you will need to use: Wingspan$males inside the test formula


#################################################################################
#                                                                               #   
#           Technical note on the default axis format for histograms.           #
#                                                                               #
#################################################################################

#  it is possiblt to override the default axis setting for histograms as shown below.

with(WingSpan, hist(males,  xaxs="i", yaxs="i"))

#  Sometimes this can be misleading.  For example, in this case the first bin is at 40,
#  not zero, and so it is possible to think the distribution is closer to zero than it
#  really is. 

#######################  New question part ###################################

##  Now read in the mine sample data

##  The values are measurements from the surface to an ore body
##  It is costly to remove the dirt to access the coal.
##  so the distance to the ore body is important

#  To read in the data file make sure that you have saved the file to your
#  working directory.  You need to save the file with the same
#  file name that i have used here

coal.mine <- read.csv("mine.sample.csv")

#  As part of any preliminary investigation we might do the following


head(coal.mine)                    #  Look at the way the data has been read in
str(coal.mine)                     #  The structure of the data set 

# We obtain a basic histogram as follows 

with(coal.mine, hist(measurement, breaks ='sturges')) # this is the default

#  Now edit the basic histogram so that:

#  (i)    breaks is equal to 'fd' to give smaller bin widths 
#  (ii)   the figure colour is lightgrey (ie the column fill colour is grey)
#  (iii)  the border for the columns is darkgrey
#  (iv)   the x-axis label reads "Depth to coal (m)"
#  (v)    the y-axis label reads "Frequency of observation"  
#  (vi)   the figure title is "Coal mine overburden example"

#  you just need to go back to the wingspan example to see what to do

#  Now create a density plot

with(coal.mine, plot(density(measurement)))

#  Finally conduct a formal test of the distribution shape and generate a qqplot and qqline
with(coal.mine, shapiro.test(measurement))
qqnorm(coal.mine$measurement); qqline(coal.mine$measurement, col = 2)

#  What do you conclude about whether or not it is reasonable to assume the data
#  conforms to a normal distribution?


