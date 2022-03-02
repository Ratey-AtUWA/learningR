#       ______ _   ___      _________ ____ ____    __ __ 
#      |  ____| \ | \ \    / /__   __|___ \___ \  / //_ |
#      | |__  |  \| |\ \  / /   | |    __) |__) |/ /_ | |
#      |  __| | . ` | \ \/ /    | |   |__ <|__ <| '_ \| |
#      | |____| |\  |  \  /     | |   ___) |__) | (_) | |
#      |______|_| \_|   \/      |_|  |____/____/ \___/|_|
#     __                       __    ____    _                    _     _                     ____  
#    / /  _ __    ___          \ \  / ___|  | |_    __ _   _ __  | |_  (_)  _ __     __ _    |  _ \ 
#   | |  | '__|  / _ \  _____   | | \___ \  | __|  / _` | | '__| | __| | | | '_ \   / _` |   | |_) |
#   | |  | |    |  __/ |_____|  | |  ___) | | |_  | (_| | | |    | |_  | | | | | | | (_| |   |  _ < 
#   | |  |_|     \___|          | | |____/   \__|  \__,_| |_|     \__| |_| |_| |_|  \__, |   |_| \_\
#    \_\                       /_/                                                  |___/           

#### Reading the data ####
hubbard <- read.csv(file = "hubbard.csv", stringsAsFactors = TRUE)
# ... and do a quick check
is.data.frame(hubbard) # check that it worked

# First proper check - summarise some of the data
summary(hubbard[,1:10]) # just the first 10 columns

#### First proper check - summarise some of the data ####
summary(hubbard[,1:10]) # just the first 10 columns

# The summary() function creates a little table for each column - note
# that not all these little tables looks the same. Integer or numeric columns
# get a numeric summary with minimum, mean etc., and sometime the number
# of missing (NA) values. Categorical (Factor) columns show the
# number of samples (rows) in each category (unless there are too many
# categories). These summaries are useful to check if there are zero or negative
# values in columns, how many missing observations there might be, and if the
# data have been read correctly into R.
#
# [Note: we could have specified something like hubbard[1:10,]
# which would have worked on the first 10 rows (also called
# 'observations' or 'samples), or hubbard[1:20,6:10] which would have
# used only the first 20 rows of columns 6 to 10.]

##### Final checks of the data frame ####

# Usually we would not restrict the output as done below with [,1:20].
# We only do it here so we're not bored with pages of similar-looking output.
# You should look at structure for the whole data frame using
# str(hubbard) (or whatever your data object is called).

str(hubbard[,1:20]) # 'str' gives the structure of an object

# We can see that some columns are integer values (e.g. PLOT, UTM_EASTING),
# some columns contain Factor values i.e. in fixed categories
# (e.g. Rel.To.Brook, Transect), and some columns are numeric
# (e.g. PH, OM.pct, Ni). Applying the str() function to a data
# object is **always** a good idea, to check that the data have read correctly
# into R. [NOTE that other variable types are possible such as character
# 'chr', date ('Date' or 'POSIXct'), logical, etc.]


#### Base R plotting: x-y plot using plot() ####

# We can use either plot(x, y, ...) OR plot(y ~ x, ...)
#   In R the ~ symbol means 'as a function of', so ~ indicates a formula.
#

# these 3 chunks of code do exactly the same thing:

# 1        (we recommend this one!)
with(hubbard,
     plot(EXCH_Al ~ PH)
     )

# 2
plot(hubbard$EXCH_Al ~ hubbard$PH)

# 3
attach(hubbard)
  plot(EXCH_Al ~ PH)
detach(hubbard)


# Without changing any of the (numerous) options or parameters in 
# the plot() function, the plot is not very attractive (e.g. axis titles!).
#   We can also change the overall plot appearance with par(), which sets 
# graphics parameters. Let's try some variations:
# set overall graphics parameters using par()
#        mar sets margins in 'lines' c(bottom,left,top,right)
#        mgp sets distance of text from axes c(titles, tickLabels, ticks)
#        font.lab sets font style for axis titles: 2=bold, 3=italic etc.
par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
# We'll also include some better axis title text using xlab, ylab
with(hubbard,
     plot(EXCH_Al ~ PH, 
     xlab="Soil pH", 
     ylab="Exchangeable Al (proportion of CEC)")
     )
# 
# This is starting to look a lot better!
#
# We can still add more information to the graph; for example, 
# by making use of the factors (categories) in the dataset. 
# We also need to learn these graphics parameters: 
#     col = plotting colour(s) - it's easiest to use words 
#     like "red", "darkblue" and so on 
#     - see examples at http://research.stowers.org/mcm/efg/R/Color/Chart/ 
#     or just run the R function colors() for a list of all 657 names!
#     pch = plot character(s) - numbers from 0 to 24
#     (see http://www.endmemo.com/program/R/pchsymbols.php )
# 
par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
with(hubbard,
     plot(EXCH_Al ~ PH, xlab="Soil pH",
          ylab="Exchangeable Al (proportion of CEC)",
          pch=c(1,16)[Rel.To.Brook], 
          col=c("blue","darkred")[Rel.To.Brook])
     )
# 
# The parameter pch=c(1,16)[Rel.To.Brook] separates the points by the  
# information in the factor column Rel.To.Brook, shown inside [ ].  
# This column is a 2-level factor, so can be one of two categories  
# (North or South), and so we need a vector with two numbers in it  
# (pch=c(1,16)). The code for specifying colour is very similar, except 
# our vector has 2 colour names in it.
# There is still one thing missing; a graph legend. We can do this using 
#   the legend() function. We will use the following options:
# "topleft" position of legend - see help(legend) for options, or we can 
#    use x-y coordinates
# legend = names identifying the plot symbols - we have used the 
# levels in the factor Rel.To.Brook, but we could have used 
#   legend=c("North","South") instead
# pch = plot symbols - should be exactly the same vector as in 
#   the plot function
# col = plot colours - should be exactly the same vector as in 
#   the plot function
# title = a title for the legend - optional
# 
par(mar=c(4,4,1,1), mgp=c(2,0.6,0), font.lab=2)
with(hubbard,
       plot(EXCH_Al ~ PH, xlab="Soil pH", 
            ylab="Exchangeable Al (proportion of CEC)",
            pch=c(1,16)[Rel.To.Brook], 
            col=c("blue","darkred")[Rel.To.Brook])
       )
legend("topleft", legend=levels(hubbard$Rel.To.Brook), pch=c(1,16), 
       col=c("blue","darkred"), title="Location")

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# 
#### Alternative to base-R plot: scatterplot (from 'car' package) ####

# The R package 'car' (Companion to Applied Regression) has many useful 
# additional functions that extend the capability of R. 
# The next two examples produce the same plot as in the previous examples, 
# using the scatterplot() function in the car package.
# 
# load required package(s)
library(car)
# par() used to set overall plot appearance using options within 
# par(), e.g.
#     mar sets plot margins, mgp sets distance of axis title and 
#     tick labels from axis
par(font.lab=2, mar=c(4,4,1,1), mgp=c(2.2,0.7,0.0))
# draw scatterplot with customised options 
# remember pch sets plot character (symbol); 
# we will also use the parameter cex which sets symbol/font sizes
scatterplot(EXCH_Al ~ PH, data=hubbard, smooth=FALSE, 
            legend = c(coords="topleft"), 
            cex=1.5, cex.lab=1.5, cex.axis=1.2)
# 
# Note that we get some additional graph features by default:
# 1. boxplots for each variable in the plot margins &ndash; these 
# are useful for evaluating the distribution of our variables and 
# any extreme values 
# 2. a linear regression line showing the trend of the relationship 
# (it's possible to add this in base R plots, too) 
# 
#  We can turn both of these features off if we want - search for 
#   'scatterplot' in the R Studio help sub-window, and look under 
#   Arguments and Details. 
#   Also, we separately specify the dataset to be used, i.e., data=hubbard.

#### scatterplot with groups, Hubbard Brook soil data ####
# {r, fig.height=4, fig.width=4}
# 'require()' loads package(s) if they haven't already been loaded
require(car)
# adjust overall plot appearance using options within par()
# mar sets plot margins, mgp sets distance of axis title and tick 
#   labels from axis
par(font.lab=2, mar=c(4,4,1,1), mgp=c(2.2,0.7,0.0))
# create custom palette with nice colours :)
# this lets us specify colours using numbers - try it!
palette(c("black","red3","blue3","darkgreen","sienna"))
# draw scatterplot with points grouped by a factor (Rel.To.Brook) 
scatterplot(EXCH_Al ~ PH | Rel.To.Brook, data=hubbard, smooth=FALSE,
            legend = c(coords="topleft"), col=c(5,3,1), 
            pch=c(16,0,2), cex=1.2, cex.lab=1.3, cex.axis=1.0)
# 
# scatterplot() creates a legend automatically
# if we plot by factor groupings (note the different way that the 
# legend position is specified within the scatterplot() function).
# This is pretty similar to the base R plot above (we can also customise 
# the axis titles in scatterplot(), using xlab= and ylab= as before).
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.


#### Other types of data presentation: plot types and tables ####

# We'll give you some starting code chunks, and the output from them. 
#   You can then use the help in R Studio to try to customise the plots 
#   according to the suggestions below each plot!
#  
#### Box plots #### 
boxplot(MOISTURE.pct ~ Rel.To.Brook, data=hubbard)
# 
## For box plots, try the following:
#     include informative axis labels (titles)
#     plotting boxes separated by a different factor
#     make boxes a different colour (all the same, and all different!)
#     add notches to boxes representing approximate 95% confidence 
#       intervals around the median
#     give the (vertical) y-axis a log<sub>10</sub> scale
#     ...and so on.
# 
#### Histograms ####
with(hubbard, hist(MOISTURE.pct))
## For histograms, try the following:
#     add suitable axis labels (titles)
#     make bars a different colour (all the same)
#     change the number of cells (bars) on the histogram to give 
#       wider or narower intervals
#     log<sub>10</sub>-transform the x-axis (horizontal axis)
#     remove the title above the graph (this information would 
#       usually go in a caption)
#     ...and so on.
# 
#### Strip Charts and Plots of Means (two plots together) ####
# We use the mfrow= or mfcol= argument in the par() function to 
#   plot multiple graphs
require(RcmdrMisc)# needed for plotMeans() function
# use the mfrow or mfcol argument in the par() function to plot 
# multiple graphs
par(mfrow=c(1,2))
stripchart(hubbard$OM.pct, main="Strip Chart")
with(hubbard,plotMeans(OM.pct, Transect, error.bars = "conf.int"))
par(mfrow=c(1,1)) # to get back to single plots
# 
## For one or both plots, try the following:
#     add suitable axis labels (titles), in bold font;
#     plotting means separated by a different factor;
#     for plot of means, rotate the tick labels so that none ore omitted;
#     make strip chart symbols a different shape  colour (all the same, 
#     and all different!);
#     make the strip chart vertical instead of horizontal;
#     apply some 'jitter' (noise) to the strip chart symbols so that 
#     overlapping points are easier to see;
#     log10-transform the  numerical axis of the strip chart so that 
#     overlapping points are easier to see;
#     remove the titles above the graphs (this information would usually 
#     go in caption/s)
# ...and so on.
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# 
#### Tables ####
# There are a few ways to make useful tables in R to summarise your data. 
# Here are a couple of examples.
#
#### using the tapply() function in base R ####
# use the cat() [conCATenate] function to make a Table heading 
#     (\n is a line break)
cat("One-way table of means\n")
tapply(X = hubbard$EXCH_Ni, INDEX=hubbard$Rel.To.Brook, 
       FUN=mean, na.rm=TRUE)
cat("\nTwo-way table of means\n")
tapply(X = hubbard$EXCH_Ni, 
       INDEX=list(hubbard$Transect,hubbard$Rel.To.Brook), 
       FUN=mean, na.rm=TRUE)
# 
## For tapply() tables, try the following:
# we have used the 'mean' function (FUN=mean) &ndash; try another 
#     function to get minima, maxima, standard deviation, etc.
# try copying the output to Word or Excel and using this to make 
#     a table in that software
# ...and so on.
# 

#### using the numSummary() function in the 'RcmdrMisc' R package ####
require(RcmdrMisc)
# use the cat() [conCATenate] function to make a Table heading 
#   (\n is a line break)
cat("Summary statistics for EXCH_Ni\n")
numSummary(hubbard$EXCH_Ni)
cat("\nSummary statistics for EXCH_Ni grouped by Rel.To.Brook\n")
numSummary(hubbard$EXCH_Ni, groups=hubbard$Rel.To.Brook)
# 
## For numSummary() tables, try the following:
# generating summary tables for more than one variable at a time
# generating summary tables with fewer statistical parameters 
#   (e.g. omit IQR)  
#   or more statistical parameters (e.g. include skewness) 
#   (use R Studio Help!)
# try copying the output to Word or Excel and using this to 
#   make a table in that software
# ...and so on.
#
#
# [end code]
