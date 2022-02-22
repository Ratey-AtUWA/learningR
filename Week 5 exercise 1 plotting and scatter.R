####    Working with scatter plots  ########

# Looking at continuous data not groups

## The exercise looks at some basic plotting features in R


#  We will start by creating a linear data series called 'x'

x <- seq(from = 1980, to =2022, by =2)

#  This line says create a series of numbers from 1980 to 2022 in steps of 2
#  and save the result in the 'object' called 'x'

#  The options available to look at the data object you have created
#  are as follows:

head(x)     # The first few observations
summary(x)  # A 5-number summary that can be used in a boxplot  
str(x)      # The structure of the data 
            # -- it says the objects are numbers and there are 18 of them       
x           #  All of the data

#  You can also see details of the data in the global environment window (top right window)

#  You can create an exponential data series called 'y' as follows

y <- exp(seq(from = 1, to =5.3, by =.2))

#  Options to look at the data you have created include:

head(y)
summary(y)
str(y)
y

#  We can look at a simple scatter plot of the data as follows:

#  Note 1

#  The '~' sign means y 'explained by' x; and x gets plotted on the horizontal 
#  and y will get plotted on the vertical

#  Note 2 we do not use with(my.data,...) of ... data = my.data as we created the
#  the objects we are working with directly in R.
#  In general there will be an extra required step.

plot(y~x)  

#  Note the difference if you use a ',' rather than a '~' symbol

plot(y,x)  #  reverse the axis

#  This format expects co-ordinate values so plots the first values 
#  on the horizontal axis and the second set of values on the vertical

#  Generally we can specify the details for the type of plot we want as follows:

plot(y~x, type ='l') #  For a line plot
plot(y~x, type ='p') #  For plot showing the points
plot(y~x, type="b")  #  For a plot showing both
plot(y~x, type="o")  #  variation draw the line through the dots 
plot(y~x, type="c")  #  variation to draw the line with gaps at the dots

#  You can also directly apply log transformations to the data you plot

#  The log transformation will 'undo' the exp on the Y data series
#  That will make the data a linear rather than curvy

plot(log(y)~x, type ='p')

#  Making curvy data approximately linear is a useful trick

#  We can start to experiment with the plot as:

#  Take you time to understand what the below code is doing.

plot(y~x, xlab = "the x axis label", ylab ="the y axis label: open dots", pch = 1) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: triangles", pch = 2) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: plus sign", pch = 3) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: cross", pch = 4) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: solid dot", pch = 19) 

#  To change from a scatter plot to a line plot, where we change the line type we use

plot(y~x, xlab = "the x axis label", ylab ="the y axis label: solid line", type ="l", lty =1) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: dashed line", type ="l", lty =2) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: dotted line", type ="l", lty =3) 
plot(y~x, xlab = "the x axis label", ylab ="the y axis label: dot-dash line", type ="l", lty =4) 

#  We can start to show some colour with: col= ""

plot(y~x, xlab = "the x axis label", ylab ="the y axis label", 
     pch = 4, 
     type ="b",
     col = "red",  # this is the new bit
     lty = 3) 

# We can make the line and points thicker using: lwd ="" 

plot(y~x, xlab = "the x axis label", ylab ="the y axis label", 
     pch = 4, 
     type ="b",
     col = "red",
     lty = 3,   #  this is the new bit
     lwd = 4)

#  We can also control, separately, the font size for any title, 
#  axis values, and axis labels using:
#  cex.main
#  cex.axis
#  cex.lab

#  These commands all work in the same way as for boxplots

plot(y~x, xlab = "the x axis label", ylab ="the y axis label", main ="my R plot",
     pch = 4, 
     type ="b",
     col = "red",
     lty = 3,
     lwd = 2,
     cex.lab=1.5,   #  to control the axis labels
     cex.axis=0.8,  #  to control the axis values
     cex.main =1.8) #  to control the title

#  If you really want to control the way the plot looks you can try some fancy stuff.

#  You can just look at how things work.  The things you really need to know about
#  are explained in more detail in subsequent script files.

plot(y~x, xlab = " X axis", ylab ="Y axis", main ="",
     ylim = c(0, 200),
     pch = 4, 
     type ="b",
     col = "grey",
     lty = 3,
     lwd = 1,
     axes=F) #  to suppress the axis information

box(which = "plot", lty = "solid") #  to add the box back

#  Add the details for the X-axis
par(tcl= -0.5)
axis(1, at=seq(from = 1980, to = 2022, by = 5), 
     labels=seq(from = 1980, to = 2022, by = 5),
     lwd=0, lwd.ticks=1)

#  Fill in some points for each year
par(tcl= -0.2)
axis(1, at=seq(from = 1980, to = 2022, by = 1), 
     labels=F,
     lwd=0, lwd.ticks=1)

#  Base information for the Y-axis
par(tcl= -0.5)
axis(2, at=seq(from = 0, to = 250, by = 50), 
     labels=seq(from = 0, to = 250, by = 50),
     lwd=0, lwd.ticks=1,las=1)

# Add some point to give the figure balance
par(tcl= -0.2)
axis(2, at=seq(from = 0, to = 250, by = 10), 
     labels=F,
     lwd=0, lwd.ticks=1)

#  Add a legend
legend("topleft", legend ="exponential growth",  lty =3, pch=4, bty ="n", col = "grey", cex=.8)

#####################################################################
#
#      / \`\          __
#      |  \ `\      /`/ \
#      \_/`\  \-"-/` /\  \
#           |       |  \  |
#           (d     b)   \_/
#           /       \
#       ,".|.'.\_/.'.|.",   
#      /   /\' _|_ '/\   \
#      |  /  '-`"`-'  \  |
#      | |             | |
#      | \    \   /    / |
# jgs   \ \    \ /    / /
#        `"`\   :   /'"`   ....28 days... 6 hours... 42 minutes... 12 seconds
#            `""`""`
##############################################################################

