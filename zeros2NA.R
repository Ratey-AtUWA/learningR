dataframe <- read.csv("data_file.csv", stringsAsFactors = T)
#### check for zeros and replace with NA ####
# first look at the variable
print(dataframe$variable)
# find row numbers of zeros
which(dataframe$variable == 0)
# find how many zero values
length(which(dataframe$variable == 0))
# use this info to replace zero values with NA
dataframe[which(dataframe$variable==0),"variable"] <-
  rep(NA,length(which(dataframe$variable == 0)))
# check it
print(dataframe$variable)
