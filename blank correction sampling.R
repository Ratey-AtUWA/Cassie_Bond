
#  the sample data structure
test.data <- c(0,1,1,1,2) 

#  Look at the data.frame you have created
test.data

# this is your data on any computer
setwd("C:/Masters/Thesis/James Stats")

my.data <- read.csv("new_data.csv")

#  36 rows?  Make it whatever it actually is
length(df.new$Total.MP.across.sizes)

#  The sample() function takes randomly selected values 
#  from a based data set you tell the function to sample from

# Draw 36 values from the data we have added 
sample(test.data, size =144,replace = TRUE )

set.seed(42) #  for reproducibiity 
out.values <- sample(test.data, size =144,replace = TRUE )

#  write to the working directory
write.csv(out.values, file ="blanks_corrected.csv", row.names = F)

###################################################################################
