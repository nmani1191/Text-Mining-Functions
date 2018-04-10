source("Functions.R")

lookup_data<-read.csv("Pattern_Dictionary.csv",header=T,na.strings=c(""," "))

lookup_data<-lookup_data[order(lookup_data$Priority),]
lookup_data$Patterns <- textp(lookup_data$Patterns)

test_data <- read.csv("test_data.csv",header=T,na.strings=c(""," ","na","NA","NULL","null"))

input <- test_data
lookup <-  lookup_data
text_column <- "Description"


system.time({random_mapped_data <- random_pattern_match(input,lookup,text_column)})

# [1] "2018-04-10 07:21:56 IST"
# [1] "2018-04-10 07:22:09 IST"
# user  system elapsed 
# 13.60    0.03   13.79 

system.time({forward_mapped_data <- forward_pattern_match(input,lookup,text_column)})

# [1] "2018-04-10 07:22:30 IST"
# [1] "2018-04-10 07:22:48 IST"
# user  system elapsed 
# 18.45    0.00   18.57
