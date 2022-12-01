library(tidyverse)
library(stringr)
library(purrr)

#setting up data
data <- read.csv('08/day-08-input.csv',header=FALSE)
data <- separate(data, col=V1, into = c("patterns", "outputs"), sep = "|")
patterns <- data[1:10]
outputs <- data[12:15]

#Part 1
#count 2,3 4, 7 length strings in output

count <- 0
for(cols in 1:4){
  for(rows in 1:nrow(outputs)){
    if(nchar(outputs[rows,cols]) %in% c(2,3,4,7) ){
      count <- count + 1
    }
  }
  }

print(count)

#Part 2
#we know which ones are 1(2) 4(4) 7(3) 8(7)

# 0(6) 
# 9(6)
# 6(6)

# 2(5)
# 3(5)
# 5(5)


#Can work out some rules based on intersections

# 6 -> has 6 characters and one missing must be in 7
# 9 -> has 6 characters but 4 must be in it
# 0 -> is remaining example with 6 characters

# 3 -> has 5 characters but must have both present in 1
# 5 -> has 5 characters all of which in 6
# 2 -> everything else

#Checks whether any of the strings in a vector of strings: string1 are wholly contained in a second string: string 2 (in any order)
# Returns a vector of logicals for whether the string in that position is wholly contained
test_string_contained <- function(string1,string2){
  test <- TRUE
  #checks each character in the string individually (surely a better way to do this)
  for(i in 1:nchar(string1)){
    new_test <- grepl(substr(string1,i,i),string2)
    test <- test & new_test
  }
  return(test)
}

#Set up array for answers

answer_index <- array(dim = c(nrow(patterns), ncol(patterns)))

for(row in 1:nrow(patterns)){

#Easy answers first
#column i of answer_index gives the column position in the pattern array of integer i. except 10 is 0
answer_index[row,1] <- which(nchar(t(patterns[row,]))==2)
answer_index[row,4] <- which(nchar(t(patterns[row,]))==4)
answer_index[row,7] <- which(nchar(t(patterns[row,]))==3)
answer_index[row,8] <- which(nchar(t(patterns[row,]))==7)

#This could all be streamlined. 

#test for 6 
patterns_with_6_char <- (t(patterns[row,])[which(nchar(t(patterns[row,])) == 6)])
pattern_for_no_7 <- t(patterns[row,])[answer_index[row,7]]
#check which pattern with 6 char is missing something from pattern for no_7
pattern_for_no_6 <- patterns_with_6_char[which(test_string(pattern_for_no_7,patterns_with_6_char)==FALSE)]
#update answer_index for 6
answer_index[row,6] <- which(t(patterns[row,])==pattern_for_no_6)

#test for 3
patterns_with_5_char <- (t(patterns[row,])[which(nchar(t(patterns[row,])) == 5)])
pattern_for_no_1 <- t(patterns[row,])[answer_index[row,1]]
#check which pattern with 5 char has both from 1
pattern_for_no_3 <- patterns_with_5_char[which(test_string(pattern_for_no_1,patterns_with_5_char))]
#update answer_index for 3
answer_index[row,3] <- which(t(patterns[row,])==pattern_for_no_3)

#test for 5 - everything in 5 will be in 6
for(i in 1:length(patterns_with_5_char)){
if(test_string_contained(patterns_with_5_char[i],pattern_for_no_6)==TRUE){ pattern_for_no_5 <- patterns_with_5_char[i]}
}
#update answer_index for 5
answer_index[row,5] <- which(t(patterns[row,])==pattern_for_no_5)

#test for 2 - everything else
pattern_for_no_2 <- patterns_with_5_char[patterns_with_5_char %in% c(pattern_for_no_5,pattern_for_no_3) == F]
#update answer_index for 2
answer_index[row,2] <- which(t(patterns[row,])==pattern_for_no_2)

#test for 9 
patterns_with_6_char <- patterns_with_6_char[patterns_with_6_char != pattern_for_no_6]
pattern_for_no_4 <- t(patterns[row,])[answer_index[row,4]]
pattern_for_no_9 <- patterns_with_6_char[which(test_string(pattern_for_no_4,patterns_with_6_char))]
pattern_for_no_0 <- patterns_with_6_char[patterns_with_6_char != pattern_for_no_9]

#update answer_index for 9 and 0
answer_index[row,9] <- which(t(patterns[row,])==pattern_for_no_9)
answer_index[row,10] <- which(t(patterns[row,])==pattern_for_no_0)

}

#Now need to find which patterns match the output - where test_string is true

#need to test whether the strings are same length AND contain all the same characters

#Same as function above but also checks whether they are same length
test_string_exact <- function(test1,test2){
  test <- TRUE
  for(i in 1:nchar(test1)){
    new_test <- grepl(substr(test1,i,i),test2)
    new_test2 <- nchar(test1) == nchar(test2)
    test <- test & new_test & new_test2
  }
  return(test)
}

#set up array to capture row numbers
sum <- array(dim = nrow(outputs))

#Loop through each position in outputs and find equivalent number
for(rows in 1:nrow(outputs)){
  comb_num <- ""
  for(cols in 1:ncol(outputs)){
    location <- which(test_string_exact(outputs[rows,cols],patterns[rows,]))
    #if statement because 0 is stored at position 10 :) 
    num <- ifelse(which(t(answer_index[rows,]) == location)==10, 0, which(t(answer_index[rows,]) == location))
    comb_num <- paste0(comb_num, num) 
  }
  sum[rows] <- comb_num
}

sum <- as.integer(sum)
#answer 2
print(sum(sum))
                 