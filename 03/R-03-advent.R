library(tidyverse)
data <- read.csv('03/day-3-input.csv',header=FALSE)
colnames(data) <- c("full")
data$full <- substr(data$full, 1,12)

#Part 1

#Function - test_column
#returns true if column has mainly 1s, false if majority zeros
# data = input data
# i = column number of dataframe to check minus 1 (because the first column is the full binary number)

test_column <- function(data,i){
  #if average value of a column of 1s/0s is > 0.5 then it's mostly 1s
sum(as.numeric(data[,i+1]))/nrow(data) >= 0.5
}

#Function expand_binary_data
#Takes a dataframe of equal length binary strings as input
#Extracts each element of a string as a new column

expand_binary_data <- function(data){
  gamma <- ""
  epsilon <- ""
binary_length <- max((nchar(data$full)))
for(i in 1:binary_length){
  #create new columns for each position
  data[,i+1] <- substr(data[,'full'],i,i)
  #name new columns
  names(data)[names(data) == paste0("V",i+1)] <- paste0("pos_",i)
  
  #check which is most common in each position and add 1 or 0 to gamma and epsilon strings as appropriate
  if(test_column(data,i)){
         gamma <- paste(gamma,"1",sep = "")
         epsilon <- paste(epsilon,"0",sep = "")}
  else {gamma <- paste(gamma,"0",sep = "")
        epsilon <- paste(epsilon,"1",sep = "")}
}
answer <- strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
#Return two things
results <- list("answer" = answer, "data" = data)
}

results <- expand_binary_data(data)
print(results$answer)
#Need expanded data for the next part
data <- results$data

#Part 2

#oxygen gen rating
oxy_func <- function(data){ 
  #create a new dataset we will iteratively reduce
  ox_data <- data
  #Part 2
  for(i in 1:(length(data)-1)){
    #Set column (ie position) we are looking at
    col <- paste0('pos_',i)
    #filter to only rows with most common value
    if(test_column(ox_data,i)){
     ox_data <- filter(ox_data, get(col)==1)}
    else{ox_data <- filter(ox_data, get(col)==0)}
    #stop process when we only have one row left
    if (nrow(ox_data)==1) {break}
  }
  oxygen_gen_rating <- strtoi(ox_data[1,1], base = 2)
  return(oxygen_gen_rating)
}

co2_func <- function(data){ 
    #co2 rating - same loop as above but take least common value in each column
    co2_data <- data
    #Part 2
    for(i in 1:(length(data)-1)){
      col <- paste0('pos_',i)
      if(test_column(co2_data,i)){
        co2_data <- filter(co2_data,get(col)==0)}
      else{co2_data <- filter(co2_data,get(col)==1)}
      if (nrow(co2_data)==1) {break}
    }
    co2_rating <- strtoi(co2_data[1,1], base = 2)
    return(co2_rating)
}

oxygen_gen_rating <- oxy_func(data)
co2_rating <- co2_func(data)

answer2 <- oxygen_gen_rating * co2_rating
print(answer2)
