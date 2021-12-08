library(tidyverse)

#setting up data
data <- read.csv('07/day-07-input.csv',header=FALSE)
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),',')))
data <- t(data)
colnames(data) <- "x"
data<- as.integer(data)
data_master <- data

#Part 1

fuel_cost <- function(data,p){
  for(i in 1:length(data)){
    fuel <- fuel + abs(data[i] - p)
  }
  return(fuel)
}

#Median is best position 
best_pos <- median(data)

#calculate fuel cost at that position
ans1 <- fuel_cost(data,best_pos)
  
#Answer 1
print(ans1)

#Part 2
#Update fuel cost function for part 2

fuel_cost_2 <- function(data,p){
  fuel <- 0
  for(i in 1:length(data)){
    diff <- abs(data[i] - p)
  #sum from 1 to n is given by n(n+1)/2
  cost <- (diff * (diff + 1)) / 2
  fuel  <- fuel + cost
  }
  return(fuel)
}

fuel <- 0
lowest_fuel <- 100000000000 

for (p in 1:length(data)){
  
  fuel <- fuel_cost_2(data,p)
  
  if (fuel <= lowest_fuel){lowest_fuel <- fuel; best_pos <- p}
  print(p)
  print(fuel)
  
  fuel <- 0
  
}

#Answer 2
print(lowest_fuel)
