library(tidyverse)
data <- read.csv('03/day-3-input.csv',header=FALSE)
colnames(data) <- c("full")
data$full <- substr(data$full, 1,12)

#Part 1
gamma <- ""
epsilon <- ""

for(i in 1:12){
  #create new columns for each position
  data[,i+1] <- substr(data[,'full'],i,i)
  #name new columns
  names(data)[names(data) == paste0("V",i+1)] <- paste0("pos_",i)
  #check which is most common in each position and add 1 or 0 to gamme and epsilon strings as appropriate
  if(sum(as.numeric(data[,i+1]))/nrow(data) >= 0.5){
         gamma <- paste(gamma,"1",sep = "")
         epsilon <- paste(epsilon,"0",sep = "")}
  else {gamma <- paste(gamma,"0",sep = "")
        epsilon <- paste(epsilon,"1",sep = "")}
   }

answer <- strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
print(answer)

#Part 2

#oxygen gen rating
data2 <- data
#Part 2
for(i in 1:12){
  #Set column (ie position) we are looking at
  col <- paste0('pos_',i)
  #filter to only rows with most common value
  if(sum(as.numeric(data2[,col]))/nrow(data2) >= 0.5){
    #!! injection operator used to pass col string as object which can be used in filter
   data2 <- filter(data2,!!as.symbol(col)==1)}
  else{data2 <- filter(data2,!!as.symbol(col)==0)}
  #stop process when we only have 
  if (nrow(data2)==1) {break}
  }

oxygen_gen_rating <- strtoi(data2[1,1], base = 2)

#co2 rating - same loop as above but take least common value in each column
data3 <- data
#Part 2
for(i in 1:12){
  col <- paste0('pos_',i)
  if(sum(as.numeric(data3[,col]))/nrow(data3) >= 0.5){
    data3 <- filter(data3,!!as.symbol(col)==0)}
  else{data3 <- filter(data3,!!as.symbol(col)==1)}
  if (nrow(data3)==1) {break}
}

co2_rating <- strtoi(data3[1,1], base = 2)

answer2 <- oxygen_gen_rating * co2_rating
print(answer2)
