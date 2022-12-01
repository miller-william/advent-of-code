library(tidyverse)
data <- read.csv('02/day-2-input.csv',header=FALSE)

#Part 1

#Splitting string into two features
data <- transpose(strsplit(data$V1," "))
data <- as.data.frame(do.call(cbind, data))
data$V2 <- as.numeric(data$V2)
colnames(data) <- c("direction", "X")

#Counting different directions
forward <- sum(select(filter(data, direction == 'forward'),'X'))
down <- sum(select(filter(data, direction == 'down'),'X'))
up <- sum(select(filter(data, direction == 'up'),'X'))

#by question definition of depth
depth <- down - up

#Answer is depth times forward
answer <- depth * forward
print(answer)

#Part 2

#down and up X increase/decrease aim by X units. 
# forward X increases horizontal by X and depth by aim * X

aim <- 0
horizontal <- 0
depth <- 0

for(i in 1:nrow(data)){
  if(data[i,'direction'] == 'down'){aim <- aim + data[i,'X']}
  if(data[i,'direction'] == 'up'){aim <- aim - data[i,'X']}
  if(data[i,'direction'] == 'forward'){
                                horizontal <- horizontal + data[i,'X']
                                depth <- depth + aim*data[i,'X']
  }
}

answer2 <- depth * horizontal
print(answer2)
