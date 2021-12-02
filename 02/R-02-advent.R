library(tidyverse)
data <- read.csv('02/day-2-input.csv',header=FALSE)

#Part 1

#Splitting string into two features
data <- transpose(strsplit(data$V1," "))
data <- as.data.frame(do.call(cbind, data))
data$V2 <- as.numeric(data$V2)

#Counting different directions
forward <- sum(select(filter(data, V1 == 'forward'),'V2'))
down <- sum(select(filter(data, V1 == 'down'),'V2'))
up <- sum(select(filter(data, V1 == 'up'),'V2'))

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
  if(data[i,'V1'] == 'down'){aim <- aim + data[i,'V2']}
  if(data[i,'V1'] == 'up'){aim <- aim - data[i,'V2']}
  if(data[i,'V1'] == 'forward'){
                                horizontal <- horizontal + data[i,'V2']
                                depth <- depth + aim*data[i,'V2']
  }
}

answer2 <- depth * horizontal
print(answer2)
