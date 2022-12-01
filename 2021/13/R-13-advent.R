library(tidyverse)

data <- read.csv('13/day-13-input.csv', header=FALSE, colClasses = "character")
instructions <- data[723:734,]
data <- data[1:722,]
data <- data.frame(do.call('rbind', strsplit((data),',')))
data[] <- lapply(data, function(x) as.integer(x))
names(data) <- c('x','y')

#extract instruction info
instructions <- as.data.frame(instructions)

instructions$coord <- substr(instructions$instructions,12,12)
instructions$number <- as.numeric(gsub(".*?([0-9]+).*", "\\1", instructions$instructions))

#Part 1

#need to define new x-coord system with respect to fold line
#for every x,y coord, new x coord is abs(x-655)

coord <- instructions[1,2]
number <- instructions[1,3]

data_p1 <- data

data_p1[,coord] <- abs(data_p1[,coord] - number)

#Answer 1
nrow(unique(data_p1))

#Part 2

data_p2 <- data

for(i in 1:nrow(instructions)){
  coord <- instructions[i,2]
  number <- instructions[i,3]
  plot(data_p2, main = paste0(instructions[i,1]))
  
  for(c in 1:nrow(data_p2)){
       pos <- data_p2[c,coord]
       
      #need to change the coord if it is beyond the fold point
      if(pos > number ){
        data_p2[c,coord] <- number - abs(data_p2[c,coord] - number)
      }
       
    }
    
    data_p2 <- distinct(data_p2)
    print(nrow(data_p2))
  
}

#Because y increases top to bottom
data_p2$y <- - data_p2$y

#Answer 2
plot(data_p2, pch=19, xlim=c(0,40), ylim=c(-40,0))
