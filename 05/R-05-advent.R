library(tidyverse)

#setting up data -> turning "593,10 -> 593,98" format into 4 numeric variables x1,x2,y1,y2
data <- read.csv('05/day-5-input.csv',header=FALSE)
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),'->')))
data1 <- data.frame(do.call('rbind', strsplit(as.character(data$X1),','))) 
colnames(data1) <- c('x1','y1')
data2 <- data.frame(do.call('rbind', strsplit(as.character(data$X2),',')))
colnames(data2) <- c('x2','y2')
data <- cbind(data1,data2)
data[] <- lapply(data, function(x) as.numeric(x))

#for two sets of x,y co-ords, output integer points on line between - this could be simplified
draw_line <- function(x1,x2,y1,y2){
  #calculate gradient for us in cases which are not x1=x2 or y1=y2
  grad <- ifelse(is.infinite((y2-y1)/(x2-x1))|is.na((y2-y1)/(x2-x1)),0,(y2-y1)/(x2-x1))
  x <- NULL
  y <- NULL
  #direction of x and y
  sx <- sign(x2-x1)
  sy <- sign(y2-y1)
  #calculate x-coords
  x <- x1
  #Case where x is unchanged
  if(sx==0){x = array(x2,dim = 1+abs(y2-y1))}
  #if x changes, create an array which goes from x1 to x2
  else{ 
    for(i in 1:(abs(x2-x1))){
      x=c(x,tail(x, n=1)+(1*sx))
    }
  }
  #calculate y-coords
  y=y1
  for(i in 1:(length(x)-1)){
    #Case where y is unchanged
    if(y2==y1){y = array(y2,dim = abs(x2-x1))}
    #Case where x is unchanged
    else{if(sx==0){y <- c(y, tail(y, n=1) + 1*sy)}
    #case where both x and y change
    else{ y <- c(y, tail(y, n=1) + grad*sx)}}
  }
  #Output list of coords
  coords <- cbind(x,y)
return(coords)
}

#increment array values by 1 of coords provided
pop_array <- function(map, coords){
  for(i in 1:nrow(coords)){
      map[coords[i,1],coords[i,2]] <- map[coords[i,1],coords[i,2]] + 1
      if(map[coords[i,1],coords[i,2]] >50){break}
  }
  return(map)
}

#Final function initialises an array of 0s.
#Then loop through each row in data, calculate co-ords for that row, then updates the array.
#returns number of cells where value is >1 - our overlaps.

calculate_answer <- function(data){
  #Initialise array of 0s 
  map <- array(0, dim=c(max(data$x1,data$x2),max(data$y1,data$y2)))
  for(i in 1:nrow(data)){
    coords <- NULL
    coords <- draw_line(x1 = data[i,'x1'], x2 = data[i,'x2'], y1 = data[i,'y1'], y2 = data[i,'y2'])
    map <- pop_array(map,coords)
  }
  answer <- length(map[map>1])
  return(answer)
}

#Part 1
#Only consider lines where x1 = x2 or y1 = y2
data_p1 <- filter(data, x1==x2 | y1==y2)
#Answer 1
calculate_answer(data_p1)

#Part 2
#Now apply to all data
#Answer 2
calculate_answer(data)

 