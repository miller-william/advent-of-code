library(tidyverse)
library(stringr)
library(purrr)

options(scipen = 200, digits = 4)
data <- read.csv('09/day-09-input.csv', header=FALSE, colClasses = "character")
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),'')))
data[] <- lapply(data, function(x) as.numeric(x))

#Part 1 - Crunching it manually... Could definitely simplify using functions

low_points  <- array(dim= c(1,2))
  heights <- array(dim = 1)

for(row in 1:nrow(data)){
  for(col in 1:ncol(data)){
    
    #Top edge
    if(row==1 & col!=1 & col!=100){    
      if(data[row,col] < data[row,col-1]){
        if(data[row,col] < data[row,col+1]){
          if(data[row,col] < data[row+1,col]){
            
            low_points <- rbind(low_points, c(row,col))
            heights <- rbind(heights,data[row,col])
          }
         }
        }
      }
      #body

    #bottom edge
    if(row==100 & col!=1 & col!=100){ 
    if(data[row,col] < data[row-1,col]){
      if(data[row,col] < data[row,col-1]){
        if(data[row,col] < data[row,col+1]){
            
            low_points <- rbind(low_points, c(row,col))
            heights <- rbind(heights,data[row,col])
          }
        }
      }
    }
    #left edge
    if(col==1 & row!=1 & row!=100){ 
      if(data[row,col] < data[row-1,col]){
          if(data[row,col] < data[row,col+1]){
            if(data[row,col] < data[row+1,col]){
              
              low_points <- rbind(low_points, c(row,col))
              heights <- rbind(heights,data[row,col])
          }
        }
      }
    }
    #right edge
    if(col==100 & row!=1 & row!=100){
    if(data[row,col] < data[row-1,col]){
      if(data[row,col] < data[row,col-1]){
          if(data[row,col] < data[row+1,col]){
            
            low_points <- rbind(low_points, c(row,col))
            heights <- rbind(heights,data[row,col])
          }
        }
      }
    }
    
    #bottom left
    if(row==100 & col ==1){ 
        if(data[row,col] < data[row,col+1]){
          if(data[row,col] < data[row-1,col]){
            
            low_points <- rbind(low_points, c(row,col))
            heights <- rbind(heights,data[row,col])
        }
      }
    }
    
    #top left
    if(row==1 & col ==1){ 
      if(data[row,col] < data[row,col+1]){
        if(data[row,col] < data[row+1,col]){
          
          low_points <- rbind(low_points, c(row,col))
          heights <- rbind(heights,data[row,col])
        }
      }
    }
    #top right
    if(row==1 & col ==100){ 
      if(data[row,col] < data[row,col-1]){
        if(data[row,col] < data[row+1,col]){
          
          low_points <- rbind(low_points, c(row,col))
          heights <- rbind(heights,data[row,col])
        }
      }
    }
    #bottom right
    if(row==100 & col ==100){ 
      if(data[row,col] < data[row,col-1]){
        if(data[row,col] < data[row-1,col]){
          
          low_points <- rbind(low_points, c(row,col))
          heights <- rbind(heights,data[row,col])
        }
      }
    }
    #body
    if(row != 1 & row != 100 & col != 1 & col != 100){
    if(data[row,col] < data[row-1,col]){
      if(data[row,col] < data[row,col-1]){
        if(data[row,col] < data[row,col+1]){
          if(data[row,col] < data[row+1,col]){
            
            low_points <- rbind(low_points, c(row,col))
            heights <- rbind(heights,data[row,col])
          }
        }
      }
    }
    }
    
}
}

  heights <- heights[2:length(heights)]
  low_points[] <- low_points[is.na(low_points)==F]
  
#answer1
print(sum(heights+1))

#part2 
#This can be achieved quite easily via raster package
library(igraph)
library(raster)

#shift all data up by 1 and turn boundary points to zeros
data <- data + 1
data[data == 10] <- 0
data[] <- lapply(data, function(x) as.numeric(x))
mat <- as.matrix(data)
image(mat)

#create raster layer object for clump()
Rmat <- raster(mat)
Clumps <- as.matrix(clump(Rmat, directions=4))
ClumpsImage <- Clumps
ClumpsImage[ClumpsImage>0] <- 1
image(ClumpsImage)

#turn the clumps into a list
tot <- max(Clumps, na.rm=TRUE)
res <- vector("list",tot)
for (i in 1:tot){
  res[i] <- list(which(Clumps == i, arr.ind = TRUE))
  
}
clump_size <- array()
for (i in 1:tot){
  clump_size[i] <- (sum(is.na(Clumps[Clumps==i])==F))
}

#answer2
print(prod(sort(clump_size, decreasing = T )[1:3]))
