library(igraph)
library(dplyr)
library(magrittr)
library(stringr)

mat <- read.csv('day-15-input.csv', header=FALSE, colClasses = "character")

# example
mat <- as.data.frame(read.table(text=
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581", header=F))

mat <- data.frame(do.call('rbind', strsplit((as.character(mat$V1)),'')))
mat <- as.matrix(mat)

#part 1

#for each ij, it has an edge with i+1,j : i, j+1 : i-1,j : i, j-1

edge <- list()
weights <- list()

#generate edge and weights matrix from the data
#if loops are for the corner and edge cases
#this takes a really long time when run for part 2.... 
for(i in 1:(nrow(mat))){
  for(j in 1:(ncol(mat))){
    
    if(i==1 & j == 1){ 
    edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j), paste0(i,'-',j+1))
    weights[[paste0(i,'-', j)]] <- c(mat[i+1,j] ,mat[i,j+1])
    }
    else if(i==1 & j==ncol(mat)){
      edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j), paste0(i,'-',j-1))
      weights[[paste0(i,'-', j)]] <- c(mat[i+1,j], mat[i,j-1])
    }
    else if(i==nrow(mat) & j==1){
      edge[[paste0(i,'-', j)]] <- c(paste0(i,'-',j+1), paste0(i-1,'-',j))
      weights[[paste0(i,'-', j)]] <- c(mat[i,j+1] ,mat[i-1,j])
    }
    else if(i==nrow(mat) & j==ncol(mat)){ 
      edge[[paste0(i,'-', j)]] <- c(paste0(i-1,'-',j), paste0(i,'-',j-1))
      weights[[paste0(i,'-', j)]] <- c(mat[i-1,j], mat[i,j-1])}
    else if(i==1){ 
      edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j), paste0(i,'-',j+1),paste0(i,'-',j-1))
      weights[[paste0(i,'-', j)]] <- c(mat[i+1,j] ,mat[i,j+1] , mat[i,j-1])}
    else if(i==nrow(mat)){ 
      edge[[paste0(i,'-', j)]] <- c( paste0(i,'-',j+1), paste0(i-1,'-',j), paste0(i,'-',j-1))
      weights[[paste0(i,'-', j)]] <- c(mat[i,j+1] ,mat[i-1,j], mat[i,j-1])}
    else if(j==1){
      edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j), paste0(i,'-',j+1), paste0(i-1,'-',j))
      weights[[paste0(i,'-', j)]] <- c(mat[i+1,j] ,mat[i,j+1] ,mat[i-1,j])
    }
    else if(j==ncol(mat)){
      edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j),paste0(i-1,'-',j), paste0(i,'-',j-1))
      weights[[paste0(i,'-', j)]] <- c(mat[i+1,j] ,mat[i-1,j], mat[i,j-1])
    }    
    else{
          edge[[paste0(i,'-', j)]] <- c(paste0(i+1,'-',j), paste0(i,'-',j+1), paste0(i-1,'-',j), paste0(i,'-',j-1))
          weights[[paste0(i,'-', j)]] <- c(mat[i+1,j] ,mat[i,j+1] ,mat[i-1,j], mat[i,j-1]) }
  }
  
}

# create edgelist with weights
G <- data.frame(stack(edge), weights = stack(weights)[[1]])

el <- as.matrix(stack(edge))
g <- graph_from_edgelist(el)
edge.attributes(g)$weight <- G$weights

sp <- (shortest_paths(g,from = "1-1", to = "500-500"))

get_mat_value_at_pos <- function(string_pos){
  i <- as.integer(sapply(str_split(string_pos,"-"), "[[", 1))
  j <- as.integer(sapply(str_split(string_pos,"-"), "[[", 2))
  return(as.integer(mat[i,j]))
}

sum <- 0
for( i in 2:length(sp$vpath[[1]]$name)){
sum <- sum + get_mat_value_at_pos((sp$vpath[[1]][i]$name))
}
#Answer part 1
print(sum)


#Part 2

#Need to create new bigger matrix.

#increment matrix and append

increment_func <- function(x){
  x <- as.integer(x)
  if(x==9){y <- 1}
  else{y <- x+1}
  return(y)
}

increment_func(mat[1,1])


#extend row-wise

mat2 <- mat
mat2[] <- sapply(mat,FUN=increment_func)

mat3 <- mat2
mat3[] <- sapply(mat2,FUN=increment_func)

mat4 <- mat3
mat4[] <- sapply(mat3,FUN=increment_func)

mat5 <- mat4
mat5[] <- sapply(mat4,FUN=increment_func)

mat <- cbind(mat,mat2,mat3,mat4,mat5)

#extend column-wise

mat2 <- mat
mat2[] <- sapply(mat,FUN=increment_func)

mat3 <- mat2
mat3[] <- sapply(mat2,FUN=increment_func)

mat4 <- mat3
mat4[] <- sapply(mat3,FUN=increment_func)

mat5 <- mat4
mat5[] <- sapply(mat4,FUN=increment_func)

mat <- rbind(mat,mat2,mat3,mat4,mat5)

#now we do the same as we did for part 1

