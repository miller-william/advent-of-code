library(GGally)
library(network)
library(sna)
library(ggplot2)

data <- read.csv('12/day-12-input.csv', header=FALSE, colClasses = "character")
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),'-')))

#list of all valid path routes in both directions - except where starts with end or ends with start

data <- cbind(c(data[,1], data[,2]), c(data[,2], data[,1]))
data <- data[data[,2] != "start",]
data <- data[data[,1] != "end",]
#get small cave letters
sc <- setdiff(data[grepl("[a-z]", data[,1]), 1], c("start", "end"))


######### Part 1 ################

position <- "A"

#recursive function

path_count <- function(position, sc_vec) {
  
  if (position == "end") return(1)
  if (position %in% sc) {
    #keep counter for lowercase letters as can only visit once
    print(position); print(sc_vec); 
    sc_vec[position] <- sc_vec[position] + 1L
  }
  #record 'id' : this position and current state of sc_vec
  id <- paste(position, paste0(sc_vec, collapse = ""), sep = "_")
  
  if (id %in% names(envir$res)) return(envir$res[id])
  #if visited a small cave more than once, not valid path
  if (sum(sc_vec > 1) > 0 ) {
    res <- 0
  } else
  {
    #list of caves that could be moved to - e.g. where current position is in first column of data
    # AND not a small cave already visited
    ne <- setdiff(data[data[,1] == position, 2], names(sc_vec[sc_vec > 1]))
    
    #apply function recursively to these cave options
    if(length(ne) == 0){ res <- 0L} else{res <- sum(sapply(ne,  path_count, sc_vec))}
  }
  
  envir$res <- c(envir$res, setNames(res, id))
  return(res)
  
}

envir <- environment()
envir$res <- NULL
sc_vec <- setNames(rep(0, length(sc)), sc)
path_count("start", sc_vec)

### Part 2 ###

#  Modify function above so visit a single small cave twice. but max

p <- "start"

path_count_p2 <- function(position, sc_vec) {
  
  
  if (position == "end") return(1)
  if (position %in% sc) {
    #keep counter for lowercase letters as can only visit once
    print(position); print(sc_vec); 
    sc_vec[position] <- sc_vec[position] + 1L
  }
  
  id <- paste(position, paste0(sc_vec, collapse = ""), sep = "_")
  
  if (id %in% names(envir$res)) return(envir$res[id])
  #if visited a small cave more than once, not valid path
  if ((any(sc_vec > 2) | sum(sc_vec > 1) > 1)) {
    res <- 0
  } else
  {
    #list of caves that could be moved to - e.g. where current position is in first column of data
    # AND not a small cave already visited
    ne <- setdiff(data[data[,1] == position, 2], names(sc_vec[sc_vec > 1]))
    if(length(ne) == 0){ res <- 0L} else{res <- sum(sapply(ne,  path_count_p2, sc_vec = sc_vec))}
  }
  
  envir$res <- c(envir$res, setNames(res, id))
  return(res)
  
}

envir <- environment()
envir$res <- NULL
sc_vec <- setNames(rep(0, length(sc)), sc)
path_count_p2("start", sc_vec)





#######################################################

test <- as.data.frame(c("start-A",
          "start-b",
          "A-c",
          "A-b",
          "b-d",
          "A-end",
          "b-end"))
data <- test
names(data) <- "V1"





caves <- sort(unique(append(data$X1,data$X2)))

cave_net <- array(0, dim = c(length(caves),length(caves)))

colnames(cave_net) <- caves
rownames(cave_net) <- caves

cave_net

for(i in 1:nrow(data)){
  cave1 <- data[i,1]
  cave2 <- data[i,2]
  cave_net[cave1,cave2] <- 1
  cave_net[cave2,cave1] <- 1

}
cave_net

cave_net <- as.data.frame(cave_net)

path <- "start"

for(caves in c("A",  "b", "c", "d", "end")){
  
  if(cave_net[path[length((path))],caves] == 1){path <- c(path,caves) }
  
  }















cave_net_graph <- graph_from_adjacency_matrix(cave_net)
paths <- all_simple_paths(cave_net_graph, from = "start", to = "end" )







# random graph




cave_net <- network(cave_net, directed = FALSE)

ggnet2(cave_net,node.label = caves)



# random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]
ggnet2(net, label.nodes=T)


require(network); require(GGally)
nw <- network(matrix(c(0,1,1,0),2))
network.vertex.names(nw)
network.vertex.names(nw) <- c("a","b")
ggnet(nw,node.group=c("A","B"),label.nodes=T, col="white")





?all_simple_paths

#####stack overflow method
# Find paths from node index n to m using adjacency list a.
adjlist_find_paths <- function(a, n, m, path = list()) {
  path <- c(path, list(n))
  if (n == m) {
    return(list(path))
  } else {
    paths = list()
    for (child in a[[n]]) {
      if (!child %in% unlist(path)) {
        child_paths <- adjlist_find_paths(a, child, m, path)
        paths <- c(paths, child_paths)
      }
    }
    return(paths)
  }
}

# Find paths in graph from vertex source to vertex dest.
paths_from_to <- function(graph, source, dest) {
  a <- as_adj_list(graph, mode = "out")
  paths <- adjlist_find_paths(a, source, dest)
  lapply(paths, function(path) {V(graph)[unlist(path)]})
}


