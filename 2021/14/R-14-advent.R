library(data.table)
library(stringr)
library(stringi)

data <- read.csv('14/day-14-input.csv', header=FALSE, colClasses = "character")
poly <- data[1,1]
data <- data[2:nrow(data),1]

data <- data.frame(do.call('rbind', strsplit((data),' -> ')))
names(data) <- c("input","out")
data <- as.data.table(data)

for(step in 1:40){
  
  print(step)
  insert <- array(0)
  
  #working out which character needs to be inserted
  for(x in 1:(nchar(poly)-1)){
    pair <- substr(poly,x,x+1)
    #print(pair)
    insert[x] <- as.character(data[data$input == pair,2])
  }
  
  #inserting each insert into the right place
  for(n in 1:length(insert)){
    pos <- (2*n)
    stri_sub(poly, pos, pos-1) <- insert[n]
    #poly <- str_insert(poly,pos,insert[n])
    #print(insert[n])
  }
print(poly)
}

uniques <- unique(strsplit(poly, "")[[1]]) 
print(uniques)
str_count(poly, uniques)

#Part 2
#predictably, I now need to not create one massive vector..
#Let's keep a count of the pairs and count every time we add a letter

#two new pairs created for each pair, every iteration

for(i in 1:nrow(data)){
  #create columns for each of the two individual pairs that are created when you add a letter to an input
  data[i,'out1'] <- paste0(substr(data[i,'input'],1,1), data[i,'out'])
  data[i,'out2'] <- paste0(data[i,'out'],substr(data[i,'input'],2,2))
}

data$count <- 0
#initialise counts with starting poly
for(x in 1:(nchar(poly)-1)){
  pair <- substr(poly,x,x+1)
  data[input == pair,'count'] <- data[input == pair,'count'] + 1
}

data$count_temp <- 0
data$count_letter <- 0

for(steps in 1:40){

    #for each pair, add its out1 and out2 to temp total
    for(i in 1:nrow(data)){
      print(i)
      
      out1_t <- as.character(data[i,'out1'])
      out2_t <- as.character(data[i,'out2'])
      
      #count of the new pairs created
      data[input == out1_t,'count_temp'] <- data[input == out1_t,'count_temp'] + data[i,'count']
      data[input == out2_t,'count_temp'] <- data[input == out2_t,'count_temp'] + data[i,'count']
      
      #count of the letters added
      data[i,'count_letter'] <- data[i,'count_letter'] + data[i,'count']
      
      
    }
    
    data$count <- data$count_temp
    data$count_temp <- 0

}

#count letters
counts <- array()
for(letter in unique(data$out)){
  print(letter)
  print(sum(data[out==letter,count_letter]))
  counts[letter] <- sum(data[out==letter,count_letter])
  
}
counts <- counts[is.na(counts)==F]

#need to remember the starting letters!
#count them and add them to counts here
for(i in 1:length(counts)){
  counts[names(counts[i])] <- counts[names(counts[i])] + str_count(poly,names(counts[i]))
}

options(scipen = 200, digits = 4)

#answer 2
max(counts) - min(counts)
counts[counts==max(counts)] - counts[counts==min(counts)]
 
