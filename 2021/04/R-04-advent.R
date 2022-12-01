#This is a really bad/ inefficient / inelegant way to solve this...

library(tidyverse)
data <- read.csv('04/day-4-input.csv',header=FALSE)

pick_order <- data[1,1]
pick_order <- as.numeric(strsplit(pick_order, ",")[[1]])

data <- (data[-1,])
data <- (strsplit(data, " "))
#remove blank entries from strsplit output
data <- lapply(data, function(z){ z[!is.na(z) & z != ""]})
data <- t(as.data.frame(data))
rownames(data) <- NULL
bingo <- as.data.frame(data)
bingo[] <- lapply(bingo, function(x) as.numeric(x))

group <- 1 
count <- 1

#Group each bingo table
for(i in 1:nrow(bingo)){
  bingo[i,"group"] <- group
  count <- count + 1
  if(count==6){
    count <- 1
    group <- group + 1
  }
}

bingo_backup <- bingo

#for each number in pick_order, remove it from the table, then check winning conditions
# if a row is all blank - win
# if a column is all blank - win

winner <- ""

for(i in 1:length(pick_order)){
  
  #replace all pick_order values with blanks
  bingo[,1:5][bingo[,1:5] == paste0(pick_order[i])] <- ""
  #check rows by counting blanks in a row
  bingo$winning_row <- rowSums(bingo == "")
  if(max(bingo$winning_row)==5){bingo_g <- filter(bingo,winning_row==5);winner <- bingo_g$group ;bingo_g <- filter(bingo,group == winner); winning_num <- pick_order[i]; break}
  #check columns by looping through groups
    for(j in 1:max(bingo$group)){
      bingo_g <- filter(bingo,group == j)
      if(sum(bingo_g$V1 == "")==5 ){winner <- j;break}
      if(sum(bingo_g$V2 == "")==5 ){winner <- j;break}
      if(sum(bingo_g$V3 == "")==5 ){winner <- j;break}
      if(sum(bingo_g$V4 == "")==5 ){winner <- j;break}
      if(sum(bingo_g$V5 == "")==5 ){winner <- j;break}
    }
    if(winner != ""){winning_num <- pick_order[i]; break}
}

answer <- as.numeric(winning_num) * sum(as.numeric(bingo_g$V1), 
                         as.numeric(bingo_g$V2),
                         as.numeric(bingo_g$V3), 
                         as.numeric(bingo_g$V4), 
                         as.numeric(bingo_g$V5), na.rm = T)
print(answer)

#Part 2 - want last one to win. Modify code above
bingo <- bingo_backup

win_count <- 0
winners <- NULL
last_winning_num <- NULL

for(i in 1:length(pick_order)){
  winner <- ""
  #replace all pick_order values with blanks
  bingo[,1:5][bingo[,1:5] == paste0(pick_order[i])] <- ""
  #check rows by counting blanks in a row
  bingo$winning_row <- rowSums(bingo == "")
  if(max(bingo$winning_row)==5){
    #Filter for rows with 5 blanks that AREN'T already in our winners list
    bingo_g <- filter(bingo,winning_row==5 & !group %in% winners);
    #List of new row condition winners
    winner <- unique(bingo_g$group);
    #Add new row winners to list of winners - only keep unique ones
    winners <- unique(c(winners, winner))
  }
  #Check how many unique winners we have - if 100, we are done
  if(length(winners)==100){last_winning_num <- pick_order[i]; bingo_g <- filter(bingo,group == winner); debug <- 1;break}
  
  #Now do column win condition with a brute force loop (sure there's a better way, by only checking non-winners but here goes... )
  for(j in (unique(bingo$group))){
    bingo_g <- filter(bingo,group == j)
    if(sum(bingo_g$V1 == "")==5 ){winner <- j;winners <- unique(c(winners, winner))}
    if(sum(bingo_g$V2 == "")==5 ){winner <- j;winners <- unique(c(winners, winner))}
    if(sum(bingo_g$V3 == "")==5 ){winner <- j;winners <- unique(c(winners, winner))}
    if(sum(bingo_g$V4 == "")==5 ){winner <- j;winners <- unique(c(winners, winner))}
    if(sum(bingo_g$V5 == "")==5 ){winner <- j;winners <- unique(c(winners, winner))}
  }
  #Check how many unique winners we have - if 100, we are done
  if(length(winners)==100){last_winning_num <- pick_order[i]; bingo_g <- filter(bingo,group == winner); debug <- 2; break}
  
  }

answer2 <- as.numeric(last_winning_num) * sum(as.numeric(bingo_g$V1), 
                                        as.numeric(bingo_g$V2),
                                        as.numeric(bingo_g$V3), 
                                        as.numeric(bingo_g$V4), 
                                        as.numeric(bingo_g$V5), na.rm = T)
print(answer2)
