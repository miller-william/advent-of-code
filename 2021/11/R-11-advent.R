data <- read.csv('11/day-11-input.csv', header=FALSE, colClasses = "character")
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),'')))
data[] <- lapply(data, function(x) as.integer(x))

#Set up array of 0s which is going to record whether a position has flashed in each step
flashes <- data.frame(array(0,dim = c(nrow(data),ncol(data))))
#This will record new flashes in each step - with hindsight could actually count this via flashes at end of each step....
total_flashes <- 0

for(loop in 1:2000){
  
  print(paste0("Iteration: ",loop))
  
  #for each position - increment by one then resolve flashes
  for (row in 1:nrow(data)){
    for(col in 1:ncol(data)){
    
      #primary flashes
      data[row,col] <- data[row,col] + 1
      #flash!
      #if a position becomes 10, mark flashes array with 1
        if(data[row,col] == 10){
          if(flashes[row,col] != 1){
          flashes[row,col] <- 1
          total_flashes <- total_flashes + 1
          }
        }
    }
  }

      #resolve secondary flashes
      # use a while loop to keep looping until all flashes have incremented their adjacent rows. 
      # When we increment to a new '10' we update the flashes array.
      # When we've adjusted adjacent cells for a given flash, we mark it as a negative 1 in the flash array so we know it's done
      while (sum(flashes==1) > 0 ){
        for (row2 in 1:nrow(data)){
          for(col2 in 1:ncol(data)){
              if(flashes[row2,col2]==1){
                #print(paste0("secondary flash at ",row2,",",col2))
                for(i in -1:1){
                  #loop and increment adjacent co-ords
                  for(j in -1:1){
                      if((i==0 & j==0)==F){
                    if( (row2+i == 0 | col2+j == 0 | row2+i > 10 | col2+j > 10 ) == F ){
                      #print(paste0("row and col: ", row2+i,",",col2+j))
                      data[row2+i,col2+j] <- data[row2+i,col2+j] + 1
                      #if there's a secondary flash
                      #if exactly 10 (e.g. first time flash, turn on flash)
                       if(data[row2+i,col2+j] == 10){
                         #print(paste0("tertiary flash! At ",row2+i,",",col2+j))
                         #print(data)
                         total_flashes <- total_flashes + 1
                            if(flashes[row2+i,col2+j] != 1){flashes[row2+i,col2+j] <- 1}
                         }
                    }
                    }
                  }
                }
                # -1 indicates adjacent flash impacts have been accounted for
                flashes[row2,col2] <- -1
                
                 }
          } 
         }
       
  }
  if(loop==100){print(paste0("Part 1 answer is:",total_flashes));answer <- total_flashes}
  if(sum(flashes)==-100){print("Simultaneous flash!");answer2 <- loop; break}
  
  flashes[flashes < 0] <- 0
  data[data>9] <- 0
  #print(data)

}

#Part 1 answer
print(answer)

#Part 2 answer
print(answer2)