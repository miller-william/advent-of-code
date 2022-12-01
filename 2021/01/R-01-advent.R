data <- read.csv('01/day-1-input.csv',header=FALSE)

#Part 1
count <- 0

for(i in 1:(nrow(data)-1)){
  if(data[i+1,1] > data[i,1] ){count <- count + 1}
}

#Answer to part 1
print(count)

#Part 2
count <- -1 #Not 0 because we don't want to count the first case where we don't have a previous sum to compare
old_sum <- 0

#number in rolling sum
n <- 3
#full cases
cases <- n * floor(nrow(data)/n)

for(i in 1:cases){
  new_sum <- sum(data[i:(i+(n-1)),'V1'])
  if(new_sum > old_sum){count <- count + 1}
  old_sum <- new_sum
}

#Answer to part 2
print(count)

