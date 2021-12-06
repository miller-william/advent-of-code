library(tidyverse)

#setting up data
data <- read.csv('06/day-06-input.csv',header=FALSE)
data <- data.frame(do.call('rbind', strsplit(as.character(data$V1),',')))
data <- t(data)
colnames(data) <- "x"
#data <- as.data.frame(data)
#data[] <- lapply(data, function(x) as.integer(x))
data<- as.integer(data)
#Rules
#Count number of 0
#Decrease every number by 1 except 0 -> 6
# Add new rows of 8 for each 0
data_master <- data

update_num <- function(x){
  if(x == 0){x <- 6}
  else{x <- x-1}
  return(as.integer(x))
}

#Part 1
test <- NULL
count <- NULL
model_data <- array()

for(day in 1:80){
  print(day)
  new <- sum(data==0)
  data <- sapply(data, function(x) update_num(x))
  new_rows <- (seq(8,8, length.out = new))
  data <- c(data,new_rows)
  model_data[day] <- length(data)
  count <- c(count,sum(data==0))
}

#Answer 1
print(length(data))

#Part 2

new_fish <- c(sum(data == 0),
        sum(data == 1),
        sum(data == 2),
         sum(data == 3),
         sum(data == 4),
         sum(data == 5),
         sum(data == 6),
          0,
          0) %>% as.integer()

fish <- (seq(0,0, length.out = 7))
count <- as.data.frame(count)

for(day in 1:256){
  
  new <- fish[1]
  old <- new_fish[1]
  
  for (i in 1:6){
    fish[i] <- fish[i+1]
  }

  for (i in 1:8){
    new_fish[i] <- new_fish[i+1]
  }
  
  fish[7] <- new + old
  new_fish[9] <- new + old
  count[day,2] <- (fish[1] + new_fish[1])
}

#Part 2 Answer
answer2 <- sum(fish) + sum(new_fish)
print(sum(fish) + sum(new_fish))
  
#### Just for 'fun' ########
#build exponential model to fit data
#Taken from here - https://rpubs.com/mengxu/exponential-model

model_data <- cbind(1:80,model_data)
colnames(model_data) <- c("day","fish")
model_data <- as.data.frame(model_data)

# Select an approximate $\theta$, since theta must be lower than min(y), and greater than zero
theta.0 <- min(model_data$fish) * 0.5  

# Estimate the rest parameters using a linear model
model.0 <- lm(log(fish - theta.0) ~ day, model_data) 
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
start

model <- nls(fish ~ alpha * exp(beta * day) + theta , data = model_data, start = start)

# Plot fitted curve
plot(model_data$day, model_data$fish)
lines(model_data$day, predict(model, list(day = model_data$day)), col = 'skyblue', lwd = 3)

options(scipen = 100, digits = 4)
predicted_answer <- predict(model, list(day=256))

difference <- abs((answer2 - predicted_answer) / answer2)
print(difference)
#close but not good enough :(
