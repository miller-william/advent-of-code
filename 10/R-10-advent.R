library(tidyverse)
library(stringr)
library(purrr)

#comments to be added.....

data <- read.csv('10/day-10-input.csv', header=FALSE, colClasses = "character")

#Part1
new_sum <- 0
sum <- 0

# remove closed chunks from string
for(row in 1:nrow(data)){
      for(i in 1:100){
    
      data[row,1] <- str_replace_all(data[row,1], "\\[\\]", "")%>% 
             str_replace_all("\\{\\}", "") %>%
               str_replace_all("\\(\\)", "") %>%
                str_replace_all("<>", "") 
      }

  if( grepl("\\}",data[row,1]) | grepl("\\]",data[row,1]) | grepl("\\)",data[row,1]) | grepl(">",data[row,1])) {
    
    illegal <- str_replace_all(data[row,1], "\\[", "") %>%
               str_replace_all("\\{", "") %>%
               str_replace_all("\\(", "") %>%
               str_replace_all("<", "") %>%
               substr(1,1)
    
    print(illegal)
    if(illegal == ")"){new_sum <- 3}
    if(illegal == "]"){new_sum <- 57}
    if(illegal == "}"){new_sum <- 1197}
    if(illegal == ">"){new_sum <- 25137}

    data[row,1] <- "corrupt"
  }
    print(paste0("row is", row))
    print(paste0("new_sum is", new_sum))
    sum <- sum + new_sum
    print(sum)
    new_sum <- 0
    
    
    }

#Part 2

data_p2 <- filter(data, V1 != 'corrupt')
data_p2$score <- 0
score <- 0

for(row in 1:nrow(data_p2)){
data_p2[row,1] <- str_replace_all(data_p2[row,1], "\\[", "]")%>% 
  str_replace_all("\\{", "}") %>%
  str_replace_all("\\(", ")") %>%
  str_replace_all("<", ">")

  data_p2[row,1] <- intToUtf8(rev(utf8ToInt(data_p2[row,1])))
  
  score <- 0
  for(n in 1:nchar(data_p2[row,1])){
    char_n <- substr(data_p2[row,1],n,n)
    if(char_n == ")"){score_n <- 1}
    if(char_n == "]"){score_n <- 2}
    if(char_n == "}"){score_n <- 3}
    if(char_n == ">"){score_n <- 4}
    
    score <- (5* score) + score_n
    
  }
  data_p2[row,2] <- score
  
}

#answer 2
sort(data_p2$score)[median(1:nrow(data_p2))]