

#Example
#target area: x=20..30, y=-10..-5

#target area: x=144..178, y=-100..-76

#The probe's x position increases by its x velocity.
#The probe's y position increases by its y velocity.
#Due to drag, the probe's x velocity changes by 1 toward the value 0; that is, it decreases by 1 if it is greater than 0, increases by 1 if it is less than 0, or does not change if it is already 0.
#Due to gravity, the probe's y velocity decreases by 1.

ymax <- -76
ymin <- -100
xmax <- 178
xmin <- 144

#Function takes a starting velocity and iterates 100 steps. Outputs starting velocity, whether it hit zone, and max_height reached

launch <- function(xv,yv, steps_n=100, p2=FALSE){
    
  x <- 0
  y <- 0
  z <- 0
  start_xv <- xv
  start_yv <- yv
  max_height <- y
  plot_x <- c()
  plot_y <- c()
  
  #movement
  for(steps in 1:steps_n){
        x <- x + xv
        y <- y + yv
        
        plot_x <- c(plot_x,x)
        plot_y <- c(plot_y,y)
        
        
        if(y > max_height){max_height <- y}
        
        #print(paste0("step: ", steps))
        #print(x)
        #print(y)
        
        #check position
        if(x <= xmax & x >= xmin & y <= ymax & y >= ymin){z <- 1; 
        #print("in zone!");
        print(max_height); break;}
        #check if past or below the point
        if(x > xmax){
          #print(paste0("Past the zone: x=",x));
          break}
        if(y < ymin & yv < 0){
          #print("Below the zone and dropping"); 
          break;}
        
        if(xv > 0){xv <- xv - 1}
        if(xv < 0){xv <- xv + 1}
        yv <- yv - 1 
        
  
  }
  
  if(p2==FALSE){
    output$xv <- start_xv
    output$yv <- start_yv
    output$z <- z
    output$max_height <- max_height
    return(output)
  }
  if(p2==TRUE & z==1){
    output$z <- z
    
  }
  #plot(plot_x,plot_y)

}


#Loop through launch function to find answers to part 1 and 2

#part 1
output <- list()
ans1 <- 0

for(i in 1:100){
  for(j in 1:200){
    #print(paste0("xv: ",i," - ","yv: ",j))
    output <- launch(i,j,10000, p2=FALSE)
    if(output$max_height > ans1 & output$z == 1){ans1 <- output$max_height}
  }
}
print(ans1)

#part 2
count <- c()
#set limits as getting into the edge of the box in one step

for(i in 1:179){
  for(j in -100:200){
    #print(paste0("xv: ",i," - ","yv: ",j))
    count <- c(count,launch(i,j,10000,p2=TRUE))
  }
}
#answer 2
print(length(count))

