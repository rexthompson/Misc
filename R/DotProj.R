## This is a short piece of code I wrote to help visualize the concepts at play in one of the theoretical proofs
## my brother and I worked on together for one of his math classes. The theory says that for n red dots and n
## blue dots, it is always possible to connect pairs of red and blue dots such that no two lines intersect.

drawDots <- function(size=7, grouped=FALSE) {
  
  if ( grouped ) {
    red_x <- sample(x=c(750:1250),size=size,replace = FALSE)
    red_y <- sample(x=c(750:1250),size=size,replace = FALSE)
    blue_x <- sample(x=c(0:500,1500:2000),size=size,replace = FALSE)
    blue_y <- sample(x=c(0:500,1500:2000),size=size,replace = FALSE)    
  } else {
    red_x <- sample(x=2000,size=size,replace = FALSE)
    red_y <- sample(x=2000,size=size,replace = FALSE)
    blue_x <- sample(x=2000,size=size,replace = FALSE)
    blue_y <- sample(x=2000,size=size,replace = FALSE)
  }
  
  plot(0,0,col='transparent',xlim=c(0,2000),ylim=c(0,2000),axes = FALSE,xlab='',ylab=''); box()
  
  n <- 0
  for (i in 1:size) {
    for (j in 1:size) {
      lines(x=c(red_x[i],blue_x[j]),y=c(red_y[i],blue_y[j]),col=adjustcolor('black',0.5))
      n <- n+1
    }
  }
  
  points(red_x,red_y,pch=16,col='red')
  points(blue_x,blue_y,pch=16,col='blue')
  
  return(n)
}
