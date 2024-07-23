# Anscombe.R

source("eiras.friendlycolor.R")
source("eiras.correg.R")

col <- friendlycolor(c(1,7,13,19))
pch <- 21:24

Anscombe <- datasets::anscombe

min.x <- 4 # min(c(Anscombe$x1,Anscombe$x2,Anscombe$x3,Anscombe$x4),na.rm=TRUE)
max.x <- 19 # max(c(Anscombe$x1,Anscombe$x2,Anscombe$x3,Anscombe$x4),na.rm=TRUE)
min.y <- 3 # min(c(Anscombe$y1,Anscombe$y2,Anscombe$y3,Anscombe$y4),na.rm=TRUE)
max.y <- 17 # max(c(Anscombe$y1,Anscombe$y2,Anscombe$y3,Anscombe$y4),na.rm=TRUE)

res <- list()
for (i in 1:4)
{
  if (i==1)
  {
    x <- Anscombe$x1
    y <- Anscombe$y1
  }
  if (i==2)
  {
    x <- Anscombe$x2
    y <- Anscombe$y2
  }
  if (i==3)
  {
    x <- Anscombe$x3
    y <- Anscombe$y3
  }
  if (i==4)
  {
    x <- Anscombe$x4
    y <- Anscombe$y4
  }
  
  res <- c(res,  correg (x, y,
                 alpha=0.05, B=0,
                 method = "lm",
                 jitter=0,
                 main=paste("conjunto ",i,sep=""),
                 xlab="x", ylab="y",
                 xlim=c(min.x,max.x), 
                 ylim=c(min.y,max.y), 
                 bg="transparent", col=col[i], pch=pch[i]))
}

