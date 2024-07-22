# eiras.plot.barmeansd.R

plot.showmeansd <- function(x.mean, x.sd, color, y.pos)
{
  tp <- 44
  for (i in -3:3)
  {
    lines(c(x.mean-i*x.sd, x.mean),
          c(y.pos,y.pos),
          lwd=10, lty=1, col = paste(color,tp,sep="") )
  }
}

plot.barmeansd <- function(x.values, color, y.pos)
{
  x.mean <- mean(x.values,na.rm=TRUE)
  x.sd <- sd(x.values,na.rm=TRUE)
  plot.showmeansd(x.mean, x.sd, color, y.pos)  
}
