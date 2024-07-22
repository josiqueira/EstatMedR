# eiras.plot.density.withmeansd.R

source("eiras.friendlycolor.R")
source("eiras.plot.density.empty.R")
source("eiras.plot.barmeansd.R")

plot.density.singleline <- function(values, col)
{
  dvalues <- density(values)
  x.mean <- mean(values,na.rm=TRUE)
  x.sd <- sd(values,na.rm=TRUE)
  lines(dvalues,col = col, lwd=2, type = "l")
  rug(values, col = col)
}

plot.density.withmeansd <- function(values,
                               main="",xlab="",ylab="",
                               x.min=NA, x.max=NA, 
                               y.min=NA, y.max=NA, 
                               col="#000000",
                               getlimits=FALSE)
{
  # distribuicao dos valores nesta populacao ficticia
  dvalues <- density(values)
  x.mean <- mean(values,na.rm=TRUE)
  x.sd <- sd(values,na.rm=TRUE)
  if (is.na(x.min)) {x.min <- x.mean-4*x.sd}
  if (is.na(x.max)) {x.max <- x.mean+4*x.sd}
  if (is.na(y.min)) {y.min <- min(dvalues$y,na.rm=TRUE)}
  if (is.na(y.max)) {y.max <- max(dvalues$y,na.rm=TRUE)}
  plot.density.empty(main=main,
                     xlab=xlab,
                     ylab=ylab,
                     xlim=c(x.min,x.max),
                     ylim=c(y.min,y.max*1.3))
  lines(dvalues,col = col, lwd=2, type = "l")
  rug(values, col = col)
  abline(v=x.mean,lty=4,col=col)
  # mean & sds
  plot.barmeansd(values,color=col,y.pos=y.max*1.2)  

  if (getlimits==TRUE)
  {
    return(c(x.min,x.max,y.min,y.max))
  }
}