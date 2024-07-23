# eiras.shade.R
shade.polygon <- function(d, mean, sd,
                          col="#000000", sds=2)
{
  # shade mean +- sd*s
  for (s in 1:sds)
  {
    coord <- c(NA,NA)
    i <- 1
    for (signal in c(-1,1))
    {
      x <- mean+sd*s*signal
      x.tmp <- abs(d$x-x)
      coord[i] <- which(x.tmp==min(x.tmp))
      i<-i+1
    }
    x.tmp <- d$x[coord[1]:coord[2]]
    x.tmp <- c(x.tmp[1],x.tmp,x.tmp[length(x.tmp)])
    y.tmp <- d$y[coord[1]:coord[2]]
    y.tmp <- c(0,y.tmp,0)
    polygon(x.tmp,y.tmp,border=NA,col=paste0(col,"30"))
  }
}