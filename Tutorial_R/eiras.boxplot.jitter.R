source("eiras.numeric.summary.R")
source("eiras.jitter.R")

boxplot.jitter <- function (data, main=NA, ylab=NA, ylim=NA)
{
  data <- data[!is.na(data)]
  s <- numeric.summary(data)
  
  if (is.na(ylim[1]))
  {
    ymin <-  as.numeric(s[1])*0.9
    ymax <-  as.numeric(s[5])*1.1
  } else
  {
    ymin <-  ylim[1]
    ymax <-  ylim[2]
  }     
  
  median <- as.numeric(s[3])
  lbox <- as.numeric(s[2])
  ubox <- as.numeric(s[4])
  
  outlier <- (ubox-lbox)*1.5
  outlierext <- (ubox-lbox)*3
  
  uoutlier <- max(data[data<=ubox+outlier],na.rm=TRUE)
  outlier.u <- data[data>ubox+outlier & data<=ubox+outlierext & !is.na(data)]
  outlier.uext <- data[data>ubox+outlierext & !is.na(data)]
  loutlier <- min(data[data>=lbox-outlier],na.rm=TRUE)
  outlier.l <- data[data<lbox-outlier & data>=lbox-outlierext & !is.na(data)]
  outlier.lext <- data[data<lbox-outlierext & !is.na(data)]
  
  plot(NA, 
       main=main,
       xlab="", ylab=ylab, 
       xlim=c(0,20), ylim=c(ymin,ymax),
       axes = FALSE)
  axis(side=2)
  # box
  rect(xleft=10-4, ybottom=lbox, xright=10+4, ytop=ubox)
  # median
  lines(c(10-4,10+4),c(median,median),lwd=2) 
  # upper limit
  lines(c(10-1.5,10+1.5),c(uoutlier,uoutlier))
  lines(c(10,10),c(uoutlier,ubox))
  # lower limit
  lines(c(10-1.5,10+1.5),c(loutlier,loutlier))
  lines(c(10,10),c(loutlier,lbox))
  # outliers
  if(length(outlier.u)>0){points(jitter(rep(10,length(outlier.u)),jitter=1),jitter(outlier.u,jitter=NA,l.lim=uoutlier),pch=21,col="#00000088")}
  if(length(outlier.uext)>0){points(jitter(rep(10,length(outlier.uext)),jitter=1),jitter(outlier.uext,jitter=NA,l.lim=ubox+outlierext),pch=8,col="#00000088")}
  if(length(outlier.l)>0){points(jitter(rep(10,length(outlier.l)),jitter=1),jitter(outlier.l,jitter=NA,u.lim=loutlier),pch=21,col="#00000088")}
  if(length(outlier.lext)>0){points(jitter(rep(10,length(outlier.lext)),jitter=1),jitter(outlier.lext,jitter=NA,u.lim=lbox-outlierext),pch=8,col="#00000088")}
}
  