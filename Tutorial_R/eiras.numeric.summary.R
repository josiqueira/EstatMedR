# eiras.numeric.summary.R

numeric.summary <- function(values)
{
  # summary returns a matrix, changed to values.frame
  dfrm <- as.data.frame(as.matrix(summary(values)))  
  # add sd
  sd <- sd(as.numeric(values),na.rm=TRUE)
  # count n, NA
  n <- sum(!is.na(values))
  na <- length(values)-n
  
  m <- matrix(data=c(dfrm[1,1],dfrm[2,1],dfrm[3,1],dfrm[5,1],dfrm[6,1],dfrm[4,1],sd,n,na),nrow=1,ncol=9)
  colnames(m) <- c("Min.","1st Qu.","Median","3rd Qu.", "Max.","Mean","St. Dev.","n","NA's")
  rownames(m) <- ""

  return (m)
}
