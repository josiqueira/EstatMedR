# jitter = c(NA, num)
# respectively: NA or given value

add.jitter <- function (values, jitter=NA)
{
  if (is.na(jitter))
  {
    s <- sort(values)
    min <- max(s)
    for (i in 2:length(s))
    {
      dif <- abs(s[i]-s[i-1])
      if (dif > 0 & min > dif) {min<-dif}
    }
    # jitter <- abs(median(values, na.rm=TRUE) / 50)
    jitter <- min / 4
  }
  if (jitter != 0)
  {
    values <- values + runif(length(values),-jitter,jitter)
  }
  
  return(values)
}

