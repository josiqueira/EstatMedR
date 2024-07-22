# eiras.matrix2dataframe.R
# reconstitui dataframe a partir de tabela de contingencia (matrix)

matrix2dataframe <- function(TC, judges)
{
  rnames <- rownames(TC)
  if(is.null(rnames)){rnames<-c("A","B")}
  cnames <- colnames(TC)
  if(is.null(cnames)){cnames<-c("A","B")}
  nrows <- sum(TC,na.rm=TRUE)
  dimensao <- length(rnames)
  rcnames <- c(rnames,cnames)
  
  dtf <- data.frame(matrix(data=0,nrow=nrows,ncol=2))
  names(dtf) <- judges
  r.idx <- 1
  for (r in 1:dimensao)
  {
    rname <- rnames[r]
    for (c in 1:dimensao)
    {
      cname <- cnames[c]
      if(TC[r,c]>0)
      {
        for (n in 1:TC[r,c])
        {
          dtf[r.idx,1] <- rname
          dtf[r.idx,2] <- cname
          r.idx <- r.idx+1
        }
      }
    }
  }
  dtf[,1] <- as.factor(dtf[,1])
  dtf[,2] <- as.factor(dtf[,2])
  return (dtf)
}

