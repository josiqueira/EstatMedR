# eiras.pseudomediana.R

pseudomediana <- function (x,y=NULL,conf.level=NA,B=NA)
{
  # comb <- combn(x,2)
  # m <- rep(0,ncol(comb))
  # for (i in 1:ncol(comb))
  # {
  #   m[i] <- mean(comb[,i])
  # }
  # return(median(m))

  names <- c("pm","pairs")
  if(!is.null(y))
  {
    names[1] <- "diff(pm)"
  }  
  if(is.numeric(conf.level))
  {
    if(conf.level>=0 & conf.level<=1)
    {
      names <- c(names,c("lower",
                         "upper",
                         "conf.level",
                         "B"))
      if(is.na(B)) {B <- 1e3}
    } else {conf.level <- NA; B <- 1}
  } else {conf.level <- NA; B <- 1}
  res <- matrix(nrow=1,ncol=length(names))
  colnames(res) <- names
  rownames(res) <- ""
  
  b.median <- c()
  b.pairs <- c()
  for (b in 1:B)
  {
    # bootstrapping
    v1 <- x
    v2 <- y
    if(b>1)
    {
      v1 <- sample(x,size=length(x),replace=TRUE)
      if(!is.null(v2))
      {
        v2 <- sample(y,size=length(y),replace=TRUE)
      }
    }
    
    m <- c()
    if(is.null(v2))
    {
      for (i1 in 1:length(v1))
      {
        for (i2 in i1:length(v1))
        {
          m <- c(m,mean(c(v1[i1],v1[i2])))
        }
      }
    } else
    {
      for (i1 in 1:length(v1))
      {
        for (i2 in 1:length(v2))
        {
          m <- c(m,c(v1[i1]-v2[i2]))
        }
      }
    }
    m <- sort(m)
    b.median <- c(b.median, median(m))
    b.pairs <- c(b.pairs, length(m))
  } # for b
  
  res[1] <- median(b.median)
  res[2] <- mean(b.pairs)
  if(is.numeric(conf.level))
  {
    alpha <- 1-conf.level
    q <- quantile(b.median,probs=c(alpha/2,1-alpha/2))
    res[3] <- q[1]
    res[4] <- q[2]
    res[5] <- conf.level
    res[6] <- B
  }
  retorno <- list()
  retorno[["statistics"]] <- res
  if(is.numeric(conf.level))
  {
    retorno[["density"]] <- density(b.median)
  }
  return(retorno)
}
