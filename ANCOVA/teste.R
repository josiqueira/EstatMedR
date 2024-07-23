d.range <- seq (0.01,1,length.out = 20)
r.range <- seq (0.1,0.9,length.out = 20)

colnames <- c("d", "r", "n")
drn <- data.frame(matrix(ncol=length(colnames),nrow=0))
for (d in d.range)
{
  for (r in r.range)
  {
    alfa <- .05
    poder <- .9
    n.porcondicao <- 2*((qnorm(1-alfa/2)+qnorm(poder))*sqrt(1-r^2)/d)^2
    n.total <- 2*n.porcondicao
    tmp <- data.frame(d,r,n.total)
    names(tmp) <- colnames
    drn <- rbind(drn,tmp) 
  }
}
nmin <- min(drn$n)
nmax <- max(drn$n)
for (r in r.range)
{
}