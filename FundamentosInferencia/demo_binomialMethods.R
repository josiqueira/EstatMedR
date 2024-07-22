library (DescTools)
source("eiras.friendlycolor.R")

# sides <- c("left", "two.sided", "right")
if (!exists("sides"))
{
  sides <- "two.sided"
}
if (is.na(sides))
{
  sides <- "two.sided"
}
if (!exists("method"))
{
  method <- "agresti-coull"
}
if (is.na(method[1]))
{
  method <- "agresti-coull"
}


IC95 <- NA
try(
IC95 <- DescTools::BinomCI(x=sucessos, n=n, 
                           sides=sides,
                           method=method)
)
if (is.na(IC95[1]))
{
  cat("O valor de x = ",sucessos," precisa estar entre 1 e ",n-1,"\n",sep="")
} else
{
  IC95 <- round(IC95,4)
  print(IC95)
  xmin <- min(as.numeric(IC95[,2]),na.rm=TRUE)
  xmax <- max(as.numeric(IC95[,3]),na.rm=TRUE)
  lmax <- 0.2
  plot(NA,
       xlab="Proporcao", ylab="",
       xlim=c(0,1.5),ylim=c(0,nrow(IC95)),
       axes=FALSE)
  axis(1, at=seq(0,1,0.1))
  for (r in 1:nrow(IC95))
  {
    y <- length(method)-r+1
    lines(c(IC95[r,2],IC95[r,3]),rep(y,2))
    points(c(IC95[r,2],IC95[r,3]),rep(y,2),pch="I")
    points(IC95[r,1],y,pch=21,col="black",bg="black")
    text(xmax, y, rownames(IC95)[r], pos=4)
    abline(v=proppop, lty=2)
  }
}
