library (DescTools)
source("eiras.friendlycolor.R")

# sides <- c("left", "two.sided", "right")
if (!exists("sides"))
{
  sides <- "two.sided"
}
if (is.na(sides[1]))
{
  sides <- "two.sided"
}
if (!exists("method"))
{
  method <- "ac"
}
if (is.na(method[1]))
{
  method <- "ac"
}

library (DescTools)

sucessos.F <- 48
n.F <- 80 
sucessos.M <- 56
n.M <- 70

xmax <- -1
lmax <- 0.2
plot(NA,
     xlab="Diferença de Proporções", ylab="",
     xlim=c(-1.0,1.5),ylim=c(0,length(method)+1),
     axes=FALSE)
axis(1, at=seq(-1,1,0.1), cex.axis=0.8)

r <- 0
for (m in method)
{
  xci <- NA
  try(
    xci <- DescTools::BinomDiffCI(sucessos.F, n.F, 
                                  sucessos.M, n.M,
                                  method = m,
                                  sides = sides)
  )
  if (!is.na(xci[1]))
  {
    y <- length(method)-r
    if (xmax < xci[3]) {xmax <- xci[3]}
    cat("method ",m,":\n",sep="")
    prmatrix(xci, rowlab="", quote=FALSE)
    lines(c(xci[2],xci[3]),rep(y,2))
    points(c(xci[2],xci[3]),rep(y,2),pch="I")
    points(xci[1],y,pch=21,col="black",bg="black")
    text(xmax, y, m, pos=4)
    r <- r+1    
  }
}
abline(v=0, lty=2)

