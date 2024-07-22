source("eiras.tern.graph.R")

eiras.tern.graph()

# 10 random points
a <- runif(10)
b <- runif(10)
c <- runif(10)
for (p in 1:10)
{
  cat("\npoint: ",p,"\n")
  cat("original: ",a[p],b[p],c[p],"\n")
  cat("\tsum: ",sum(a[p],b[p],c[p]),"\n")
  coord <- eiras.tern.coord(c(a[p],b[p],c[p]))
  cat("\tcorrected sum: ",sum(coord[1,3:5]),"\n")
  cat("\tplot (",sum(coord[1,1]),", ",sum(coord[1,2]),")\n")
  points(coord[1,1],coord[1,2],pch=21,bg="blue",cex=0.8)
}