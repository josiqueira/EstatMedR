# demo_r_de_Spearman_boot_monotonico.R

# disable warnings
options(warn=-1)

alfa <- 0.05
B <- 0

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.correg.R")

# cores
col <- friendlycolor(31) # preto
bg <- friendlycolor(24) # amarelo
pch <- 21 # circulo

# valores
set.seed(27)
x <- 1:20
y <- c(runif(1,1,2))
for (i in 2:length(x))
{
  y <- c(y,runif(1,y[i-1]+0.01,y[i-1]+i^2))
}
cat("x:",x,"\n")
cat("y:",round(y,3),"\n")
correg(x, y,
       alpha=alfa, B=B, 
       method="spearman",
       xlab="x", ylab="y", 
       col=col, bg=bg, pch=pch)
lines(x,y,type="b")

# enable warnings
options(warn=0)
