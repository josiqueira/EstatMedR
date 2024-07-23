# https://www.statmethods.net/stats/power.html 
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
# https://advstats.psychstat.org/book/power/index.php
# https://livebook.manning.com/book/r-in-action/chapter-10/141
# http://psychstat.org/correlation
# https://stats.idre.ucla.edu/r/dae/power-analysis-for-two-group-independent-sample-t-test/
# https://rcompanion.org/rcompanion/b_02.html
# http://www.biostathandbook.com/power.html
# https://publicifsv.sund.ku.dk/~tag/Teaching/share/R-tutorials/Basic-statistics/Power.html


library(pwr)
poder <- .9
alfa <- 0.05
nmin <- 10
nmax <- 100
pH0 <- 1/2
pH1 <- 2/3
power.prop.test(p1 = pH0, p2 = pH1, sig.level = alfa, power = poder,
                alternative = "one.sided")
out <- pwr::pwr.p.test(h = pwr::ES.h(p1 = pH1, p2 = pH0),
           sig.level = alfa,
           power = poder,
           alternative = "greater")
print(out)
plot(out)

samplesizes <- seq(from=nmin,to=nmax,by=10)
out <- pwr::pwr.p.test(h = pwr::ES.h(p1 = pH0, p2 = pH1),
                       sig.level = alfa,
                       n=samplesizes,
                       alternative = "less")
power.samplesizes <- out$power

plot(samplesizes,
     power.samplesizes,
     xlim=c(0,nmax),
     main="Teste bilateral de proporcao para uma condicao\nalfa=.05",
     xlab="n",
     ylab="Power",
     ylim=c(0,1),
     type="b",
     col="black",
     lwd=1,axes=FALSE)
axis(1,at=seq(0,nmax,10))
axis(2,at=seq(0,1,.1))
xrange <- range(samplesizes)
yrange <- round(range(power.samplesizes))
abline(v=0, h=seq(0,yrange[2],.1), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],10), lty=2,
       col="grey89")
abline(h=poder,lty=2)

pwr::cohen.ES(test = "p", size = "small")
pwr::pwr.2p2n.test(h = 0.2, n1 = 543, n2 = 675, sig.level = alfa)

pwr::cohen.ES(test = "r", size = "medium")
pwr::pwr.r.test(r="medium", power=poder)
pwr::pwr.p.test(h = c(0.2,0.5,0.8),
           n = 20,
           sig.level = alfa)
n <- seq(nmin,nmax,10)
p.out <- pwr.p.test(h = 0.5,
                    n = n,
                    sig.level = alfa)
data.frame(n, power = sprintf("%.2f%%", p.out$power * 100))
addSegs <- function(p1, p2){
  tp1 <- ES.h(p1, 0); tp2 <- ES.h(p2, 0)
  segments(p1,0,p1,tp1, col="blue"); segments(p2,0,p2,tp2,col="blue")
  segments(0, tp1, p1, tp1, col="red"); segments(0, tp2, p2, tp2, col="red")
}

curve(expr = ES.h(p1 = x, p2 = 0), xlim = c(0,1),
      xlab = "proportion", ylab = "transformed proportion")
addSegs(p1 = 0.50, p2 = 0.55) # 50% vs 55%
addSegs(p1 = 0.05, p2 = 0.10) # 5% vs 10%

# Plot sample size curves for detecting correlations of
# various sizes.

# range of correlations
r <- seq(.1,.5,.01)
nr <- length(r)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = alfa, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

# Sample sizes for detecting significant effects in a one-way ANOVA
# R in Action
library(pwr)
es <- seq(.1, .5, .01)
nes <- length(es)

samsize <- NULL
for (i in 1:nes){
  result <- pwr.anova.test(k=5, f=es[i], sig.level=alfa, power=poder)
  samsize[i] <- ceiling(result$n)
}

plot(samsize,es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
     main="One Way ANOVA with Power=.90 and Alpha=.05")

# install.packages("piface", repos="http://R-Forge.R-project.org")
# library(piface)
# piface()

differences <- seq(from=0.1,to=2,by=0.1)
samplesize.sd04 <- sapply(differences,
                          function(d){power.t.test(power=poder,
                                                   delta=d,
                                                   sd=0.4,
                                                   type="two.sample")$n})
samplesize.sd03 <- sapply(differences,
                          function(d){power.t.test(power=poder,
                                                   delta=d,
                                                   sd=0.3,
                                                   type="two.sample")$n})
plot(differences,
     samplesize.sd04,
     xlim=c(0,2),
     xlab="Expected difference between groups",
     ylab="Required sample size",
     ylim=c(0,350),
     type="b",
     col="darkblue",
     lwd=5,axes=FALSE)
lines(differences, samplesize.sd03,col="turquoise",lwd=5,type="b")
axis(1,at=c(0,0.2,0.5,1,1.5,2))
axis(2,at=c(350,100,50,10,0))
legend(x="topright",lwd=5,bty="n",legend=c("SD=0.4","SD=0.3"),col=c("darkblue","turquoise"))


