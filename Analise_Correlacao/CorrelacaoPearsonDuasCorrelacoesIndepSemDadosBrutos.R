nF <- 38
corrF <- 0.637148
nM <- 51
corrM <- 0.646062 

out <- psych::r.test(n=nF, n2=nM, corrF, corrM)
cat("z = ",out$z,", p = ",out$p,"\n",sep="")

