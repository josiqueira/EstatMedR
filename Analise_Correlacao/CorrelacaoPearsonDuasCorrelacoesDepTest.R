out <- psych::r.test(n=85, r12=0.53, r13=0.41, r23=0.59); cat("\n")
cat("t = ",out$t,", p = ",out$p,"\n",sep="") 
