# n da margem de erro de 3%: se p = .5, p*(1-p) = 1/4
# margem de erro = quantil95% * erro.padrao
# erro.padrao de proporcao = sqrt(p*(1-p)/n)
me3 <- .03
curve(1.96*sqrt(1/(4*x)),500,1500,
      xlab="n",ylab="margem de erro")
abline(h=me3,lty=2)
n <- 1/(4*(me3/1.96)^2)
cat("n = ",n)

nmin <- 200
nmax <- 10000
p <- .5
n <- seq(nmin,nmax,100)
me <- 100*1.96*sqrt(p*(1-p)/n)
df <- data.frame(n,me)
curve(1.96*sqrt(p*(1-p)/x),nmin,nmax,lty=1,
      xlab="n",ylab="margem de erro")
abline(h=me3,lty=2)
p <- .2
me <- 100*1.96*sqrt(p*(1-p)/n)
df <- data.frame(df,me)
colnames(df) <- c("n","%margem_erro(p=.5)","%margem_erro(p=.2)")
print(round(df,2))
p <- .2
curve(1.96*sqrt(p*(1-p)/x),nmin,nmax,lty=3,add=TRUE,
      xlab="n",ylab="margem de erro")

suppressMessages(library(samplingbook, warn.conflicts = FALSE))

P_vector <- c(0.5, 0.4, 0.3, 0.2, 0.1)
e_vector <- c(0.2, 0.1, 0.05, 0.04, 0.03, 0.02, 0.01)
results <- matrix(NA, ncol=length(P_vector), nrow=length(e_vector))
for (i in 1:length(e_vector)){
   for (j in 1:length(P_vector)){
      x <- try(samplingbook::sample.size.prop(e=e_vector[i], 
                                              P=P_vector[j], 
                                              N=Inf),
               silent=TRUE)
      if (class(x)=='try-error') {results[i,j] <- NA}
      else {results[i,j] <- x$n}
   }
}
dimnames(results) <- list(paste('me=',e_vector, sep=''), 
                          paste('p=',P_vector, sep=''))
print(results)




