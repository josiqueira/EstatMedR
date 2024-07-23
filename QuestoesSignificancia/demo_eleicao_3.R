library(samplingbook)
# demo_eleicao3.R

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
