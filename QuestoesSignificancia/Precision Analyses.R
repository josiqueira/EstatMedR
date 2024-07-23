library(pwr2ppl)
library(samplingbook)
alfa <- .05
# Precisao = amplitude esperada do IC95 do efeito
# Precision Analysis for Mean/ Proportion Differences
pH0 <- .6
pH1 <- .8
npoder90 <- 220
print(out <- pwr2ppl::propind(p1=pH1,p2=pH0,
                              nlow=npoder90,nhigh=npoder90,by=1,nratio=.5))
ntot <- out$n1+out$n2
sdpH0 <- sqrt(pH0*(1-pH0))
sdpH1 <- sqrt(pH1*(1-pH1))
print(pwr2ppl::md_prec(m1=pH1,m2=pH0, 
                 s1=sdpH1, 
                 s2=sdpH0,
                 nlow=ntot, nhigh=ntot, propn1=.5, ci=1-alfa, by=10))

pwr2ppl::md_prec(m1=0,m2=0, s1=5, s2=5,
                 nlow=100, nhigh =40000, 
                 propn1=.5, ci=.95, by=1000)

# Precision Analyses for Standardized Mean Differences
pwr2ppl::d_prec(d=.4,nlow=100, nhigh=2000, propn1=.5, ci=.95, by=100)

# Precision Analyses for Correlations
pwr2ppl::r_prec(r=.3, nlow=80, nhigh=400, by=20, ci=.95)

## 1) examples with different precisions
# precision 1% for election forecast of SPD in 2005
samplingbook::sample.size.prop(e=0.01, P=0.5, N=Inf)
data(election)
samplingbook::sample.size.prop(e=0.01, P=mean(election$SPD_02), N=Inf)
# precision 5% for questionnaire
samplingbook::sample.size.prop(e=0.05, P=0.5, N=300)
samplingbook::sample.size.prop(e=0.05, P=0.5, N=Inf)
# precision 10%
samplingbook::sample.size.prop(e=0.1, P=0.5, N=300)
samplingbook::sample.size.prop(e=0.1, P=0.5, N=1000)

## 2) tables in the book
# table 2.2
P_vector <- c(0.1, 0.2, 0.3, 0.4, 0.5)
N_vector <- c(10, 100, 1000, 10000, Inf)
results <- matrix(NA, ncol=length(P_vector), nrow=length(N_vector))
for (i in 1:length(P_vector)){
  for (j in 1:length(N_vector)){
    x <- try(samplingbook::sample.size.prop(e=0.1, 
                                            P=P_vector[i], 
                                            N=N_vector[j]))
    if (class(x)=='try-error') {results[i,j] <- NA}
    else {results[i,j] <- x$n}
  }
}
dimnames(results) <- list(paste('p=',P_vector, sep=''), 
                          paste('n=',N_vector, sep=''))
results
# table 2.3
P_vector <- c(0.5, 0.4, 0.3, 0.2, 0.1)
e_vector <- c(0.1, 0.05, 0.04, 0.03, 0.02, 0.01)
results <- matrix(NA, ncol=length(P_vector), nrow=length(e_vector))
for (i in 1:length(e_vector)){
  for (j in 1:length(P_vector)){
    x <- try(samplingbook::sample.size.prop(e=e_vector[i], 
                                            P=P_vector[j], 
                                            N=Inf))
    if (class(x)=='try-error') {results[i,j] <- NA}
    else {results[i,j] <- x$n}
  }
}
dimnames(results) <- list(paste('me=',e_vector, sep=''), 
                          paste('p=',P_vector, sep=''))
results

