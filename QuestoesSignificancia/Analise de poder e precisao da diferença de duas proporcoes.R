library(pwr)
library(pwr2ppl)
# EXEMPLO
# Proportions in Two Independent Groups p.91
# Borenstein, M; Rothstein, H & Cohen, J (2008) 
# IBM SPSS Sample Power 3. IL: SPSS/IBM.
pH0 <- .6
pH1 <- .8
poder <- .9
alfa <- 0.05
ntotpoder90 <- 216
ra <- .5 # razao de alocacao na primeira condicao independente
sink("Analise de poder e precisao da diferença de duas proporcoes.txt")
print(pwr2ppl::propind(p1=pH1,p2=pH0,
                              nlow=ntotpoder90,nhigh=ntotpoder90,
                              by=1,nratio=ra,tails=2))
print(pwr::pwr.2p2n.test(h = pwr::ES.h(p1 = pH1, p2 = pH0), 
                         n1 = ntotpoder90*ra, n2 = ntotpoder90*ra, 
                         sig.level = alfa,
                         alternative = "two.sided"))
sdpH0 <- sqrt(pH0*(1-pH0))
sdpH1 <- sqrt(pH1*(1-pH1))
print(pwr2ppl::md_prec(m1=pH1,m2=pH0, 
                       s1=sdpH1, 
                       s2=sdpH0,
                       nlow=ntotpoder90, nhigh=ntotpoder90, 
                       propn1=ra, ci=1-alfa, by=1))
sink()


