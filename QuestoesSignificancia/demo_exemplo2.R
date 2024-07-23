# EXEMPLO
# Proportions in Two Independent Groups p.91
# Borenstein, M; Rothstein, H & Cohen, J (2008) 
# IBM SPSS Sample Power 3. IL: SPSS/IBM.
if(!exists("pH0")){pH0 <- 0.5}
if(!exists("pH1")){pH1 <- 0.7}
if(!exists("alfa")){alfa <- 0.05}
if(!exists("n_total")){n_total <- 100}

ra <- 0.5 # razao de alocacao para grupos balanceados
# desvio padrao das proporcoes
sdpH0 <- sqrt(pH0*(1-pH0))
sdpH1 <- sqrt(pH1*(1-pH1))
precisao <- pwr2ppl::md_prec(m1=pH1,m2=pH0, 
                       s1=sdpH1, 
                       s2=sdpH0,
                       nlow=n_total, nhigh=n_total, 
                       propn1=ra, ci=1-alfa, by=1)
precisao$IC.center <- round((precisao$LL+precisao$UL)/2,3)
print(precisao)