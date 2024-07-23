# demo_eleicao_1.R
# n da margem de erro de 3%: se p = .5, p*(1-p) = 1/4
# margem de erro = quantil95% * erro.padrao
# erro.padrao de proporcao = sqrt(p*(1-p)/n)
me3 <- 0.03
p <- 0.5
curve(1.96*sqrt((p*(1-p))/x),500,1500,
      xlab="n",ylab="margem de erro")
abline(h=me3,lty=2)
n <- 1/(4*(me3/1.96)^2)
cat("n = ",round(n,0))
