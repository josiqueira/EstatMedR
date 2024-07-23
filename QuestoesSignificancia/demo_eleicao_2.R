# demo_eleicao_2.R

nmin <- 100
nmax <- 1500
n <- seq(from=nmin,to=nmax,by=100)
p <- c(0.5,0.2)
me5 <- 100*1.96*sqrt(p[1]*(1-p[1])/n)
me2 <- 100*1.96*sqrt(p[2]*(1-p[2])/n)
df <- data.frame(n,me5,me2)
colnames(df) <- c("n",
                  "%margem_erro(p=.5)",
                  "%margem_erro(p=.2)")
print(round(df,2))
