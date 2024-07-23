library(psych)
library(es.dif)
source("eiras.bartitle.R")
source("eiras.numeric.summary.R")

m <- seq(160,185,2.5)
f <- seq(150,170,2.5)

nm <- length(m)
nf <- length(f)
estatura <- c(m,f)
grupo <- factor(c(rep("Masculino",nm),
           rep("Feminino",nf)))
dados <- data.frame(grupo,estatura)
o.dados <- dados
# sink("Verificacao de tamanho de efeito.txt")

for (step in 1:2)
{
  if(step==1)
  {
    cat(bartitle("Amostra original"))
  } else
  {
    cat(bartitle(paste("Amostra * ",step,sep="")))
    # multiplica a amostra
    dados <- o.dados
    for (i in 2:step)
    {
      dados <- rbind(dados,o.dados)
    }
  }
  
  m <- dados$estatura[dados$grupo=="Masculino"]
  cat("Estatura dos homens:\n",m,"\n")
  desc.m <- numeric.summary(m)
  print(desc.m)
  sdm <- desc.m[7]

  f <- dados$estatura[dados$grupo=="Feminino"]
  cat("Estatura das mulheres:\n",f,"\n")
  desc.f <- numeric.summary(f)
  print(desc.f)
  sdf <- desc.f[7]
  
  nm <- as.numeric(desc.m[8])
  nf <- as.numeric(desc.f[8])
  
  grupo.num <- dados$grupo
  grupo.num <- as.character(grupo.num)
  grupo.num[grupo.num=="Masculino"] <- "1"
  grupo.num[grupo.num=="Feminino"] <- "0"
  grupo.num <-as.numeric(grupo.num)
  var.sex <- var(grupo.num)
  estatura <- dados$estatura
  var.estatura <- var(estatura)
  cat("\n#########",var.sex,var.estatura,"\n")

  print(fit0 <- t.test(estatura~grupo,data=dados,var.equal=TRUE))
  print(fit <- t.test(estatura~grupo,data=dados))
  if(step==1)
  {
    o.fit0 <- fit0
    o.fit <- fit
    o.nm <- nm
    o.nf <- nf
    o.sdm <- sdm
    o.sdf <- sdf
    o.var.sex <- var.sex
    o.var.estatura <- var.estatura
  }  
  print(lm <- summary(lm(estatura~grupo,data=dados)))
  if(step==1)
  {
    o.lm <- lm
  }  
  
  df <- as.numeric(fit$parameter)
  t <- fit$statistic
  F <- as.numeric(fit$statistic^2)
  eta2 <- F/(F+df)
  
  # de de Cohen
  # calculo manual
  vm <- var(m)
  vf <- var(f)
  Sc = sqrt(((nm-1)*vm+(nf-1)*vf)/(nm+nf-2))
  d <- abs(mean(m)-mean(f))/Sc
  # calculos por funcao
  out <- es.dif::es.d(m,f)
  d1 <- as.numeric(out[1,2])
  psy <- psych::cohen.d(dados$estatura,dados$grupo) 
  d2 <- as.numeric(psy$cohen.d[2])
  g <- as.numeric(psy$hedges.g)[2]
  if(step==1)
  {
    o.out <- out
    o.eta2 <- eta2
    o.psy <- psy
    o.F <- F
    o.d <- d
    o.d1 <- d1
    o.d2 <- d2
    o.g <- g
  }  
  
  R2aj <- (F-1)/((F-1)+df+1)
  omega2 <- (F-1)/((F-1)+df+2)
  if(step==1)
  {
    o.R2aj <- R2aj
    o.omega2 <- omega2
  }  
  res <- oneway.test(formula = estatura~grupo, data=dados)
  print(res)
  # print(res$anova)
  if(step==1)
  {
    o.res <- res
  }  
}

cat("\n\nComparativo:\n")
estim <- c(
  "n(m)",
  "n(f)",
  "sd(m)",
  "sd(f)",
  "var(sexo)",
  "var(estatura)",
  "p(t_student)",
  "df(t_student)","t_student",
  "p(t_welch)",
  "df(t_welch)","t_welch",
  "IC95(t)_LL","IC95(t)_UL",
  "eta^2",
  "R^2(lm)",
  "slope(lm)",
  "dCohen (formula)",
  "dCohen (es.dif::es.d)",
  "dCohen (psych::cohen.d)",
  "gHedges (psych::cohen.d)",
  "Radj^2 (formula)",
  "Radj^2(lm)",
  "omega^2 (formula)",
  "df(denF)","F",
  "p(F)"
)
o.v <- c(
  o.nm,
  o.nf,
  o.sdm,
  o.sdf,
  o.var.sex,
  o.var.estatura,
  o.fit0$p.value,
  round(as.numeric(o.fit0$parameter),3),as.numeric(o.fit0$statistic),
  o.fit$p.value,
  round(as.numeric(o.fit$parameter),3),as.numeric(o.fit$statistic),
  as.numeric(o.fit$conf.int[1]), as.numeric(o.fit$conf.int[2]),
  o.eta2,
  o.lm$r.squared,
  o.lm$coefficients[2,1],
  o.d,
  o.d1,
  o.d2,
  o.g,
  o.R2aj,
  o.lm$adj.r.squared,
  o.omega2,
  round(as.numeric(o.res$parameter[2]),3),
  as.numeric(o.res$statistic),
  as.numeric(o.res$p.value)
)
v <- c(
  nm,
  nf,
  sdm,
  sdf,
  var.sex,
  var.estatura,
  fit0$p.value,
  round(as.numeric(fit0$parameter),3),as.numeric(fit0$statistic),
  fit$p.value,
  round(as.numeric(fit$parameter),3),as.numeric(fit$statistic),
  as.numeric(fit$conf.int[1]), as.numeric(fit$conf.int[2]),
  eta2,
  lm$r.squared,
  lm$coefficients[2,1],  
  d,
  d1,
  d2,
  g,
  R2aj,
  lm$adj.r.squared,
  omega2,
  round(as.numeric(res$parameter[2]),3),
  as.numeric(res$statistic),
  as.numeric(res$p.value)
)
df_compara <- data.frame(estim,o.v,v,v/o.v)
names(df_compara) <- c("","amostra","amostra x 2","razao")
df_compara$amostra <- round(df_compara$amostra,5)
df_compara$`amostra x 2` <- round(df_compara$`amostra x 2`,5)
df_compara$razao <- round(df_compara$razao,3)
print(df_compara)
