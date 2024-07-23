# CorpoCerebro_RLS_problema.R
# (versao nao padronizada, para regressão)

alfa <- 0.05
B <- 0

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.correg.R")
source("eiras.density_and_normal.R")
col_CxC <- friendlycolor(39) # cinza
pch_CxC <- 21

CorpoCerebro <- readxl::read_excel("CorpoCerebroVonBonin.xlsx")

correg(CorpoCerebro$Corpo, CorpoCerebro$Cerebro,
       alpha=alfa, B=B, 
       method="lm_robust",
       xlab="Corpo (g)", ylab="Cérebro (g)", 
       bg="transparent", col=col_CxC, pch=pch_CxC,
       suppress.text=TRUE)
# marca os problemas
points(200000,500,cex=5,lwd=4)
lines(c(-2000000,3000000),c(1500,11000),lwd=4,lty=5)
lines(c(300000,70000000),c(-50,2000),lwd=4,lty=5)
