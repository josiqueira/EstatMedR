# CorpoCerebro_RLS_bootstrapping.R

alfa <- 0.05
B <- 1e3

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.correg.R")

col_Corpo <- friendlycolor(14) # verde
pch_Corpo <- 22
col_Cerebro <- friendlycolor(39) # cinza
pch_Cerebro <- 23
col_CxC <- friendlycolor(39) # cinza
pch_CxC <- 21

CorpoCerebro <- readxl::read_excel("CorpoCerebroVonBonin.xlsx")

correg (CorpoCerebro$Corpo, CorpoCerebro$Cerebro,
                     alpha=alfa, B=B, 
                     method="lm_robust",
                     xlab="Corpo (g)", ylab="Cérebro (g)", 
                     ylim=c(0,9000),
                     bg="transparent", col=col_CxC, pch=pch_CxC)
