# CorpoCerebro_RLS_log.R

alfa <- 0.05
B <- 0
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
# transformacao log
CorpoCerebro$Cerebro_log <- log(CorpoCerebro$Cerebro)
CorpoCerebro$Corpo_log <- log(CorpoCerebro$Corpo)

lst <- correg(CorpoCerebro$Corpo_log, CorpoCerebro$Cerebro_log,
              alpha=alfa, B=B, 
              method="lm_robust",
              xlab="Ln[Corpo (g)]", ylab="Ln[Cérebro (g)]", 
              bg="transparent", col=col_CxC, pch=pch_CxC)
