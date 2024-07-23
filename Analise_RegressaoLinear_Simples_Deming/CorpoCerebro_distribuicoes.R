# CorpoCerebro_RLS_problema.R
# (versao nao padronizada, para regressão)

source("eiras.friendlycolor.R")
source("eiras.density_and_normal.R")

col_Corpo <- friendlycolor(14) # verde
col_Cerebro <- friendlycolor(39) # cinza

CorpoCerebro <- readxl::read_excel("CorpoCerebroVonBonin.xlsx")

# density plots
density_and_normal(CorpoCerebro$Corpo, 
                   col=col_Corpo,
                   xlab="Corpo (g)", ylab="densidade")
density_and_normal(CorpoCerebro$Cerebro, 
                   col=col_Cerebro,
                   xlab="Cérebro (g)", ylab="densidade")

