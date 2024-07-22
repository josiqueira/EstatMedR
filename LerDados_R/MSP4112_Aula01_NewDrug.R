library(readxl)
library(dplyr)
library(Hmisc)
library(sjPlot)
library(DescTools)
library(reshape2)
library(reshape)
library(labelled)
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
Dados.wide <- readxl::read_xlsx("NewDrug.xlsx")
Dados.wide$drug <- factor(Dados.wide$drug)
print.data.frame(Dados.wide)
psych::describeBy(subset(Dados.wide,select=c(-drug)),
                    group=list(Dados.wide$drug),
                    mat=TRUE,
                    digits=2)
str(Dados.wide)
print(labelled::look_for(Dados.wide))

Dados.wide <- cbind(seq(1:nrow(Dados.wide)),Dados.wide)
names(Dados.wide) <- c("ID",names(Dados.wide[,2:ncol(Dados.wide)]))
print(labelled::look_for(Dados.wide))

Dados.long <- Hmisc::reShape(Dados.wide,
                             id=c("ID"),
                             colvar=c("drug"),
                             base=c("resp", "pulse"),
                             reps=3,
                             timevar="time",
                             times=c(1,2,3))
str(Dados.long)
print.data.frame(Dados.long)
rownames((Dados.long))

Dados.lista <- list(Dados.wide, Dados.long)
names(Dados.lista) <- c("NewDrug_wide", "NewDrug_long")
saveRDS(Dados.lista, "NewDrug_wide_long.rds")
Dados_wide_long <- readRDS("NewDrug_wide_long.rds")
str(Dados_wide_long)
print.data.frame(Dados_wide_long$NewDrug_wide)
print.data.frame(Dados_wide_long$NewDrug_long)

# Dados_long.resp <- reshape::melt(Dados.wide,
#                                  id.vars=c("ID", "drug"),
#                                  measure.vars=c("resp1", "resp2", "resp3"),
#                                  variable.name="condicao",
#                                  value.name="resp")
# names(Dados_long.resp) <- c("ID","Treatment","Time","resp")
# Dados_long.pulse <- reshape::melt(Dados.wide,
#                                   id.vars=c("ID", "drug"),
#                                   measure.vars=c("pulse1", "pulse2", "pulse3"),
#                                   variable.name="condicao",
#                                   value.name="pulse")
# Dados.long <- cbind(Dados_long.resp, Dados_long.pulse$value)
# names(Dados.long) <- c("ID","Treatment","Time","resp","pulse")
# 
# # Dados.long$Time <- dplyr::recode(Dados.long$Time,
# #                                  "resp1"="1",
# #                                  "resp2"="2",
# #                                  "resp3"="3")
# # Dados.long
# # str(Dados.long)
# 
# Dados.long$Time <- DescTools::Recode(Dados.long$Time,
#                                      "time1"=c("resp1"),
#                                      "time2"=c("resp2"),
#                                      "time3"=c("resp3"),
#                                      elselevel="")
# Dados.long
# str(Dados.long)

Dados.wide.resp <- reshape2::dcast(Dados.long, 
                                   ID + drug ~ time, 
                                   value.var="resp")
names(Dados.wide.resp) <- c("ID","drug","resp1","resp2","resp3")
print.data.frame(Dados.wide.resp)
Dados.wide.pulse <- reshape2::dcast(Dados.long, 
                                    ID + drug ~ time, 
                                    value.var="pulse")
names(Dados.wide.pulse) <- c("ID","drug","pulse1","pulse2","pulse3")
print.data.frame(Dados.wide.pulse)
Dados.wide <- cbind(Dados.wide.resp, 
                    Dados.wide.pulse[c("pulse1","pulse2","pulse3")])
print.data.frame(Dados.wide)


