# demo_estatmct.R

source("eiras.bartitle.R")
source("eiras.friendlycolor.R")
source("eiras.ConfidenceBand.R")
alfa <- 0.05
B <- 1e3
colH <- friendlycolor(8)
colM <- friendlycolor(21)

df_adm <- readxl::read_excel("Adm2008.xlsx")
df_admF <- df_adm[df_adm$Genero=="Feminino",]
df_admM <- df_adm[df_adm$Genero=="Masculino",]
nF <- nrow(df_admF)
nM <- nrow(df_admM)

# Elipse de predicao 95%
matriz <- as.matrix(df_adm[, 3:4])
n <- nrow(matriz)
car::dataEllipse(matriz[,1], matriz[,2], 
                 groups=factor(df_adm$Genero), 
                 group.labels=c("Feminino", "Masculino"),
                 levels=c(1-alfa,.999),
                 robust=TRUE,
                 main=paste("Elipses de predicao de 95% e 99.9%\n",
                            "n = ",n," (Fem=",nF,", Masc=",nM,")",sep=""),
                 xlab="Estatura (m)", 
                 ylab="MCT (kg)",
                 xlim=c(1.3, 2.1),
                 ylim=c(25, 120),
                 lwd=0.8, lty=2
                 )

# Correlacao, teste
correlF <- cor.test(df_admF$Estatura,df_admF$MCT)
correlM <- cor.test(df_admM$Estatura,df_admM$MCT)
crl <- psych::r.test(n=nF, n2=nM, correlF$estimate, correlM$estimate)

# RLS
# regressao
rlsF <- estimatr::lm_robust(df_admF$MCT ~ df_admF$Estatura)
xF <- seq(min(df_admF$Estatura,na.rm=TRUE),
          max(df_admF$Estatura,na.rm=TRUE),length.out=10)
yF <- rlsF$coefficients[1] + rlsF$coefficients[2]*xF
rlsM <- estimatr::lm_robust(df_admM$MCT ~ df_admM$Estatura)
xM <- seq(min(df_admM$Estatura,na.rm=TRUE),
          max(df_admM$Estatura,na.rm=TRUE),length.out=10)
yM <- rlsM$coefficients[1] + rlsM$coefficients[2]*xM
plot(NA, 
     main="Regressão Linear Simples",
     xlab="Estatura (m)", 
     ylab="MCT (kg)",
     xlim=c(1.3, 2.1),
     ylim=c(25, 120))
text(min(xF),min(yF),col=colH,pos=2,"Feminino")
points(df_admF$Estatura,df_admF$MCT,col=paste(colH,"60",sep=""),pch=1)
lines(xF,yF,col=colH,lwd=2)
text(max(xM),max(yM),col=colM,pos=3,"Masculino")
points(df_admM$Estatura,df_admM$MCT,col=paste(colM,"60",sep=""),pch=2)
lines(xM,yM,col=colM,lwd=2)
# band
lst <- ConfidenceBand(x=df_admF$Estatura,y=df_admF$MCT, alpha=alfa, B=B)
bandF <- lst[[2]]
lst <- ConfidenceBand(x=df_admM$Estatura,y=df_admM$MCT, alpha=alfa, B=B)
bandM <- lst[[2]]
lines(bandF$X, bandF$LB, col=colH, lty=2, lwd=1.3)
lines(bandF$X, bandF$UB, col=colH, lty=2, lwd=1.3)
lines(bandM$X, bandM$LB, col=colM, lty=2, lwd=1.3)
lines(bandM$X, bandM$UB, col=colM, lty=2, lwd=1.3)
# RLS, test (p.value da interacao, paralelismo)
df_adm$GeneroNum <- NA
df_adm$GeneroNum[df_adm$Genero=="Feminino"] <- 0
df_adm$GeneroNum[df_adm$Genero=="Masculino"] <- 1
rls_paralelismo <- estimatr::lm_robust(df_adm$MCT ~ 
                          df_adm$Estatura+df_adm$GeneroNum+
                          df_adm$Estatura:df_adm$GeneroNum)
# RLS, test (p.value do fator, coincidencia)
rls_coincidencia <- estimatr::lm_robust(df_adm$MCT ~ 
                             df_adm$Estatura+df_adm$GeneroNum)

# padronizacao
df_adm$Estatura_z <- NA
df_adm$MCT_z <- NA
m <- mean(df_admF$Estatura,na.rm=TRUE)
s <- sd(df_admF$Estatura,na.rm=TRUE)
df_admF$Estatura_z <- (df_admF$Estatura-m)/s
df_adm$Estatura_z[df_adm$Genero=="Feminino"] <- (df_adm$Estatura[df_adm$Genero=="Feminino"]-m)/s
m <- mean(df_admF$MCT,na.rm=TRUE)
s <- sd(df_admF$MCT,na.rm=TRUE)
df_admF$MCT_z <- (df_admF$MCT-m)/s
df_adm$MCT_z[df_adm$Genero=="Feminino"] <- (df_adm$MCT[df_adm$Genero=="Feminino"]-m)/s
m <- mean(df_admM$Estatura,na.rm=TRUE)
s <- sd(df_admM$Estatura,na.rm=TRUE)
df_admM$Estatura_z <- (df_admM$Estatura-m)/s
df_adm$Estatura_z[df_adm$Genero=="Masculino"] <- (df_adm$Estatura[df_adm$Genero=="Masculino"]-m)/s
m <- mean(df_admM$MCT,na.rm=TRUE)
s <- sd(df_admM$MCT,na.rm=TRUE)
df_admM$MCT_z <- (df_admM$MCT-m)/s
df_adm$MCT_z[df_adm$Genero=="Masculino"] <- (df_adm$MCT[df_adm$Genero=="Masculino"]-m)/s
car::dataEllipse(df_adm$Estatura_z, df_adm$MCT_z, 
                 groups=factor(df_adm$Genero), 
                 group.labels=c("Feminino", "Masculino"),
                 col=c(colM,colH),
                 levels=c(1-alfa),
                 robust=TRUE,
                 main=paste("Elipses predição 95% e RLS padronizadas\n",
                            "n = ",n," (Fem=",nF,", Masc=",nM,")",sep=""),
                 xlab="Estatura (z)", 
                 ylab="MCT (z)",
                 xlim=c(-4,4),
                 ylim=c(-4,4))
# RLS padronizada
rlsF <- estimatr::lm_robust(df_admF$MCT_z ~ df_admF$Estatura_z)
xF <- seq(min(df_admF$Estatura_z,na.rm=TRUE),max(df_admF$Estatura_z,na.rm=TRUE),length.out=10)
yF <- rlsF$coefficients[1] + rlsF$coefficients[2]*xF
lines(xF,yF,col=colH,lwd=2)
rlsM <- estimatr::lm_robust(df_admM$MCT_z ~ df_admM$Estatura_z)
xM <- seq(min(df_admM$Estatura_z,na.rm=TRUE),max(df_admM$Estatura_z,na.rm=TRUE),length.out=10)
yM <- rlsM$coefficients[1] + rlsM$coefficients[2]*xM
lines(xM,yM,col=colM,lwd=2)

# saida numericaS
cat(bartitle("Estatística descritiva"))
cat(bartitle("- sexo feminino"))
print(summary(df_adm[df_adm$Genero=="Feminino",3:4]))
cat(bartitle("- sexo masculino"))
print(summary(df_adm[df_adm$Genero=="Masculino",3:4]))

cat(bartitle("RLS"))
cat(bartitle("- sexo feminino"))
print(summary(rlsF))
cat(paste("\nEquation: media[MCT(kg)] = ",round(rlsF$coefficients[1],4)," + ",
          round(rlsF$coefficients[2],4)," * Estatura(m)\n",sep=""))
cat(bartitle("- sexo masculino"))
print(summary(rlsM))
cat(paste("\nEquation: media[MCT(kg)] = ",round(rlsM$coefficients[1],4)," + ",
          round(rlsM$coefficients[2],4)," * Estatura(m)\n",sep=""))

cat(bartitle("Correlação"))
cat(bartitle("- sexo feminino"))
print(correlF)
cat(bartitle("- sexo masculino"))
print(correlM)

cat(bartitle("Paralelismo das RLS"))
# print(rls_paralelismo)
cat("H0: retas populacionais com inclinações iguais\n",sep="")
cat("\tp = ",rls_paralelismo$p.value[4],sep="")

cat(bartitle("Coincidência das RLS"))
# print(rls_coincidencia)
cat("H0: retas populacionais com interceptos iguais\n",sep="")
cat("\tp = ",rls_coincidencia$p.value[3],sep="")

cat(bartitle("Correlação"))
# print(rls_coincidencia)
cat("H0: correlações de Pearson populacionais iguais\n",sep="")
cat("\tp = ",crl$p,sep="")

