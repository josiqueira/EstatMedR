suppressMessages(library(readxl))

Dados <- readxl::read_excel("AlcoholExperienceDrivingErrors.xlsx")
Dados$Grupo <- factor(Dados$Grupo,
                      levels=c("Placebo","PoucoAlcool","MuitoAlcool"))
grp <- unique(Dados$Grupo)
ngrp <- length(unique(Dados$Grupo))

ancova.fit <- lm(ErrosDirecao~Grupo+Experiencia, 
                 data=Dados)
print(reg <- summary(ancova.fit))

# interceptos adjustados
adj_intercept <- rep(NA,ngrp)
adj_intercept[1] <- reg$coefficients[1,1]
for (g in 2:ngrp)
{
  adj_intercept[g] <- adj_intercept[1]+reg$coefficients[g,1]
}

# inclinacao comum
adj_slope <- reg$coefficients[ngrp+1,1]

# media comum da covariavel
# (media das medias da experiencia nos grupos)
tmp <- aggregate(Experiencia~Grupo, data=Dados, FUN=mean)
adj_cov <- mean(tmp$Experiencia)

# medias marginais ajustadas
adj_mm <- rep(NA,ngrp)
for (g in 1:ngrp)
{
  adj_mm[g] <- adj_intercept[g] + adj_slope*adj_cov
}

# erros-padrao ajustados
# Xiaofeng Liu (2011) The Effect of a Covariate on Standard Error and Confidence Interval Width,
# Communications in Statistics - Theory and Methods, 40:3, 449-456, DOI: 10.1080/03610920903391337
# versao aproximada
# sd_VD <- aggregate(ErrosDirecao~Grupo, data=Dados, FUN=sd)
# sd_VD <- sd_VD$ErrosDirecao
# n <- sum(!is.na(Dados$ErrosDirecao))
# ep <- sd_VD/sqrt(n-(ngrp+1))
# common_ep <- mean(ep)

# output
cat("\n\n-------------------")
cat("\n")
cat("Intercepts:\n")
cat("\t",as.character(grp[1])," = ",adj_intercept[1],"\n", sep="")
for (g in 2:ngrp)
{
  cat("\t",as.character(grp[g])," = ",adj_intercept[1]," + ",reg$coefficients[g,1]," = ",adj_intercept[g],"\n", sep="")
}
cat("\n")
cat("Common slope = ",adj_slope,"\n", sep="")
cat("\n")
cat("Common Covariate Mean = ",adj_cov,"\n", sep="")
cat("\n")
cat("Adjusted marginal means:\n")
for (g in 1:ngrp)
{
  cat("\t",as.character(grp[g])," = ",adj_intercept[g]," + ",adj_slope," * ",adj_cov," = ",adj_mm[g],"\n", sep="")
}
# cat("\n")
# cat("Adjusted standard errors (approximation):\n")
# for (g in 1:ngrp)
# {
#   cat("\t",as.character(grp[g])," = ",ep[g],"\n", sep="")
# }

# grafico
xreg <- rep(NA,ngrp)
yreg <- rep(NA,ngrp)
xmin <- xmax <- ymin <- ymax <- NA 
for (g in 1:ngrp)
{
  xmin <- min(Dados$Experiencia[Dados$Grupo==as.character(grp[g])],na.rm=TRUE)
  xmax <- max(Dados$Experiencia[Dados$Grupo==as.character(grp[g])],na.rm=TRUE)
  x <- seq(xmin,xmax,length.out=100)
  if(is.na(xmin)) {xmin<-min(x)} else { if(xmin>min(x)){xmin<-min(x)} }
  if(is.na(xmax)) {xmax<-max(x)} else { if(xmax<max(x)){xmax<-max(x)} }
  y <- adj_intercept[g] + adj_slope*x
  if(is.na(ymin)) {ymin<-min(y)} else { if(ymin>min(y)){ymin<-min(y)} }
  if(is.na(ymax)) {ymax<-max(y)} else { if(ymax<max(y)){ymax<-max(y)} }
  xreg[g] <- list(x)
  yreg[g] <- list(y)
}
plot(NA, 
     xlim=c(xmin,xmax), ylim=c(ymin,ymax), 
     main="Adjusted Marginal Means",
     xlab="Experiencia",
     ylab="ErrosDirecao")
abline(v=adj_cov,lty=2)
text(adj_cov,ymin,round(adj_cov,3),pos=2,cex=0.6)
for (g in 1:ngrp)
{
  lines(unlist(xreg[g]),unlist(yreg[g]))
  x <- max(unlist(xreg[g]))
  y <- adj_intercept[g] + adj_slope*x
  text(x,y,as.character(grp[g]),pos=2,cex=0.8)
  y <- adj_intercept[g] + adj_slope*adj_cov
  lines(c(xmin,adj_cov), c(y, y), lty=2)
  text(xmin,y,round(adj_mm[g],3),pos=3,cex=0.6)
}

# # Calculo com funcoes do R
# library(emmeans)
# alfa <- 0.05
# EMM <- emmeans::emmeans(ancova.fit,
#                         pairwise~"Grupo",
#                         adjust="tukey",
#                         level=1-alfa)
# print(EMM$emmeans)
# print(plot(EMM$emmeans,
#            colors="black",
#            main="Adjusted Estimated Marginal Means",
#            xlab="ErrosDirecao controlado por Experiencia",
#            xlim=c(4,12),
#            ylab="Grupo"))
