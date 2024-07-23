source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05
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
     main="Médias Marginais Estimadas",
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
