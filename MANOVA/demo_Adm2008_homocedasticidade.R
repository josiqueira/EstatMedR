source("eiras.bartitle.R")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

options(warn=-1) # disable warnings

Dados <- readRDS("Adm2008.rds")

print(res <- heplots::boxM(Dados[,c("Estatura","MCT")], Dados$Sexo))
plot(res, main="Estudante de Administração\nEstatura e MCT")

options(warn=0) # enable warnings
