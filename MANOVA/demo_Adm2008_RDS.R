Dados <- data.frame(readxl::read_excel("Adm2008.xlsx"))
Dados$Nome <- factor(Dados$Nome)
Dados$Sexo <- factor(Dados$Sexo)
Dados$Estatura <- Dados$Estatura*100
saveRDS(Dados, "Adm2008.rds")
