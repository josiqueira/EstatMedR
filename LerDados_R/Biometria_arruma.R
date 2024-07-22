pausa <- FALSE

dados <- readxl::read_excel("Biometria_FMUSP.xlsx")

cat("\nNomes das variáveis:\n")
print(names(dados))

cat("\nEstrutura:\n")
print(str(dados))

# ID não deve ser numérico
dados$ID <- as.character(dados$ID)

# As seguintes variáveis devem ser fatores
cat("\nConverte em fatores:\n")
fatores <- c("Ano", 
             "Turma",
             "Sexo",
             "Mao",
             "TipoSang",
             "ABO",
             "AtivFisica",
             "Sedentarismo",
             "Rh" )
for (f.aux in fatores)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.character((unlist(dados[,c.aux])))
  cat("\n\t",f.aux,": ")
  print(unique(valores))
  # a principio uso os fatores em ordem alfabetica
  dados[,c.aux] <- factor(valores, levels = sort(unique(valores)))
  # confere
  print(str(dados[,c.aux]))
  if (pausa) {readline(prompt="Press [enter] to continue")}
}

# Tem problemas: string "NA" (em vez de NA)
cat("\nCorrige e converte em fatores:\n")
removeNA <- c("Mao","TipoSang","ABO")
for (f.aux in removeNA)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.character((unlist(dados[,c.aux])))
  valores[valores=="NA"] <- NA
  cat("\n\t",f.aux,": ")
  print(unique(valores))
  # a principio uso os fatores em ordem alfabetica
  dados[,c.aux] <- factor(valores, levels = sort(unique(valores)))
  # confere
  print(str(dados[,c.aux]))
  if (pausa) {readline(prompt="Press [enter] to continue")}
}

# O fator AtivFisica tem ordem inconveniente
print(levels(dados$AtivFisica))
# esta ordem eh melhor
ordem <- c(
            "sempre_inativo",
            "atualmente_inativo",
            "baixa_intensidade" ,
            "media_intensidade",
            "alta_intensidade"
          )
# os nomes estao tambem muito longos
nomes <- c("SI","AI","B","M","A")
dados$AtivFisica <- factor(dados$AtivFisica, 
                           labels = nomes,
                           levels = ordem)
print(levels(dados$AtivFisica))

# Os seguintes campos deveriam ser numericos
cat("\nVerifica porque nao eh numerico:\n")
# verificando a causa de não serem
numericos <- c("MCT",
               "Estatura")
for (f.aux in numericos)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.character((unlist(dados[,c.aux])))
  cat("\n\t",f.aux,":\n")
  print(unique(valores))
  if (pausa) {readline(prompt="Press [enter] to continue")}
}
# Tem problemas: string "NA", "na", "Na", etc
cat("\nCorrige e converte em numerico:\n")
for (f.aux in numericos)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.character((unlist(dados[,c.aux])))
  valores[toupper(valores)=="NA"] <- NA
  valores <- as.numeric(valores)
  dados[,c.aux] <- as.numeric(valores)
  cat("\n\t",f.aux,":\n")
  print(unique(valores))
  if (pausa) {readline(prompt="Press [enter] to continue")}
}

cat("\nConfere a estrutura:\n")
print(str(dados))
cat("\nSumario geral:\n")
for (c.aux in 1:ncol(dados))
{
  cat("\n",names(dados)[c.aux],":\n")
  print(summary(dados[,c.aux]))
}

# Graficos
cat("\nGraficos e tabelas iniciais:\n")
for (f.aux in fatores)
{
  c.aux <- which(names(dados)==f.aux)
  tabela <- table(dados[,c.aux])
  print(tabela)
  barplot(tabela, main=f.aux)
  if (pausa) {readline(prompt="Press [enter] to continue")}
}
for (f.aux in numericos)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.numeric(unlist(dados[,c.aux]))
  boxplot(valores)
  if (pausa) {readline(prompt="Press [enter] to continue")}
}

# Note que existe um indivíduo com mais que 600 kg
cat("\nIndividuo com mais que 600 kg\n")
dt_tmp <- dados[dados$MCT>200,]
dt_tmp <- na.omit(dt_tmp)
print(dt_tmp)
# podemos optar por remover o dado incorreto
dados$MCT[dados$MCT>200] <- NA

# Note que existe um indivíduo com menos que 140 cm
cat("\nIndividuo com massa e estatura iguais a 120\n")
dt_tmp <- dados[dados$Estatura<140,]
dt_tmp <- na.omit(dt_tmp)
print(dt_tmp)
# podemos optar por remover o dado incorreto
dados$Estatura[dados$Estatura<140] <- NA

cat("\nOptamos por remover os dados incorretos, restando:")
cat("\n- MCT:\n")
print(summary(dados$MCT))
cat("\n- Estatura:\n")
print(summary(dados$Estatura))

# Tudo corrigido, convem salvar
saveRDS(dados,"Biometria_analise.rds")

