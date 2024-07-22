dados <- readxl::read_excel("Biometria_analise.xlsx")

gravar <- FALSE

# Os seguintes campos sao fatores
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
  dados[,c.aux] <- factor(valores, levels = sort(unique(valores)))
}

# Os seguintes campos sao numericos
numericos <- c("MCT",
               "Estatura")
for (f.aux in numericos)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.numeric((unlist(dados[,c.aux])))
}

# O fator AtivFisica ordem inconveniente, corrigir
nomes <- c("SI","AI","B","M","A")
dados$AtivFisica <- factor(dados$AtivFisica, 
                           labels = nomes,
                           levels = nomes)

# Descritiva
if (gravar)
{
  sink("DescreveBiometria.txt")
  pdf("DescreveBiometria.pdf",paper="a4")
}

cat("\nNomes dos campos:\n")
print(names(dados))
cat("\nEstrutura:\n")
print(str(dados))

for (f.aux in fatores)
{
  c.aux <- which(names(dados)==f.aux)
  tabela <- table(dados[,c.aux])
  cat("\n--------------------\n")
  print(tabela)
  barplot(tabela, main=f.aux)
}
for (f.aux in numericos)
{
  c.aux <- which(names(dados)==f.aux)
  valores <- as.numeric(unlist(dados[,c.aux]))
  cat("\n--------------------\n")
  cat(f.aux,"\n")
  print(summary(valores))
  boxplot(valores)
}

if (gravar)
{
  dev.off()
  sink()
}

