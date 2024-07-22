# para sortear os mesmos numeros do exemplo
set.seed(123)
estatura <- rnorm(10, 1.70, 0.10)
print(estatura)

estatura <- round(estatura*100,0)
print(estatura)

estatura.cat <- cut(estatura, 
                    breaks=c(0,160,180,+Inf), 
                    labels=c("baixa","intermediaria","alta"))
print(str(estatura.cat))

estatura.cat <- ordered(estatura.cat)
print(str(estatura.cat))

somatometria <- data.frame(estatura,estatura.cat)
print(somatometria)
somatometria$estcivil <- c("casado","solteiro","solteiro",
                           "solteiro","divorciado", "solteiro",
                           "casado", "casado", "viúvo", 
                           "solteiro")
print(somatometria)

# para sortear os mesmos numeros do exemplo
set.seed(570) 
somatometria$mct <- round(rnorm(10, 75, 20),0)
# massa em kg, estatura em metro
somatometria$imc <- round(somatometria$mct/((somatometria$estatura/100)^2),1)
print(somatometria)

classe <- c("Subpeso",
            "Normal",
            "Sobrepeso", "Obesidade I", "Obesidade II", "Obesidade III")
pc <- c(0, 18.5, 25, 30, 35, 40, +Inf)
somatometria$imc_classe <- cut(somatometria$imc, pc, classe)
print(somatometria)

classe <- c("Anormal",
            "Normal",
            "Anormal")
pc <- c(0, 18.5, 25, +Inf)
somatometria$imc_dicot <- cut(somatometria$imc, pc, classe)
print(somatometria)

somatometria$glicemia <- c(90,87,98,97,127,90,93,80,93,89)
print(somatometria)
