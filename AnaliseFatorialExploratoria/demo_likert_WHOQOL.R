Dados <- readRDS("WHOQOL.rds")

cat("\nTransformando respostas em dados ordinais\n")
Dados[,4:27] <- lapply(Dados[,4:27],as.ordered)
cat("\n\t- verificando com class:\n")
print(sapply(Dados[,4:27],class))
cat("\n\t- verificando com str:\n")
print(str(Dados[,4:27]))

# esta funcao precisa que todos os itens tenham o mesmo numero de niveis
# descritiva com o pacote likert
cat("\nUsando o pacote likert\n")
cat("\nVerifica quantos niveis cada item tem\n")
print(sapply(Dados[,4:27],function(x){length(levels(x))}))

# usa a funcao preparatoria
lkt <- likert::likert(Dados[,4:27])

cat("\nFrequencia das respostas\n")
print(lkt, digits=1)
print(plot(lkt, type="heat", low.color="lightgray", high.color="darkgray"))

cat("\nFrequencia das respostas\n")
print(summary(lkt), digits=2)
print(plot(lkt, low.color="lightgray", high.color="black"))
