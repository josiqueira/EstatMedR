Dados <- data.frame(readxl::read_excel("Nutricao.xlsx"))
Dados$Instructor <- factor(Dados$Instructor)
Dados$Student <- factor(Dados$Student)
fit <- oneway.test(data=Dados,
                   Sodium~Instructor)
print(fit)
eta2 <- effectsize::eta_squared(fit)
print(eta2, digits = 6)
es <- effectsize::interpret_eta_squared(eta2$Eta2)
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)


