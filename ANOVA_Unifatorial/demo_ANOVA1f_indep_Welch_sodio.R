TH <- readRDS("Nutricao3.rds")
cat("\nANOVA unifatorial independente de Welch\n\n")
cat("\nAnálise de significância estatística: testes omnibus e posthoc\n")
print(res <- jmv::anovaOneW(formula=Sodium~Instructor, 
                            data=TH,
                            desc=TRUE, 
                            descPlot=FALSE, 
                            phMethod='gamesHowell',
                            phMeanDif=TRUE, 
                            phTest=TRUE, 
                            phFlag=TRUE))

cat("\nStatistical analysis: omnibus test")
fit <- oneway.test(Sodium ~ Instructor, 
                   data=TH,
                   na.action=na.omit)
print(fit)

cat("\nFisher's One-way ANOVA with White's heteroscedasticity correction")
cat("\nStatistical analysis: Post hoc tests")
print(data.frame(rstatix::games_howell_test(Sodium~Instructor, 
                                            data=TH,
                                            detailed=TRUE)))
cat("\nAnálise de significância prática: tamanho de efeito\n")
eta2 <- effectsize::eta_squared(fit)
print(eta2, digits = 6)
es <- effectsize::interpret_eta_squared(eta2$Eta2)
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)

