source("summarySEwithin2.R")
Dados <- readRDS("Nutricao3rmdesb.rds")
alfa <- 0.05
cat("\n\tFactor (Instructor):\n")
print(psych::describe(Sodium~Instructor, 
                      data=Dados), digits=2)

ic <- summarySEwithin2(Dados, 
                       measurevar="Sodium", 
                       withinvars="Instructor",
                       idvar="Student", 
                       na.rm=TRUE, 
                       conf.interval=1-alfa/length(unique(Dados$Instructor)))
print(ic)
grf <- ggplot2::ggplot(ic, 
                       ggplot2::aes(x=Instructor, 
                                    y=Sodium)) +
  ggplot2::geom_errorbar(width=.1, 
                         ggplot2::aes(ymin=Sodium-ci, 
                                      ymax=Sodium+ci)) +
  ggplot2::geom_point(shape=21, 
                      size=3, 
                      fill="white") +
  ggplot2::ylab("Sodium") +
  ggplot2::ggtitle("3 Instructors: Weight\nWithin-subject CI95% Bonferroni") +
  ggplot2::theme_bw()
print(grf)

cat("\nRepeated Measures one-way ANOVA")
modelo <- lmerTest::lmer(Sodium ~ Instructor + (1|Student), 
                         data=Dados)
cat("\nANOVA")
print(anv <- car::Anova(modelo,
                        test.statistic="F"))

cat("\nRegression")
print(summary(modelo, correl=FALSE))
      
cat("\nEffect size analysis")
eta2 <- effectsize::eta_squared(modelo)
print(eta2, digits = 6)
es <- effectsize::interpret_eta_squared(eta2$Eta2)
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)

cat("\nPost hoc tests")

emm <- emmeans::emmeans(modelo, 
                        pairwise~"Instructor", 
                        adjust="holm",
                        level=1-alfa,
                        lmer.df="satterthwaite",
                        lmerTest.limit=nrow(Dados))
print(emm)
print(plot(emm$emmeans, 
           colors="black"))
print(plot(emm$contrasts, 
           colors="black"))

print(multcomp::cld(object=emm$emmeans,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))
