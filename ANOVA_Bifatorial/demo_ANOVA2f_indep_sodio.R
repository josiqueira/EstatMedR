# demo_ANOVA2f_premissas.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))

suppressMessages(library(lawstat))
suppressMessages(library(MuMIn))
suppressMessages(library(heplots))


suppressMessages(library(emmeans))
suppressMessages(library(multcomp))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- read_excel("Nutricao2fatores.xlsx")
Dados$Instructor <- factor(Dados$Instructor)
Dados$Student <- factor(Dados$Student)
Dados$Supplement <- factor(Dados$Supplement)

cat(bartitle("Data"))
showdataframe(Dados,4,3)

cat(bartitle("ANOVA bifatorial independente de Fisher\ncom ajuste para heterocedasticidade de White"))

alfa <- 0.05
cat(bartitle("Analise de significancia estatistica: teste omnibus"))
modelo <- lm(Sodium~Instructor*Supplement, data=Dados)
print(reg <- summary(modelo))
print(anv <- car::Anova(modelo, type=3, white.adjust=TRUE))

# grafico
fobs <- reg$fstatistic[1]
dfn <- reg$fstatistic[2]
dfd <- reg$fstatistic[3]
fc <- qf(1-alfa, dfn, dfd)
p <- 1-pf(fobs, dfn, dfd)
if (p < 1e-4)
{
  p <- sprintf("%.2e",p)
} else
{
  p <- sprintf("%.4f",p)
}
f <- seq(0,1.4*max(fc,fobs),length.out=300)
densf <- df(f, dfn, dfd)
plot(f, densf, 
     main="Omnibus test",
     xlab="F", ylab="Density", 
     lwd=2, type="l")
abline(v=fc, lty=3)
abline(v=fobs, lty=4)
legend("topright",
       c("H0: there is not model",
         paste("Fc(",dfn,",",dfd,") = ",round(fc,3),sep=""),
         paste("Fobs = ",round(fobs,3),"\n",
               "p = ",p,sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       cex=0.8, 
       box.lwd=0, bg="transparent")


cat(bartitle("Analise de significancia pratica: tamanho de efeito"))
cat("\n- Omnibus:\n")
eta2 <- as.numeric(MuMIn::r.squaredLR(modelo))
Eta2classification(eta2)
cat("\n- Partial(s):\n")
eta2p <- heplots::etasq(modelo)
eta2p$classification <- NA
for (r in 1:nrow(eta2p))
{
  if(!is.na(eta2p$`Partial eta^2`[r]))
  {
    eta2p$classification[r] <- Eta2classification(eta2p$`Partial eta^2`[r],show=FALSE)  
  }
}
eta2p <- eta2p[!is.na(eta2p$`Partial eta^2`),]
prmatrix(eta2p,quote=FALSE)


# cat(bartitle("Post-hoc tests"))
# 
# print(EMM <- emmeans::emmeans(modelo, "Instructor"))
# print(grf <- plot(EMM, colors = "black",
#                   main="Estimated Marginal Means",
#                   xlab="Sodium",
#                   ylab="Instructor"))
# 
# modelo2 <- lm(Sodium~InstructorShort, data=Dados)
# mc.tukey <- multcomp::glht(modelo2, linfct = multcomp::mcp(InstructorShort = "Tukey"))
# print(mcs.tukey <- summary(mc.tukey, test=adjusted("bonferroni")))
# multcomp::cld(mcs.tukey, level=alfa, decreasing=TRUE)
# t.ic <- mcs.tukey$test$qfunction(0.95)
# df_plot <- data.frame(
#   names (mcs.tukey$test$coefficient),
#   as.vector(mcs.tukey$test$coefficients),
#   as.vector(mcs.tukey$test$coefficients)-t.ic*as.vector(mcs.tukey$test$sigma),
#   as.vector(mcs.tukey$test$coefficients)+t.ic*as.vector(mcs.tukey$test$sigma),
#   as.vector(mcs.tukey$test$pvalues)
# )
# eiras_plotIC(df_plot,
#              main="95% family-wise confidence level",
#              xlab="Difference",
#              usecolor="n"
# )
# legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")
# 
# mc.dunnett <- multcomp::glht(modelo2, linfct = multcomp::mcp(InstructorShort = "Dunnett"))
# mcs.dunnett <- summary(mc.dunnett, test=adjusted("bonferroni"))
# print(mcs.dunnett)
# t.ic <- mcs.dunnett$test$qfunction(0.95)
# df_plot <- data.frame(
#   names (mcs.dunnett$test$coefficient),
#   as.vector(mcs.dunnett$test$coefficients),
#   as.vector(mcs.dunnett$test$coefficients)-t.ic*as.vector(mcs.dunnett$test$sigma),
#   as.vector(mcs.dunnett$test$coefficients)+t.ic*as.vector(mcs.dunnett$test$sigma),
#   as.vector(mcs.dunnett$test$pvalues)
# )
# eiras_plotIC(df_plot,
#              main="95% family-wise confidence level",
#              xlab="Difference",
#              usecolor="n"
# )
# legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")
# 
# # enable warnings
# options(warn=0)
# # 
