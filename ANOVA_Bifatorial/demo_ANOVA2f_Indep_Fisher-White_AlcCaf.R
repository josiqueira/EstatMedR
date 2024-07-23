alfa <- 0.05

Dados <- readRDS("CafeinaAlcool_entre.rds")

cat("\nFisher-White's Bifatorial independent ANOVA")

# ANOVA bifatorial independente de Fisher
# teste _omnibus_: `lm`, `car::Anova(white.adjust="hc2", ...))`
cat("\nStatistical analysis: omnibus test")
# teste omnibus: `lm`, `car::Anova`
modelo <- lm(NumErros ~ Alcool*Cafeina,  
             data=Dados)
cat("\nANOVA")
print(anv <- car::Anova(modelo, white.adjust="hc2"))
regr <- estimatr::lm_robust(NumErros ~ Alcool*Cafeina, 
                            se_type="HC2",
                            data=Dados)
# regr <- lmtest::coeftest(vcov = sandwich::vcovHC(modelo, 
#                                                  type = "HC2"), 
#                          modelo)
# print(regr)
print(reg <- summary(regr))
cat("R^2 = eta^2 omnibus =", reg$r.squared)
print(effectsize::interpret_r2(reg$r.squared))
print(effectsize::eta_squared(modelo),6)

# grafico do teste F
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
     main="Fisher-White Two-way ANOVA: Omnibus test",
     xlab="F", ylab="Density",
     lwd=2, type="l")
abline(v=fc, lty=3)
abline(v=fobs, lty=4)
legend("topright",
       c("H0: there is not model",
         paste("F(",dfn,",",dfd,",",1-alfa,") = ",round(fc,3),sep=""),
         paste("F(",dfn,",",dfd,") = ",round(fobs,3),"\n",
               "p = ",p,sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       cex=0.8,
       bty="n")

