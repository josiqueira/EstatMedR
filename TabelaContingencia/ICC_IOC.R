# O International Olympic Commitee (IOC) treina juizes para competicoes
# de ginastica.
# SPSS Base 10: Applications Guide, 1999.
library(psych)
library(haven)
library(irr)
Dados <- haven::read_sav("judges.sav")
Dados <- Dados[,1:7] 
# Good absolute agreement between the raters, ICC=0.723, was observed while using 
# the two-way random effect models and single rater with a p-value of 3.24e-11.
print(irr::icc(Dados, 
               model="twoway",
               type="agreement", 
               unit="single"))

cat("\n\nMatriz de correlacao:\n")
print(mc <- round(cor(Dados),2))
for (rc in 1:7)
{
  mc[rc,rc] <- NA
}
cat("\ncorrelacao media = ",mean(mc,na.rm=TRUE))
