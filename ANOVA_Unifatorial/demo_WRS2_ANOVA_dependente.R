Dados <- readRDS("Nutricao3rm.rds")
alfa <- 0.05
# random seed
s <- round(runif(1,1,1e5),0)
cat("\nseed = ",s,"\n",sep="")

cat("\n\"WRS\" bootstrapped repeated measure one-Way ANOVA")
set.seed(s)
print(WRS2::rmanovab(Dados$Sodium, 
                     Dados$Instructor, 
                     tr=0, 
                     Dados$Student, 
                     nboot=1e4))

cat("\n\"WRS\" post hoc")
set.seed(s)
print(WRS2::pairdepb(Dados$Sodium, 
                     Dados$Instructor, tr=0, 
                     Dados$Student, 
                     nboot=1e4))
