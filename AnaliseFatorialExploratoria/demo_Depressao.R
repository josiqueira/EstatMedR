source("eiras.bartitle.R")

cat(bartitle("Data: correlation matrix"))
n <- 300 # number of observations
Matriz <- data.frame(readxl::read_excel("Depressao.xlsx"))
Matriz_tmp <- data.matrix(Matriz)
rownames(Matriz_tmp) <- as.character(unlist(Matriz[1:(ncol(Matriz)-1),1]))
Matriz_tmp <- Matriz_tmp[,-1]
Matriz <- Matriz_tmp
print(Matriz)

cat(bartitle("Singular values"))
sv <- svd(Matriz)$d
dt <- data.frame(1:length(sv),format(as.numeric(sv), digits=2, nsmall=2))
names(dt) <- c("component","value")
print(dt)
plot(sv, 
     xlab="Components", ylab="Singular values", 
     type="o", pch=16, axes=FALSE)
axis(1, at=1:length(sv))
axis(2)
abline(h=1,col="darkgray",lty=2)

cat(bartitle("Model heuristic"))
sv1maior <- sv[1]
sv2maior <- sv[2]
H <- NA
if(sv2maior>0) {H <- sv1maior/sv2maior}
cat("FirstSV/SecondSV = H = ",round(sv1maior,2),"/",round(sv2maior,2)," ~ ",round(H,2),"\n",sep="")
cat("\t- if < 2: first order\n")
cat("\t- if > 4: unifactor\n")
cat("\t- otherwise: bifactor\n")
if (H <= 2) {txt <- "\tpossible first order model, sv1/sv2 <= 2"}
if (H > 2 & H <= 4) {txt <- "\tpossible bifactor model, 2 < sv1/sv2 <= 4"}
if (H > 4) {txt <- "\tpossible unifactor model, sv1/sv2 > 4"}
cat(txt)

cat(bartitle("Condition index"))
sv.maior <- sv[1]
sv.menor <- sv[length(sv)]
IC <- NA
if(sv.menor>0) {IC <- sqrt(sv.maior/sv.menor)}
cat("(MaxSV/MinSV)^0.5 = CI = (",round(sv.maior,2),"/",round(sv.menor,2),")^0.5 ~ ",round(IC,1),"\n",sep="")
cat("(analysis may be jeopardized if index > 30)\n",sep="")

cat(bartitle("Exploratory Common Factor Analysis (EFA)"))
cat(bartitle("suggested number of common factors",2))
print(psych::fa.parallel(x=Matriz, n.obs=n,
                         plot=FALSE)$p)
# print(psych::VSS(x=Matriz,
#                  n.obs=n,
#                  plot=FALSE))

cat(bartitle("usefulness of correlation matrix",2))
print(psych::KMO(Matriz))

cat(bartitle("EFA",2))
num.factors <- 1
fa.fit <- psych::fa(r=Matriz,
                    n.obs=n,
                    nfactors=num.factors)
print(fa.fit, sort=TRUE,digits=2,cut=.3)
psych::fa.diagram(fa.fit, 
                        main="Depression Scale",
                        cut=0,
                        digits=2,
                        sort=TRUE)
cat(bartitle("Heuristic for model rejection:",2))
cat("X^2 = ",fa.fit$chi,"\n",sep="")
cat("df = ",fa.fit$dof,"\n",sep="")
cat("X^2/df = ",fa.fit$chi/fa.fit$dof,"\n",sep="")
cat("(model may possibly be rejected when X^2/df >  2)\n",sep="")
