source("eiras.showdataframe.R")
source("eiras.bartitle.R")
options(warn=-1)
Dados <- readRDS("WHOQOL.rds")
n <- nrow(Dados)

# todas as colunas tratadas como ordinais
Dados[,4:27] <- lapply(Dados[,4:27],as.ordered)

r <- polycor::hetcor(Dados[,4:27], use="pairwise.complete.obs", pd=TRUE)
print(r, digits=2)
r.hetcor <- r$correlations

cat(bartitle("Singular values"))
sv <- svd(r.hetcor)$d
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
cat("\t(analysis may be jeopardized if index > 30)\n",sep="")

cat(bartitle("Exploratory Common Factor Analysis (EFA)"))
cat(bartitle("suggested number of common factors",2))
print(psych::fa.parallel(x=r.hetcor, n.obs=n, plot=FALSE)$p)

cat(bartitle("usefulness of correlation matrix",2))
print(psych::KMO(r.hetcor))

cat(bartitle("EFA",2))

cat(bartitle("tentando como unifatorial",3))
num.factors <- 1
fa.fit <- psych::fa(r=r.hetcor,
                    n.obs=n,
                    nfactors=num.factors)
print(fa.fit, sort=TRUE,digits=2,cut=.3)
psych::fa.diagram(fa.fit, 
                  main="WHOQOL",
                  cut=0.3,
                  digits=1,
                  sort=TRUE)
cat(bartitle("Heuristic for model rejection:",2))
cat("X^2 = ",fa.fit$chi,"\n",sep="")
cat("df = ",fa.fit$dof,"\n",sep="")
cat("X^2/df = ",fa.fit$chi/fa.fit$dof,"\n",sep="")
cat("(model may possibly be rejected when X^2/df > 2)\n",sep="")

cat(bartitle("primeira ordem com 4 fatores comuns (seguindo a WHO)",3))
num.factors <- 4
fa.fit <- psych::fa(r=r.hetcor,
                    n.obs=n,
                    nfactors=num.factors)
print(fa.fit, sort=TRUE,digits=2,cut=.3)
psych::fa.diagram(fa.fit, 
                  main="WHOQOL",
                  cut=0.3,
                  digits=1,
                  sort=TRUE)
cat(bartitle("Heuristic for model rejection:",2))
cat("X^2 = ",fa.fit$chi,"\n",sep="")
cat("df = ",fa.fit$dof,"\n",sep="")
cat("X^2/df = ",fa.fit$chi/fa.fit$dof,"\n",sep="")
cat("(model may possibly be rejected when X^2/df > 2)\n",sep="")

cat(bartitle("tentando como bifatorial com 4 fatores especificos e 1 geral",3))
num.factors <- 4
fabi.fit <- psych::omega(m=r.hetcor,
                         n.obs=n,
                         nfactors=num.factors,
                         cut=0.3)
print(fabi.fit, sort=FALSE, digits=2, cut=0.3)
print(psych::omega.diagram(fabi.fit,main="WHOQOL",cut=0.3,gcut=0.3,sort=TRUE))

cat(bartitle("Heuristic for model rejection:",2))
cat("X^2 = ",fabi.fit$stats$chi,"\n",sep="")
cat("df = ",fabi.fit$stats$dof,"\n",sep="")
cat("X^2/df = ",fabi.fit$stats$chi/fabi.fit$stats$dof,"\n",sep="")
cat("(model may possibly be rejected when X^2/df > 2)\n",sep="")


options(warn=0)

