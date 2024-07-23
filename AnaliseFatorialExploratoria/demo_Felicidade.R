source("eiras.showdataframe.R")
source("eiras.bartitle.R")

cat(bartitle("Data"))
Dados <- readRDS("Felicidade.rds")
showdataframe(Dados, head=4, tail=3)
n <- nrow(Dados) # number of observations

# todas as colunas tratadas como ordinais
Dados[,] <- lapply(Dados[,],as.ordered)

cat(bartitle("Correlation matrix"))
r <- polycor::hetcor(Dados, use="pairwise.complete.obs", pd=TRUE)
print(r, digits=1)
r.hetcor <- r$correlations

cat(bartitle("assumindo variaveis como ordinais",2))
ro <- qgraph::cor_auto(Dados, detectOrdinal=TRUE, 
                       forcePD=TRUE, missing="pairwise")
print (
  GGally::ggcorr(data=NULL,
                 name="Felicidade",
                 cor_matrix=ro,
                 geom="tile",
                 min_size=0,
                 max_size=10, 
                 nbreaks=6,
                 digits=2,
                 label=FALSE,
                 label_round=2,
                 label_size=4)
)

cat(bartitle("Network (pacote qgraph)"))
n <- nrow(na.omit(Dados))
cat(bartitle("graph=\"glasso\""),2)
qgraph::qgraph(ro, 
               graph="glasso", 
               sampleSize=n,
               layout="spring",
               shape="rectangle",
               vsize=5,
               label.cex=2,
               labels=colnames(ro),
               label.prop=0,
               theme="gray",
               title="Escala de Felicidade", 
               details=FALSE)
cat(bartitle("graph=\"cor\""),2)
qgraph::qgraph(ro, 
               graph="cor", 
               sampleSize=n,
               minimum="sig",
               bonf=TRUE,
               layout="spring", 
               shape="rectangle",
               vsize=5,
               label.cex=2,
               labels=colnames(ro),
               label.prop=0,
               theme="gray",
               title="Escala de Felicidade",
               details=FALSE)

cat(bartitle("Frequencias (pacote likert)"))
cat(bartitle("Verifica numero de niveis observados em cada item",2))
print(sapply(Dados,function(x){length(levels(x))}))
# usa a funcao preparatoria
lkt <- likert::likert(as.data.frame(Dados))
cat(bartitle("Frequencia das respostas",2))
print(lkt)
print(plot(lkt, type="heat", low.color="lightgray", high.color="darkgray"))
print(summary(lkt))
print(plot(lkt, low.color="lightgray", high.color="black"))

cat(bartitle("Dendrograma (pacote pvclust)"))
cat("\n(so funciona assumido itens intervalares)\n")
Dados_num <- Dados
Dados_num[,] <- lapply(Dados_num[,],as.numeric)
res.pv <- pvclust::pvclust(Dados_num, 
                           method.dist="abscor",
                           method.hclust="average", 
                           nboot = 5e2)
plot(res.pv)

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
num.factors <- 2
fa.fit <- psych::fa(r=r.hetcor,
                    n.obs=n,
                    nfactors=num.factors)
print(fa.fit, sort=TRUE,digits=2,cut=.3)
psych::fa.diagram(fa.fit, 
                  main="Felicidade",
                  cut=0,
                  digits=2,
                  sort=TRUE)
cat(bartitle("Heuristic for model rejection):",2))
cat("X^2 = ",fa.fit$chi,"\n",sep="")
cat("df = ",fa.fit$dof,"\n",sep="")
cat("X^2/df = ",fa.fit$chi/fa.fit$dof,"\n",sep="")
cat("(model may possibly be relected when X^2/df > 2)\n",sep="")

options(warn=0)

