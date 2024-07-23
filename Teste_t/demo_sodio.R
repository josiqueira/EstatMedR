suppressMessages(library(readxl, warn.conflicts = FALSE))
suppressMessages(library(car, warn.conflicts = FALSE))
suppressMessages(library(lattice, warn.conflicts = FALSE))
suppressMessages(library(ggplot2, warn.conflicts = FALSE))
suppressMessages(library(rcompanion, warn.conflicts = FALSE))

source ("eiras.bartitle.R")
source("eiras.tab.dCohen.R")
source("eiras.tab.eta2.R")

if (!exists("alternative"))
{
  alternative <- "two.sided"
}

Dtfrm_long <- readxl::read_excel("Nutricao.xlsx", sheet="independente")
Dtfrm_long$Instructor <- as.factor(Dtfrm_long$Instructor)

Dtfrm <- read_excel("Nutricao.xlsx", sheet="dependente")

# estatistica descritiva
cat(bartitle("Estatistica descritiva"))
  
res <- summary.data.frame(Dtfrm, digits=2)
print (res)

# boxplot: grupos
boxplot (unlist(Dtfrm["Brendon Small"]), 
         unlist(Dtfrm["Coach McGuirk"]), 
         main="Distribuicao dos dados", 
         names= c(names(Dtfrm)[2],names(Dtfrm)[3]),
         ylab="Ingesta de Sodio")
# boxplot: diferenca
boxplot (unlist(Dtfrm["dif"]), 
         main="Distribuicao da diferenca", 
         xlab = paste(names(Dtfrm)[3],"-",names(Dtfrm)[2]),
         ylab="Diferenca da Ingesta de Sodio")
# density plot: grupos
# densidade1 <- density(unlist(Dtfrm[2]))
# densidade2 <- density(unlist(Dtfrm[3]))
# plot (densidade1, 
#       main="Distribuicao dos dados",
#       xlab="Ingesta de Sodio",ylab="Densidade",
#       xlim = c(min(densidade1$x,densidade2$x),max(densidade1$x,densidade2$x)),
#       ylim = c(min(densidade1$y,densidade2$y),max(densidade1$y,densidade2$y))
# )
# lines (densidade2, lty=2)
# legend ("topright",c(names(Dtfrm)[2],names(Dtfrm)[3]),
#         lty=c(1,2),
#         cex=0.8, box.lwd=0, bg = "transparent")

car::densityPlot(Sodium ~ Instructor, 
                 main="Distribuicao dos dados",
                 xlab="Ingesta de Sodio",ylab="Densidade",
                 data=Dtfrm_long,
                 rug=TRUE
)
rug(unlist(Dtfrm["Coach McGuirk"]), col="cyan")

# density plot: diferenca
# densidade <- density(Dtfrm$dif)
# plot (densidade, 
#       main="Distribuicao dos dados",
#       xlab=paste("Ingesta de Sodio"," (",names(Dtfrm)[3],"-",names(Dtfrm)[2],")", sep=""),ylab="Densidade"
# )
car::densityPlot(unlist(Dtfrm["dif"]), 
                 main="Distribuicao dos dados",
                 xlab=paste("Ingesta de Sodio"," (",names(Dtfrm)[3],"-",names(Dtfrm)[2],")", sep=""),ylab="Densidade",
                 rug=TRUE)

# significancia estatistica

# dados da amostra
n <- sum(!is.na(Dtfrm$dif)) 

# significancia estatistica
cat(bartitle("Analise de significancia estatistica"))

t_out <- t.test(Dtfrm$dif, mu=0, alternative=alternative)
t <- t_out$statistic # estatistica de teste t
df <- t_out$parameter # graus de liberdade

# relatorio
cat("\nTamanho da amostra \n", sep="")
cat ("\tn = ", n, " pares\n", sep="") 
print(t_out)

# distribuicao t sob H0 (central: ncp = 0)
alfa <- 0.05
xmin <- 0 + min(-4,3*t_out$statistic)
xmax <- 0 + max( 4,3*t_out$statistic)
x <- seq(xmin,xmax,length.out=1000)
y <- dt(x, df)
dtH0 <- data.frame(x,y)

# graficos
alternative.txt <- "bilateral"
if (alternative=="less") {alternative.txt <- "unilateral a esquerda"}
if (alternative=="greater") {alternative.txt <- "unilateral a direita"}
plot(dtH0,
     main=paste("Teste t relacionado ",alternative.txt,"\nt = ",round(t,5),", df = ",round(df,3),sep=""),
     xlab="t", ylab="densidade",
     xlim=c(xmin,xmax),
     lwd=1, lty=1, type="l"
)
if (alternative=="two.sided")
{
  qalfa <- qt(c(alfa/2,1-alfa/2),df,0)
  abline(v=qalfa[1], lty = 3)
  abline(v=qalfa[2], lty = 3)
  abline(v=abs(t),lwd=1,lty=2)
  abline(v=-abs(t),lwd=1,lty=2)
  # area do valor p
  polx <- dtH0$x[dtH0$x>=abs(t)]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x>=abs(t)]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
  polx <- dtH0$x[dtH0$x<=-abs(t)]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x<=-abs(t)]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
  # area alfa
  polx <- dtH0$x[dtH0$x<=qalfa[1]]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x<=qalfa[1]]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
  polx <- dtH0$x[dtH0$x>=qalfa[2]]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x>=qalfa[2]]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
}
if (alternative=="less")
{
  qalfa <- qt(alfa,df,0)
  abline(v=qalfa, lty = 3)
  abline(v=t,lwd=1,lty=2)
  # area do valor p
  polx <- dtH0$x[dtH0$x<=t]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x<=t]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
  # area alfa
  polx <- dtH0$x[dtH0$x<=qalfa]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x<=qalfa]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
}
if (alternative=="greater")
{
  qalfa <- qt(1-alfa,df,0)
  abline(v=qalfa, lty = 3)
  abline(v=t,lwd=1,lty=2)
  # area do valor p
  polx <- dtH0$x[dtH0$x>=t]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x>=t]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#EE802622",col="#EE802688",lwd=5)
  # area alfa
  polx <- dtH0$x[dtH0$x>=qalfa]; polx <- c(min(polx),polx,max(polx))
  poly <- dtH0$y[dtH0$x>=qalfa]; poly <- c(0,poly,0)
  polygon(polx,poly,border="#a3261b22",col="#a3261b88",lwd=5)
}

# legenda
p_txt <- t_out$p.value;
if (p_txt > 0.001) 
{
  p_txt <- round(p_txt,4)
} else
{
  p_txt <- format(format(p_txt, scientific = TRUE, digits = 4)) 
} 
legend ("topright",
        c("H0","t obs.","t crit.",paste("p=",p_txt,sep=""),"alfa=0.05"), 
        lwd=c(1,1,1,10,10),
        lty=c(1,2,3,1,1),
        pch=NA,
        col=c("black","black","black","#EE802688","#a3261b88"),
        box.lwd=0, bg="transparent")

cat(bartitle("Analise de significancia pratica"))
cat("Tamanho de efeito de Cohen:\n")
# eta^2
F <- t^2 # estatistica de teste F de Fisher
eta2 <- F/(F+df)
ec <- tab.eta2(eta2)
cat("\tEta^2 = ",eta2,sep="")
cat(" (",ec,")\n",sep="")
