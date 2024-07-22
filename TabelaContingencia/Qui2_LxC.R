alfa <- 0.05
B <- 1e5
tbl <- ("
Droga  ComNausea SemNausea
        A     3         5
        B     7         2
        C     6         3
        ")
tcrxc <- as.matrix(read.table(textConnection(tbl),header=TRUE,row.names=1))
print(tcrxc)
res <- chisq.test(tcrxc,simulate.p.value=TRUE,B=B)
print(res)
x2 <- res$statistic # estatistica de teste qui-quadrado
n <- sum(res$observed)
r <- nrow(tcrxc)
c <- ncol(tcrxc)
df <- (r-1)*(c-1) # graus de liberdade de tabela de contingencia rxc
qui_critico <- qchisq(1-alfa,df)
phi2 <- x2/n # phi ou w de Cohen
dim <- min(r,c)-1 # dimensao de tcrxc: dim = max(phi^2)
V <- sqrt(phi2/dim)
cat("\nV de Cramer = ", round(V,4), "\n", sep="")
print(effectsize::interpret_cramers_v(V))
print(try(rcompanion::cramerV(tcrxc,ci=TRUE,R=1e3)))

# distribuicao qui quadrado sob H0
qui <- rchisq(1e6, df)
dqui <- density(qui)
# distribuicao qui quadrado sob H1
quih1 <- rchisq(1e6, df, x2)
dquih1 <- density(quih1)
# sugestao para os eixos, evitando cauda muito longa
# media da dist. qui^2
mediah0 <- df
dph0 <- sqrt(2*df)
mediah1 <- df+x2
dph1 <- sqrt(2*(df+2*x2))
maxx <- mediah1+3*dph1
maxy <- max(c(dqui$y,dquih1$y))
maxyXcal <- max(dquih1$y)
plot(dqui$x[dqui$x>0],dqui$y[dqui$x>0],xlim=c(0,maxx),
     ylim=c(0,maxy), xlab="qui-quadrado", ylab="densidade", type="l")
lines(c(qui_critico,qui_critico),c(0,maxy), lty=2)
lines(c(x2,x2),c(0,maxyXcal), lwd=2, lty=2)
legend ("topright",
        c("H0",
          paste("X^2 critico=",round(qui_critico,4),sep=""),
          paste("X^2=",round(x2,4),sep="")),
        pch=c(NA,NA,NA),
        lwd=c(1,1,2),
        lty=c(1,2,2),
        col=c("#000000","#000000","#000000"),
        box.lwd=0,
        bg="transparent")

