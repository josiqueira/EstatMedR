source("eiras.create.population.R")

s <- 27863
set.seed(s)
col1 <- "#000000"
col2 <- eiras::FriendyColor(8)
col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")
numamostras <- 1e5
n1 <- 6
n2 <- 8
alpha <- 0.05

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))

# amostras
rm(.Random.seed, envir=globalenv()) # reset seed

c_tstud <- c()
c_twelch <- c()
c_utrad <- c()
c_bm <- c()
wt.a <- wt.b <- wt.c <- wt.d <- 0 # Welch t x Student t
ub.a <- ub.b <- ub.c <- ub.d <- 0 # U x BM
tu.a <- tu.b <- tu.c <- tu.d <- 0 # Student t x U
wu.a <- wu.b <- wu.c <- wu.d <- 0 # Welch t x U
tb.a <- tb.b <- tb.c <- tb.d <- 0 # Student t x BM
wb.a <- wb.b <- wb.c <- wb.d <- 0 # Welch t x BM
for (amostra in 1:numamostras)
{
  amostra1 <- sample(pop1,size=n1)
  amostra2 <- sample(pop2,size=n2)
  
  # cat("\nTeste t de Student:\n")
  t <- t.test(amostra1,amostra2,var.equal=TRUE)
  p.tstud <- t$p.value
  c_tstud <- c(c_tstud, p.tstud)
  
  # cat("\nTeste t de Welch/Satterthwite:\n")
  t <- t.test(amostra1,amostra2)
  p.twelch <- t$p.value
  c_twelch <- c(c_twelch, p.twelch)
  
  # cat("\nTeste U de Mann-Whitney Convencional:\n")
  u <- wilcox.test(amostra1,amostra2, exact = FALSE)
  p.utrad <- u$p.value 
  c_utrad <- c(c_utrad, p.utrad)
  
  # Hollander & Wolfe, p. 110, results p. 111 and p. 126
  # cat("\nTeste U de Mann-Whitney Exato:\n")
  datafrm <- data.frame(
    valor = c(amostra1,amostra2),
    grupo = factor(rep(c("amostra1", "amostra2"),
                       c(length(amostra1), length(amostra2))))
  )
  e <- coin::wilcox_test(valor ~ grupo, data = datafrm,
                         distribution = "exact", conf.int = TRUE)
  
  
  # cat("\nTeste B de Brunner-Munzel:\n")
  bm <- lawstat::brunner.munzel.test(amostra1,amostra2)
  p.bm <- bm$p.value
  c_bm <- c(c_bm, p.bm)

  # "Welch t x Student t",
  if (p.twelch<alpha & p.tstud<alpha) {wt.a <- wt.a+1}
  if (p.twelch<alpha & p.tstud>=alpha) {wt.b <- wt.b+1}
  if (p.twelch>=alpha & p.tstud<alpha) {wt.c <- wt.c+1}
  if (p.twelch>=alpha & p.tstud>=alpha) {wt.d <- wt.d+1}
  # "U x BM",
  if(is.finite(p.bm))
  {
    if (p.tstud<alpha & p.bm<alpha) {ub.a <- ub.a+1}
    if (p.tstud<alpha & p.bm>=alpha) {ub.b <- ub.b+1}
    if (p.tstud>=alpha & p.bm<alpha) {ub.c <- ub.c+1}
    if (p.tstud>=alpha & p.bm>=alpha) {ub.d <- ub.d+1}
  }
  # "Student t x U",
  if (p.tstud<alpha & p.utrad<alpha) {tu.a <- tu.a+1}
  if (p.tstud<alpha & p.utrad>=alpha) {tu.b <- tu.b+1}
  if (p.tstud>=alpha & p.utrad<alpha) {tu.c <- tu.c+1}
  if (p.tstud>=alpha & p.utrad>=alpha) {tu.d <- tu.d+1}
  # "Welch t x U",
  if (p.twelch<alpha & p.utrad<alpha) {wu.a <- wu.a+1}
  if (p.twelch<alpha & p.utrad>=alpha) {wu.b <- wu.b+1}
  if (p.twelch>=alpha & p.utrad<alpha) {wu.c <- wu.c+1}
  if (p.twelch>=alpha & p.utrad>=alpha) {wu.d <- wu.d+1}
  # "Student t x BM",
  if(is.finite(p.bm))
  {
    if (p.tstud<alpha & p.bm<alpha) {tb.a <- tb.a+1}
    if (p.tstud<alpha & p.bm>=alpha) {tb.b <- tb.b+1}
    if (p.tstud>=alpha & p.bm<alpha) {tb.c <- tb.c+1}
    if (p.tstud>=alpha & p.bm>=alpha) {tb.d <- tb.d+1}
  }    
  "Welch t x BM"
  if(is.finite(p.bm))
  {
    if (p.twelch<alpha & p.bm<alpha) {wb.a <- wb.a+1}
    if (p.twelch<alpha & p.bm>=alpha) {wb.b <- wb.b+1}
    if (p.twelch>=alpha & p.bm<alpha) {wb.c <- wb.c+1}
    if (p.twelch>=alpha & p.bm>=alpha) {wb.d <- wb.d+1}
  }
}

pares <- c(
  "Welch t", "Student t",
  "Mann-Whitney U", "Brunner-Munzel B",
  "Student t", "Mann-Whitney U",
  "Welch t", "Mann-Whitney U",
  "Student t", "Brunner-Munzel B",
  "Welch t", "Brunner-Munzel B"
)
siglas <- c(
  "Wt","St",
  "U","BM",
  "St","U",
  "Wt","U",
  "St","BM",
  "Wt","BM"
)

c_tstud <- sort(c_tstud)
r_tstud <- sum(c_tstud<alpha,na.rm=TRUE)/length(c_tstud)
c_twelch <- sort(c_twelch)
r_twelch <- sum(c_twelch<alpha,na.rm=TRUE)/length(c_twelch)
c_utrad <- sort(c_utrad)
r_utrad <- sum(c_utrad<alpha,na.rm=TRUE)/length(c_utrad)
c_bm <- sort(c_bm)
r_bm <- sum(c_bm<alpha,na.rm=TRUE)/length(c_bm)

for (i in seq(1,length(pares),by=2))
{
  cat("\n-------------------\n",
      pares[i]," x ",
      pares[i+1],
      "\n-------------------\n",sep="")
  if (i==1)  {a<-wt.a; b<-wt.b; c<-wt.c; d<-wt.d; 
  r1<-r_twelch; r2<-r_tstud}
  if (i==3)  {a<-ub.a; b<-ub.b; c<-ub.c; d<-ub.d; 
  r1<-r_utrad; r2<-r_bm}
  if (i==5)  {a<-tu.a; b<-tu.b; c<-tu.c; d<-tu.d; 
  r1<-r_tstud; r2<-r_utrad}
  if (i==7)  {a<-wu.a; b<-wu.b; c<-wu.c; d<-wu.d; 
  r1<-r_twelch; r2<-r_utrad}
  if (i==9)  {a<-tb.a; b<-tb.b; c<-tb.c; d<-tb.d; 
  r1<-r_tstud; r2<-r_bm}
  if (i==11) {a<-wb.a; b<-wb.b; c<-wb.c; d<-wb.d; 
  r1<-r_twelch; r2<-r_bm}
  
  cat("\nTabela de contingencia:\n")
  m <- matrix(c(a,b,c,d),ncol=2,nrow=2,byrow=TRUE)
  colnames(m) <- c(paste0(siglas[i+1]," H1"),paste0(siglas[i+1]," H0"))
  rownames(m) <- c(paste0(siglas[i  ]," H1"),paste0(siglas[i  ]," H0"))
  print (m)
  trials <- sum(m)
  cat("\nSimulação com total de ",trials," amostragens.")
  
  cat("\n")
  cat("\nTeste da concordância entre ",
      siglas[i]," e ",siglas[i+1],":\n",sep="")
  cat("\n\tH0: G =  0")
  cat("\n\tH1: G <> 0")
  G <- ((a+d)-(b+c))/numamostras
  z <- (a+d - numamostras/2)/sqrt(numamostras/4)
  p <- 2*(1-pnorm(abs(z)))
  cat("\n")
  cat("\n\tG = ",G,sep="")
  cat("\n\tz = ",z,", p = ",p,sep="")
  cat("\n\nConclusão:")
  if(p<0.05)
  {
    cat(" há concordância entre os testes ",
        siglas[i]," e ",siglas[i+1],".",sep="")
  } else
  {
    cat(" não há concordância entre os testes ",
        siglas[i]," e ",siglas[i+1],".",sep="")
  }
  cat("\n")
  cat("\nTeste da diferença de proporção de sucessos:")
  cat("\n\tH0: p(",siglas[i],")-p(",siglas[i+1],") =  0",sep="")
  cat("\n\tH1: p(",siglas[i],")-p(",siglas[i+1],") <> 0",sep="")
  cat("\nRejeições corretas:")
  cat("\n\t",pares[i],": ",r1,sep="")
  cat("\n\t",pares[i+1],": ",r2,sep="")
  cat("\n\tDiferenca:\n")
  bintest <- DescTools::BinomDiffCI(a+b,trials,a+c,trials)
  print(bintest)
  cat("\nConclusão: a proporção de rejeições corretas por ",
      pares[i]," é ",sep="")
  if(bintest[2]>0 & bintest[3]>0 )
  {
    cat("superior")
  } 
  if(bintest[2]<0 & bintest[3]<0 )
  {
    cat("inferior")
  } 
  if(bintest[2]<0 & bintest[3]>0 )
  {
    cat("indistinguível")
  }
  cat(" à de ",pares[i+1],".",sep="")
  cat("\n")
}

d_tstud <- density(c_tstud,na.rm=TRUE)
d_twelch <- density(c_twelch,na.rm=TRUE)
d_utrad <- density(c_utrad,na.rm=TRUE)
d_bm <- density(c_bm,na.rm=TRUE)
ymax <- max(c(d_tstud$y,d_twelch$y,d_utrad$y,d_bm$y),na.rm=TRUE)
plot (NA,
      main="Comparação dos valores p obtidos",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,1), 
      ylim=c(0,ymax), 
      axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="darkgray")
lines(d_tstud,lwd=2, lty=4, col="darkgray")
lines(d_bm,lwd=2, lty=1, col="black")
lines(d_utrad,lwd=2, lty=4, col="black")
legend("right",
       c(paste0("t Welch/Satterthwite (rej.",round(r_twelch*100,1),"%)"),
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("B Brunner-Munzel (rej.",round(r_bm*100,1),"%)"),
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)")
         ),
       lty=c(1,4,1,4),
       lwd=2,
       col=c("darkgray","darkgray","black","black"),
       box.lwd=0, bg="transparent")

plot (NA,
      main="Comparação dos valores p obtidos",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,0.1), 
      ylim=c(0,ymax), 
      axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="darkgray")
lines(d_tstud,lwd=2, lty=4, col="darkgray")
lines(d_bm,lwd=2, lty=1, col="black")
lines(d_utrad,lwd=2, lty=4, col="black")
legend("topright",
       c(paste0("t Welch/Satterthwite (rej.",round(r_twelch*100,1),"%)"),
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("B Brunner-Munzel (rej.",round(r_bm*100,1),"%)"),
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)")
       ),
       lty=c(1,4,1,4),
       lwd=2,
       col=c("darkgray","darkgray","black","black"),
       box.lwd=0, bg="transparent")

plot(ecdf(c_tstud), 
     main="Empirical Cumulative Distribution Function",
     xlab="Valor p", 
     ylab="Probabilidade Acumulada", cex=0.3,
     xlim=c(0,1), ylim=c(0,1))
lines(ecdf(c_twelch), cex=0.3, col=eiras::FriendyColor(9))
lines(ecdf(c_utrad), cex=0.3, col=eiras::FriendyColor(15))
lines(ecdf(c_bm), cex=0.3, col=eiras::FriendyColor(27))
abline(v=0.05,lty=3)
legend ("right",
        c("Student t", "Welch t", "Mann-Whitney", "Brunner-Munzel"),
        lwd=2,
        col=c("black",
              eiras::FriendyColor(9),
              eiras::FriendyColor(15),
              eiras::FriendyColor(27)),
        box.lwd=0, bg="transparent")

plot(ecdf(c_tstud), 
     main="Empirical Cumulative Distribution Function",
     xlab="Valor p", 
     ylab="Probabilidade Acumulada", cex=0.3,
     xlim=c(0,0.1), ylim=c(0,1))
lines(ecdf(c_twelch), cex=0.3, col=eiras::FriendyColor(9))
lines(ecdf(c_utrad), cex=0.3, col=eiras::FriendyColor(15))
lines(ecdf(c_bm), cex=0.3, col=eiras::FriendyColor(27))
abline(v=0.05,lty=3)
legend ("bottomright",
        c("Student t", "Welch t", "Mann-Whitney", "Brunner-Munzel"),
        lwd=2,
        col=c("black",
              eiras::FriendyColor(9),
              eiras::FriendyColor(15),
              eiras::FriendyColor(27)),
        box.lwd=0, bg="transparent")

