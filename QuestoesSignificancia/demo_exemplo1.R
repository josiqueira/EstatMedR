# proporcoes (tamanho de efeito desejado)
if(!exists("pH0")){pH0 <- 0.5}
if(!exists("pH1")){pH1 <- 0.7}
if(!exists("alfa")){alfa <- 0.05}
if(!exists("razao_alocacao")){razao_alocacao <- 1}
if(!exists("alternative")){alternative <- "two.sided"}

max_poder <- 0.95 # criterio de parada
colnomes <- c("n_controle","n_intervencao","n_total","poder")
df_proppoder <- data.frame(matrix(nrow=0, ncol=length(colnomes)))
names(df_proppoder) <- colnomes

n <- 10 # n inicial
ok <- FALSE
while(!ok)
{
  n <- n+1
  n1 <- n
  n2 <- round(n1/razao_alocacao,0)
  res <-  pwr::pwr.2p2n.test(h = pwr::ES.h(p1 = pH1, p2 = pH0), 
                             n1 = n1, n2 = n2, 
                             sig.level = alfa,
                             alternative = alternative)
  df_tmp <- data.frame(n1,n2,n1+n2,round(res$power*100,1))
  names(df_tmp) <- colnomes
  df_proppoder <- rbind(df_proppoder, df_tmp)
  if (res$power >= max_poder)
  {
    ok <- TRUE
  }
}
# prmatrix(df_proppoder, 
#          rowlab=rep("",nrow(df_proppoder)) )

cat ("\nCalculo do tamanho da amostra:\n")
df_tmp <- df_proppoder
df_tmp$p80 <- df_tmp$poder>=80
df_tmp$p90 <- df_tmp$poder>=90
r80 <- which(df_tmp$p80); r80 <- r80[1]
r90 <- which(df_tmp$p90); r90 <- r90[1]
cat ("\nValores proximos a 80% de poder:\n")
prmatrix(df_proppoder[r80,], rowlab=rep("",length(r80)) )
cat ("\nValores proximos a 90% de poder:\n")
prmatrix(df_proppoder[r90,], rowlab=rep("",length(r90)) )

opar <- par()
layout(matrix(1:2,nrow=2),widths=c(1,1))
par(mar=c(4,4,1,0))

plot(df_proppoder$n_total,df_proppoder$poder,
     main="Total",
     xlab="n total",
     ylim=c(0,100), ylab="poder (%)",
     type="l", axes=FALSE)
axis(1)
axis(2)
lines(c(0,df_tmp$n_total[r80],df_tmp$n_total[r80]), c(80,80,0),lty=2)
lines(c(0,df_tmp$n_total[r90],df_tmp$n_total[r90]), c(90,90,0),lty=3)

plot(df_proppoder$n_controle,df_proppoder$poder,
     main=paste0("Alocacao ",razao_alocacao,":1"),
     xlab="n por grupo",
     xlim=c(min(df_proppoder$n_total),max(df_proppoder$n_total)),
     ylim=c(0,100), ylab="poder (%)",
     type="l", lty=2, col="blue", axes=FALSE)
axis(1)
axis(2)
lines(df_proppoder$n_intervencao,df_proppoder$poder,
     lwd=2, lty=3, col="red")
lines(c(0,df_tmp$n_controle[r80],df_tmp$n_controle[r80]), c(80,80,0),lty=2,col="blue")
lines(c(0,df_tmp$n_controle[r90],df_tmp$n_controle[r90]), c(90,90,0),lty=3,col="blue")
lines(c(0,df_tmp$n_intervencao[r80],df_tmp$n_intervencao[r80]), c(80,80,0),lty=2,col="red")
lines(c(0,df_tmp$n_intervencao[r90],df_tmp$n_intervencao[r90]), c(90,90,0),lty=3,col="red")
legend ("right",
        c("controle","intervencao"), 
        lty=c(2,3), 
        lwd=c(1,2), 
        pch=NA,
        col=c("blue","red"), 
        bty="n")

par <- opar
