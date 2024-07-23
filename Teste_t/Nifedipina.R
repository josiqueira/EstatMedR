# Teste t para uma condicao, unilateral a direita

# Exemplo:
# 
# Suspeita-se de que um medicamento vasodilatador (Nifedipina)
# para Hipertensao Arterial, amplamente receitado, esteja 
# aumentando a frequencia cardíaca dos pacientes.
# 
# Sabe-se que a frequencia cardiaca na populacao normal 
# tem Distribuicao Normal com media de 70 bpm.
# 
# Para verificar essa suspeita, planejou-se obter uma 
# amostra aleatoria de 50 pacientes que recebem Nifedipina
# para se medir a frequencia cardiaca.

# suppress warnings
options(warn=-1)

# H0: mu=70 (o valor conhecido da populacao ... 70 bpm)
# H1: mu>70 (a suspeita de que ha aumento dos bpm com nifedipina)
mu_pop = 70 # media populacional
alfa <- 0.01 # nivel de significancia adotado
alternative <- "greater"

# nome do arquivo de saída (se NA, saida em tela)
cat ("Voce pode guardar os resultados em um arquivo\n") 
cat ("(deixando em branco, a saida eh feita na tela)\n") 
filename <- readline(prompt="Nome do arquivo: ")
if (filename == "") {filename <- NA}

Tabela <- ("
           Paciente   BPM
           1          72
           2          74
           3          70
           4          70
           5          69
           6          71
           7          72
           8          71
           9          69
           10         74
           11         71
           12         71
           13         70
           14         73
           15         69
           16         68
           17         68
           18         71
           19         71
           20         72
           21         70
           22         69
           23         73
           24         69
           25         71
           26         70
           27         72
           28         73
           29         70
           30         72
           31         67
           32         72
           33         67
           34         68
           35         69
           36         72
           37         70
           38         70
           39         70
           40         71
           41         74
           42         67
           43         69
           44         71
           45         71
           46         73
           47         71
           48         71
           49         70
           50         71
")
Nifedipina <- read.table(textConnection(Tabela),header=TRUE)
# a coluna paciente nao deve ser tratada como numero
Nifedipina$Paciente <- as.factor(Nifedipina$Paciente)

# teste t
t_out <- t.test(Nifedipina$BPM, mu=mu_pop, 
                conf.level = 1-alfa, alternative = alternative)
# d de Cohen
df <- t_out$parameter # graus de liberdade
t <- t_out$statistic # estatistica de teste t observada
dp <- sd(Nifedipina$BPM)
m <- t_out$estimate
d <- abs((m-mu_pop)/dp)
# Sawilowsky, S (2009) New effect size rules of thumb. Journal of Modern Applied Statistical Methods 8(2): 467-74.
if (d<0.1) {mag<-c("Desprezivel")} 
if (d>=0.1 && d<0.2) {mag<-c("Muito pequeno")} 
if (d>=0.2 && d<0.5) {mag<-c("Pequeno")}
if (d>=0.5 && d<0.8) {mag<-c("Intermediario")}
if (d>=0.8 && d<1.2) {mag<-c("Grande")}
if (d>=1.2 && d<2) {mag<-c("Muito grande")} 
if (d>=2) {mag<-c("Enorme")}

if (!is.na(filename))
{
  filepng <- paste(filename,".png", sep="")
  png(filepng, width = 600, height = 400)
}

# distribuicao t sob H0 (central: ncp = 0)
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
        c("H0","t obs.","t crit.",
          paste("p=",p_txt,sep=""),
          paste("alfa=",alfa,sep="")), 
        lwd=c(1,1,1,10,10),
        lty=c(1,2,3,1,1),
        pch=NA,
        col=c("black","black","black","#EE802688","#a3261b88"),
        box.lwd=0, bg="transparent")

if (!is.na(filename))
{
  dev.off()
}

if (!is.na(filename))
{
  filetxt <- paste(filename,".txt", sep="")
  sink (filetxt)
  cat ("---------------------------------------\n")
  cat("Processando:",filename,"\n")
  cat ("---------------------------------------\n\n")
} 

# sumarizando BPM
cat(names(Nifedipina)[2],":\n")
sumario <- summary(Nifedipina$BPM)
print (sumario)
cat("\tDesvio-padrao = ",round(dp,2),"\n", sep="")
cat("\tTamanho da amostra = ",length(Nifedipina$BPM),"\n")

# resultado do t.test
print (t_out)

cat("Significancia pratica:\n")
cat("\td de Cohen = ",d," (",mag,")","\n\n",sep="")

if (!is.na(filename))
{
  sink()
  cat ("\n-----------------------------------------\n")
  cat ("Relatorio disponivel em ",filetxt,"\n",sep="")
  cat ("Grafico disponivel em ",filepng,"\n",sep="")
  cat (  "-----------------------------------------\n")
}

# enable warnings
options(warn=0)
