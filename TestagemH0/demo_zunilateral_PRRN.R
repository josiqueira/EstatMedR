suppressMessages(library(readxl, warn.conflicts = FALSE))
suppressMessages(library(BSDA, warn.conflicts = FALSE))

mu <- 20
sigma <- 4
alfa <- 0.05
side <- "less" # c("two-sided", "less", "greater")

df.ratos <- readxl::read_excel("Table 8.2 RawBirthWeight.xls")
dados <- df.ratos$RBW 
        
cat("Populacao:")
cat("\n\tmu = ",mu, sep="")
cat("\n\tsigma = ",sigma, sep="")

# estatistica
cat("\nAmostra:")
n <- length(dados[!is.na(dados)])
cat("\n\tn = ",n, sep="")
media <- mean(dados)
cat("\n\tmédia = ",media, sep="")
ep <- sigma / sqrt(n)
cat("\n\tep = ",ep, sep="")

cat("\nTeste z:")
z.test <- BSDA::z.test(x=dados, sigma.x=sigma, mu=mu,
                       alternative=side, conf.level=.95)
print(z.test)
z <- z.test$statistic

cat("Significancia pratica:")
d <- (media - mu)/sigma
if (side=="two.sided")
{
        d <- abs(d)
}       
cat("\n\td de Cohen = ",d, sep="")
tab.cohen <- ""
d <- abs(d)
if (d < 0.01) {tab.cohen <- "minusculo"}
if (d >= 0.01 & d < 0.2) {tab.cohen <- "muito pequeno"}
if (d >= 0.2 & d < 0.5) {tab.cohen <- "pequeno"}
if (d >= 0.5 & d < 0.8) {tab.cohen <- "intermediario"}
if (d >= 0.8 & d < 1.2) {tab.cohen <- "grande"}
if (d >= 2.0) {tab.cohen <- "muito grande"}
cat(" (",tab.cohen,")", sep="")
# normal padronizada da populacao
range <- (max(4,abs(z)))
xpopp <- seq(-range, range, length.out=1000)
ypopp <- dnorm(xpopp, mean=0, sd=1)
xmin <- min(xpopp)
xmax <- max(xpopp)
ymin <- min(ypopp)
ymax <- max(ypopp)
plot(xpopp, ypopp, 
     main="Distribuicao amostral padronizada\ncentrada em H0",
     xlab="Medias amostrais padronizadas",
     ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax),
     type="l",
     lwd=2, col="black")
# hachura das caudas
if (side=="two.sided")
{
        z25 <- abs(qnorm(p=alfa/2, mean=0, sd=1)) # |z| em 2.5%
} else
{
        z25 <- abs(qnorm(p=alfa, mean=0, sd=1)) # |z| em 2.5%
}
N95 = round(c(-z25, z25),2)
if (side=="two.sided" | side=="less")
{
        xesq <- xpopp[which(xpopp<=N95[1])]
        yesq <- ypopp[which(xpopp<=N95[1])]
        xesq <- c(min(xesq),xesq,max(xesq))
        yesq <- c(   0     ,yesq,   0     )
        polygon(xesq,yesq,border=NA,col="lightblue")
}
if (side=="two.sided" | side=="greater")
{
        xdir <- xpopp[which(xpopp>=N95[2])]
        ydir <- ypopp[which(xpopp>=N95[2])]
        xdir <- c(min(xdir),xdir,max(xdir))
        ydir <- c(   0     ,ydir,   0     )
        polygon(xdir,ydir,border=NA,col="lightblue")
}
# H0
points(0,ymax/20,pch=13)
text(0,ymax/20,"H0",pos=3,cex=0.8)
# media amostral
points(z,ymax/20,pch=16)
text(z,ymax/20,"z",pos=3,cex=0.8)
if (side=="two.sided")
{
        ;
}

# hachura das area alem da media sob H0
if (side=="two.sided" | side=="less")
{
        xesq <- xpopp[which(xpopp<=z)]
        yesq <- ypopp[which(xpopp<=z)]
        lines(xesq,yesq,lwd=3,col="#008800")
        xesq <- c(min(xesq),xesq,max(xesq))
        yesq <- c(   0     ,yesq,   0     )
        polygon(xesq,yesq,border=NA,col="#00880044")
}
if (side=="two.sided" | side=="greater")
{
        xdir <- xpopp[which(xpopp>=z)]
        ydir <- ypopp[which(xpopp>=z)]
        lines(xdir,ydir,lwd=3,col="#008800")
        xdir <- c(min(xdir),xdir,max(xdir))
        ydir <- c(   0     ,ydir,   0     )
        polygon(xdir,ydir,border=NA,col="#00880044")
}        

