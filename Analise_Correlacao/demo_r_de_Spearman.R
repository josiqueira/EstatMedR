# demo_r_de_Spearman_boot.R

# disable warnings
options(warn=-1)

alfa <- 0.05
B <- 0
source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

# cores
col <-  friendlycolor(31) # preto
bg <- friendlycolor(24) # amarelo
pch <- 21 # circulo

# valores
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
cat("estatura:",estatura,"\n")
cat("massa corporal:",massa,"\n")
lst <- correg(estatura, massa, 
              method = "spearman",
              main="",
              xlab="Estatura (cm)", 
              ylab="Massa corporal (kg)", 
              jitter=0,
              col=col, bg=bg, pch=pch,
              alpha=alfa,
              B=B)
# arrows
for (i in 1:(length(estatura)-1))
{
  arrows(estatura[i],massa[i],
         estatura[i+1],massa[i+1], length=0.15, angle=20)
}
# enable warnings
options(warn=0)
