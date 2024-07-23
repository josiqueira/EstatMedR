# demo_r_de_Spearman_boot.R

# disable warnings
options(warn=-1)

alfa <- 0.05
B <- 1e4

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

# enable warnings
options(warn=0)
