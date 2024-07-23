source("eiras.bartitle.R")
source("eiras.bivCI.R")

options(warn=-1) # disable warnings

H0.M <- c(172,76.68)
alfa <- 0.05
B <- 1e4

Dados <- readRDS("Adm2008.rds")
Masc <- subset(Dados, Sexo=="Masculino", select=c(Estatura, MCT))
rm(Dados)

p <- dim(Masc)[2]
n.M <- dim(Masc)[1]
cov.M <- cov(Masc)
mean.M <- colMeans(Masc)
Sinv.M <- solve(cov.M)
F.M <- (n.M*(n.M-p)/((n.M-1)*p))*
       (mean.M-H0.M)%*%Sinv.M%*%(mean.M-H0.M)
p.M <- 1-pf(q=F.M,df1=p,df2=n.M-p)
cat("\nMANOVA para uma condição:\n", sep="")
cat("\tF(",p,",",n.M-p,") = ",F.M,", p = ",p.M,"\n", sep="")

T2One <- MVTests::OneSampleHT2(Masc,
                               mu=H0.M,
                               alpha=alfa) 
summary(T2One)

# 'mcd' for minimum covariance determinant estimator
print(rrcov::T2.test(Masc, 
               mu = H0.M, 
               conf.level = 1-alfa,
               method="mcd"))

T2crit <- ((n.M-1)*p/(n.M-p))*qf(1-alfa, p, n.M-p)
eigen_result <- eigen(cov.M)
eigvl <- eigen_result$values
a <- sqrt(T2crit*eigen_result$values[1]/n.M) # (5-19)
b <- sqrt(T2crit*eigen_result$values[2]/n.M) # (5-19)
theta <- atan2(eigen_result$vectors[2, 1], eigen_result$vectors[1, 1])
theta_seq <- seq(0, 2 * pi, length.out = 100)
ellipse_x <- mean.M[1] + a * cos(theta_seq) * cos(theta) - 
  b * sin(theta_seq) * sin(theta)
ellipse_y <- mean.M[2] + a * cos(theta_seq) * sin(theta) + 
  b * sin(theta_seq) * cos(theta)
plot(ellipse_x, ellipse_y, type = "l", xlim=c(170,180),
     xlab="Estatura (m)", 
     ylab="MCT (kg)",
     main="Região elíptica de confiança de 95%\nEstudante de Administração Masculino",
     col="black")
points(mean.M[1], mean.M[2], col = "black", pch = 19)
points(H0.M[1], H0.M[2], pch=3, col="black")
text(H0.M[1], H0.M[2], pos=1, expression(H[0]))
text(mean.M[1],mean.M[2], pos=2, "M", col="black")

mvdalab::MVcis(Masc,
               level=1-alfa,
               Vars2Plot=c(1, 2), 
               include.zero=FALSE)
