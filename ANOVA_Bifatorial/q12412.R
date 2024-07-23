library (readxl)

dados <- readxl::read_excel("./Biometria_qst.xls")

with(dados,plot(estatura,peso))

# Ha valores de peso e estatura com a string "NA" (em vez do NA, not available)
dados$peso[dados$peso=="NA"] <- NA
dados$estatura[dados$estatura=="NA"] <- NA
# Por causa destes textos "NA", as variáveis tornaram-se character
print(str(dados))
# Para regressão, precisamos converter em números
dados$peso <- as.numeric(dados$peso)
dados$estatura <- as.numeric(dados$estatura)

modelo <- lm(peso~estatura, data=dados, na.action = na.exclude)
reg <- summary(modelo)
print(reg)

min.estatura <- min(dados$estatura,na.rm=TRUE)
max.estatura <- max(dados$estatura,na.rm=TRUE)
ve <- seq(min.estatura,max.estatura,length.out = 10000)
vd <- reg$coefficients[1,1] + reg$coefficients[2,1] * ve
elasticidade = reg$coefficients[2,1] * (ve/vd)

plot(ve,elasticidade,
     xlab="Estatura(cm)", ylab="Elasticidade(%)",
     lwd=2,type="l")

# elasticidade no centroide
m.ve <- mean(dados$estatura,na.rm=TRUE)
m.vd <- mean(dados$peso,na.rm=TRUE)
m.elasticidade = reg$coefficients[2,1] * (m.ve/m.vd)

cat("Elasticidade:
")
cat(" - minimo: ",min(elasticidade),"
",sep="")
cat(" - maximo: ",max(elasticidade),"
",sep="")
cat(" - medio: ",mean(elasticidade),"
",sep="")
cat(" - centroide: ",m.elasticidade,"
",sep="")

# # Há um dado claramente incorreto, peso de 658 kg; 
# # pode ser melhor eliminar (não afeta esta questão)
# dados$estatura[dados$estatura==658] <- NA
# # Há também uma pessoa com 1.2m e 120 quilos de peso
# # a estatura é possível, mas o mais provável é que tenham
# # digitado o peso de 120kg e a altura de 120cm por engano
# # pode ser melhor eliminar (não afeta esta questão)
# dados$peso[dados$peso==120 & dados$estatura==120] <- NA
# # note que usei peso e estatura, senão eliminarei outros
# # com peso de 120kg legitimamente 
# # (muda o gráfico mas nao afeta a resposta desta questao)
# 
# modelo <- lm(peso~estatura, data=dados, na.action = na.exclude)
# reg <- summary(modelo)
# print(reg)
# 
# min.estatura <- min(dados$estatura,na.rm=TRUE)
# max.estatura <- max(dados$estatura,na.rm=TRUE)
# ve <- seq(min.estatura,max.estatura,length.out = 10000)
# vd <- reg$coefficients[1,1] + reg$coefficients[2,1] * ve
# elasticidade = reg$coefficients[2,1] * (ve/vd)
# 
# plot(ve,elasticidade,
#      xlab="Estatura(cm)", ylab="Elasticidade(%)",
#      lwd=2,type="l")
# 
# # elasticidade no centroide
# m.ve <- mean(dados$estatura,na.rm=TRUE)
# m.vd <- mean(dados$peso,na.rm=TRUE)
# m.elasticidade = reg$coefficients[2,1] * (m.ve/m.vd)
# 
# cat("Elasticidade:
# ")
# cat(" - minimo: ",min(elasticidade),"
# ",sep="")
# cat(" - maximo: ",max(elasticidade),"
# ",sep="")
# cat(" - medio: ",mean(elasticidade),"
# ",sep="")
# cat(" - centroide: ",m.elasticidade,"
# ",sep="")