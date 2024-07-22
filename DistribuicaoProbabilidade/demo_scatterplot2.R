library(readxl)
suppressMessages(library(car, warn.conflicts = FALSE))
Dados <- readxl::read_excel("Adm2008.xlsx")
Dados$Genero <- as.factor(Dados$Genero)
car::scatterplot(MCT~Estatura|Genero, 
                       regLine=FALSE, smooth=FALSE, ellipse=FALSE, 
                       col=c("red","black"), 
                       jitter=list(x=1, y=1),
                       xlab = "Estatura (m)", 
                       ylab = "MCT (kg)", 
                       data=Dados)