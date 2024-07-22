library(readxl)
suppressMessages(library(car, warn.conflicts = FALSE))
Dados <- readxl::read_excel("Adm2008.xlsx")
N <- min(sum(!is.na(Dados$Estatura[Dados$Genero=="Feminino"])), 
         Dados$MCT[Dados$Genero=="Feminino"])
car::scatterplot(MCT[Genero=="Feminino"] ~ 
                         Estatura[Genero=="Feminino"], 
                       regLine=FALSE, smooth=FALSE, boxplots=FALSE, 
                       jitter=list(x=1, y=1), col="black",
                       main=paste("Estudantes femininas de Administração 2008", 
                                  "\nN =",N), 
                       xlab="Estatura (cm)", 
                       ylab=" Massa Corporal Total (kg)",
                       data=Dados)
