library(readxl)
suppressMessages(library(aplpack, warn.conflicts = FALSE))
Dados <- readxl::read_excel("Adm2008.xlsx")
head(Dados)
attach(Dados)
N <- min(sum(!is.na(Estatura[Genero=="Feminino"])), 
         MCT[Genero=="Feminino"])
aplpack::bagplot(Estatura[Genero=="Feminino"], 
                 MCT[Genero=="Feminino"],
                 main=paste("Estudantes femininas de Administração Noturno FEA-USP 2008", "\nN =",N),
                 xlab="Estatura (m)",
                 ylab="Massa Corporal Total (kg)",
                 na.rm = TRUE)