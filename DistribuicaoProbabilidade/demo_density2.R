library(readxl)
suppressMessages(library(car, warn.conflicts = FALSE))
Dados <- readxl::read_excel("Adm2008.xlsx")
car::densityPlot(Estatura~factor(Genero), data=Dados, bw=bw.SJ, 
                 adjust=1, kernel=dnorm, method="adaptive")