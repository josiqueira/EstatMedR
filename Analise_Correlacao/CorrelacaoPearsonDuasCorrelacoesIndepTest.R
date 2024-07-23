Dados <- readxl::read_excel("Adm2008.xlsx")
B <- 1e4
rIC95 <- bootES::bootES(Dados[c("Estatura","MCT","Genero")],
                        group.col="Genero",
                        R=B)
print(rIC95) 
