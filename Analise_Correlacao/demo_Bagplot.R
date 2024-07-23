Dados <- readxl::read_excel("Adm2008.xlsx")
Dados.F <- subset(Dados, Genero=="Feminino")
Dados.M <- subset(Dados, Genero=="Masculino")
n.F <- min(sum(!is.na(Dados.F$Estatura)), 
           sum(!is.na(Dados.F$MCT)))
n.M <- min(sum(!is.na(Dados.M$Estatura)), 
           sum(!is.na(Dados.M$MCT)))
bgp.F <- DescTools::PlotBag(Dados.F$Estatura, 
                            Dados.F$MCT, 
                            main=paste0("Feminino"," n=",n.F),
                            xlab="Estatura (m)",
                            ylab="Massa Corporal Total (kg)",
                            na.rm = TRUE,
                            show.bagpoints=FALSE,
                            show.looppoints=FALSE,
                            show.whiskers=FALSE,
                            col.loophull = "white",
                            col.looppoints = "black", 
                            col.baghull = "white",
                            col.bagpoints = "black",
                            cex=1)
print(outliers.F <- as.data.frame(bgp.F$pxy.outlier))
for (o in 1:nrow(outliers.F))
{
  r.F <- which(Dados.F$Estatura==outliers.F$x[o] & 
                 Dados.F$MCT==outliers.F$y[o])
  text(outliers.F$x[o],outliers.F$y[o], r.F, pos=1, cex=0.7)
}
bgp.M <- DescTools::PlotBag(Dados.M$Estatura, 
                            Dados.M$MCT,
                            main=paste0("Masculino"," n=",n.M),
                            xlab="Estatura (m)",
                            ylab="Massa Corporal Total (kg)",
                            na.rm = TRUE,
                            show.bagpoints=FALSE,
                            show.looppoints=FALSE,
                            show.whiskers=FALSE,
                            col.loophull = "white",
                            col.looppoints = "black", 
                            col.baghull = "white",
                            col.bagpoints = "black",
                            cex=1)
print(outliers.M <- as.data.frame(bgp.M$pxy.outlier))
if(nrow(outliers.M)>0){
for (o in 1:nrow(outliers.M)){
  r.M <- which(Dados.M$Estatura==outliers.M$x[o] & 
                 Dados.M$MCT==outliers.M$y[o])
  text(outliers.M$x[o], outliers.M$y[o], r.M, pos=1, cex=0.7)
} }
if(nrow(outliers.M)<=0){cat("Sem outlier bidimensional")}
