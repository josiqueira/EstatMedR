source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05

cat(bartitle("Regressão Linear Simples"))

car::scatterplot(ErrosDirecao~Experiencia|Grupo, 
                 data=Dados,
                 regLine=TRUE, smooth=FALSE, col=c("black","blue","red"))

graf <- ggplot2::ggplot(Dados, 
                        ggplot2::aes(y=ErrosDirecao, 
                                     x=Experiencia, 
                                     group=Grupo, 
                                     linetype=Grupo,
                                     shape=Grupo))+ 
  ggplot2::geom_point(alpha=1/4,
                      size=1)+ 
  ggplot2::theme_classic()+
  ggplot2::labs(title="RLS por Grupo",
                subtitle=paste0("BC",round((1-alfa/
                                              length(unique(Dados$Grupo)))*
                                             100,2),"%"),
                x="Experiência (ano)", 
                y="#ErrosDirecao")+
  ggplot2::geom_smooth(method=MASS::rlm,
                       na.rm=TRUE,
                       color="black",
                       level = 1-alfa/length(unique(Dados$Grupo)))
plot(graf)
for (g in unique(Dados$Grupo))
{
  cat(bartitle(g,2))
  dt_tmp <- subset(Dados, Grupo==g)
  modelo <- lm(ErrosDirecao~Experiencia, data=dt_tmp)
  print(summary(modelo))
}
