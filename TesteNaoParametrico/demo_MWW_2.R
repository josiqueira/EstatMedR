o.opt <- options(warn=-1)

ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
# monta dataframe
dt_treino <- data.frame(
  c(rep("Com",length(ComTreino)),
    rep("Sem",length(SemTreino))),
  c(ComTreino,SemTreino)
)
names(dt_treino) <- c("Treino","Simpatia")
dt_treino$Treino <- factor(dt_treino$Treino)

# descritiva (barplots
cat("\nDados:\n")
print(dt_treino)
o.par <- par()
m <- matrix(data=1:2,ncol=2,nrow=1,byrow=TRUE)
layout(m)
levs <- 1:5
v <- dt_treino$Simpatia[dt_treino$Treino=="Com"]
t <- table(factor(v, levs))
barplot(t,names.arg = names(t),
        main="Com treino", xlab="Simpatia", ylab="Ocorrencias",
        ylim = c(0,6))
v <- dt_treino$Simpatia[dt_treino$Treino=="Sem"]
t <- table(factor(v, levs))
barplot(t,names.arg = names(t),
        main="Sem treino", xlab="Simpatia", ylab="Ocorrencias")
par(o.par)

# IC95%
cat("\nIntervalos de confiança 95%:")
alfa <- 0.05
Sum <- rcompanion::groupwiseMedian(Simpatia~Treino,
                                   data = dt_treino,
                                   conf = 1-alfa,
                                   boot = TRUE,
                                   R = 1e4,
                                   percentile = TRUE,
                                   bca= FALSE,
                                   digits = 3)
X <- 1:length(unique(dt_treino$Treino))
Y <- Sum$Percentile.upper + 0.2
# sumario dos resultados
cat("\n")
print(Sum)
cat("\n")
# grafico dos intervalos
Label <- as.character(Sum$Treino)
print(
  ggplot2::ggplot(Sum,                
                  ggplot2::aes(x = Treino,
                      y = Median)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Percentile.lower,
                      ymax = Percentile.upper),
                  width = 0.05,
                  linewidth = 0.5) +
    ggplot2::geom_point(shape = 15) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = ggplot2::element_text(face = "bold")) +
    ggplot2::ylab("Median score") +
    ggplot2::annotate("text",
             x = X,
             y = Y,
             label = Label)
)

options(o.opt)
