# alfa <- 0.05
# 
# Dados <- readxl::read_excel("Enxaqueca.xlsx")
# Dados$Grupo <- as.factor(Dados$Grupo)
# print(xtabs( ~ Grupo + Sintoma2, data = Dados))
# print(lattice::histogram(~ Sintoma2 | Grupo, 
#                          data=Dados,
#                          layout=c(1,3)))
# 
# cat("\nTeste H de Kruskal-Wallis Convencional:\n")
# print(with(Dados, kruskal.test(Sintoma2,Grupo)))
# Sum <- rcompanion::groupwiseMedian(Sintoma2~Grupo,
#                                    data = Dados,
#                                    conf = 1-alfa,
#                                    boot = TRUE,
#                                    R = 1e4,
#                                    percentile = TRUE,
#                                    bca= FALSE,
#                                    digits = 3)
# X <- 1:3
# Y <- Sum$Percentile.upper + 0.2
# # sumario dos resultados
# cat("\n")
# print(Sum)
# cat("\n")
# # grafico dos intervalos
# Label <- as.character(Sum$Grupo)
# print(
#   ggplot2::ggplot(Sum,                
#                   ggplot2::aes(x = Grupo,
#                                y = Median)) +
#     ggplot2::geom_errorbar(ggplot2::aes(ymin = Percentile.lower,
#                                         ymax = Percentile.upper),
#                            width = 0.05,
#                            linewidth = 0.5) +
#     ggplot2::geom_point(shape = 15) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(axis.title=ggplot2::element_text(face="bold")) +
#     ggplot2::ylab("Median score") +
#     ggplot2::annotate("text",
#                       x = X,
#                       y = Y,
#                       label = Label)
# )
# 
# # versao bootstrapping 
# cat("\nTeste H de Kruskal-Wallis (bootstrapping):\n")
# print(coin::kruskal_test(Sintoma2~Grupo, 
#                          data=Dados,
#                          distribution=coin::approximate(nresample=1e6)))
# 

cat("\npost hoc test:\n\n")

print(PMCMRplus::kwAllPairsDunnTest(Sintoma2~Grupo,
                              data=Dados,
                              p.adjust.method="bonferroni"))

dt <- FSA::dunnTest(Sintoma2~Grupo,
                    data=Dados,
                    method="bonferroni")
pt <- dt$res
print(dt)
try(print(rcompanion::cldList(P.adj ~ Comparison, 
                              data = pt, 
                              threshold = 0.05)))

print(ggstatsplot::ggbetweenstats(
  data = Dados,
  x = Grupo,
  y = Sintoma2,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  nboot = 100L,
  conf.level = 1-alfa,
  p.adjust.method = "bonferroni",
  effsize.type = "unbiased",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  violin.args = list(width = 0, linewidth = 0),
))
