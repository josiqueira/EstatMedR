# Gestantes_matrizcorrelacoes.R

options(warn=-1)

Gestantes <- readRDS("Gestante.rds")
mc <- data.frame(Gestantes$IDADE, 
                 Gestantes$HT, 
                 Gestantes$HB, 
                 Gestantes$HEM, 
                 Gestantes$LEUC, 
                 Gestantes$FOLICO, 
                 Gestantes$B12)
names(mc) <- c("Idade","HT","HB","HEM","LEUC","FOLICO","B12")
print(head(mc))
cat("\n...\n")
print(tail(mc, addrownums = FALSE, n=2L))
cat("\nMatriz de correlacoes:\n")
print(cor(mc)) # matriz de correlacoes
# grafico da matriz
print(GGally::ggcorr(mc,
                     nbreaks = 6,
                     digits = 2,
                     label = TRUE,
                     label_size = 4,
                     color = "#888888"))


options(warn=0)
