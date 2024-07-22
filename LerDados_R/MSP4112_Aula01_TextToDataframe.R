library(FSA)
library(lattice)
library(DescTools)
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# https://rcompanion.org 
 
Dados <- read.table(header=TRUE, 
                    stringsAsFactors=TRUE, 
                    text="
 Medico   Conceito
 A        3
 A        5
 A        4
 A        4
 A        4
 A        4
 A        4
 A        4
 A        5
 A        5
 B        2
 B        4
 B        2
 B        2
 B        1
 B        2
 B        3
 B        2
 B        2
 B        3
")
Dados$Conceito <- factor(Dados$Conceito, ordered=TRUE)
levels(Dados$Conceito)
class(Dados)
str(Dados)
print.data.frame(Dados)
summary(Dados)
FSA::Summarize(as.numeric(Conceito) ~ Medico,
               digits=3,
               data=Dados)
lattice::histogram(~ Conceito | Medico,
                   data=Dados,
                   aspect = "fill",
                   col="lightgray",
                   layout=c(1,2) #  columns and rows of individual plots
)

tabela <- xtabs(~ Medico + Conceito, data=Dados)
addmargins(tabela)
round(prop.table(tabela, margin=1), 2)
spineplot(tabela)
mosaicplot(tabela, shade=TRUE)
saveRDS(object=Dados, file="MediConceito.rds")
rm(Dados)
Dados <- readRDS(file="MediConceito.rds")
class(Dados)
str(Dados)

# Para este exemplo hipotético, os agricultores foram pesquisados sobre com que frequência eles utilizam uma determinada prática de manejo adequado. As respostas são organizadas de acordo com o tamanho da operação. Ambas as variáveis na tabela de contingência são categorias ordenadas.

tabela <- ("
                     Sempre 'AsVezes' Nunca
   Hobbista          0       1         5
   'Pequeno negócio' 2       3         4
   Pequeno           4       4         4
   Médio             3       2         0
   Grande            2       0         0
")

Dados.tabela <- as.matrix(read.table(textConnection(tabela), 
                                     header=TRUE, 
                                     row.names=1))
names(dimnames(Dados.tabela)) <- c("Tamanho", "Frequência")
Dados.tabela <- as.table(Dados.tabela)
addmargins(Dados.tabela)
str(Dados.tabela)
gplots::balloonplot(t(Dados.tabela), 
                    main ="Tabela de contingência\nDuas variáveis ordinais", 
                    xlab ="Freq prática manejo", 
                    ylab="Tamanho operação",
                    dotcolor="gray",
                    show.zeros=TRUE,
                    show.margins=TRUE)
Dados <- DescTools::Untable(Dados.tabela)
Dados$Tamanho <- factor(Dados$Tamanho, 
                        ordered=TRUE,
                        levels=c("Hobbista", "Pequeno negócio",
                                 "Pequeno", "Médio", "Grande"))
Dados$Frequência <- factor(Dados$Frequência, 
                           ordered=TRUE,
                           levels=c("Nunca", "AsVezes", "Sempre"))

str(Dados)
summary(Dados)
xtabs(~Tamanho+Frequência, data=Dados)

round(prop.table(Dados.tabela,
           margin=NULL),2)
spineplot(Dados.tabela)
mosaicplot(Dados.tabela, shade=TRUE)

DadosLista <- list(Dados.tabela, Dados)
names(DadosLista) <- c("Dados.tabela", "Dados")
saveRDS(object=DadosLista, file="PraticaManejoLista.rds")
rm(DadosLista)
DadosLista <- readRDS(file="PraticaManejoLista.rds")
DadosLista$Dados.tabela
DadosLista$Dados

# Tabela de contingência 2x2 estratificada  
# Teste robusto de Mantel-Haenszel de OR comum

# Relação entre tabagismo e sobrevivência em 20 anos (1974-1994) 
# em 1.134 mulheres adultas do Reino Unido
# Delineamento: coorte
# Fonte:  APPLETON, DR et al. (1996) Ignoring a covariate:
# An example of Simpson's paradox. The American Statistician,
# 50(4): 340-1.
# A aspa inicial TEM que começar na primeira coluna da linha
# e os espaçamentos distintos dos dessa tabela podem causar 
# problemas na geração da tabela horizontalizada (ftable).
tabela3D <- (
  "         FaixaEtaria 18-24 25-34 35-44 45-54 55-64 65-74 75+
Tabagista Desfecho 
Sim       Morta       2     3     14    27    51    29   13  
          Viva        53    121   95    103   64    7    0 
Nao       Morta       1     5     7     12    40    101  64
          Viva        61    152   114   66    81    28   0
")
Dados.tabela3D <- as.table(read.ftable(textConnection(tabela3D)))
ftable(Dados.tabela3D) # Display a flattened table (tabela horizontalizada)
mosaicplot(Dados.tabela3D, shade=TRUE)
saveRDS(object=Dados.tabela3D, file="Tabagismo3D.rds")
rm(Dados.tabela3D)
Dados.tabela3D <- readRDS(file="Tabagismo3D.rds")
Dados.tabela2D <- margin.table(Dados.tabela3D, margin=c(1,2))
addmargins(Dados.tabela2D)
Dados.tabela.margin <- addmargins(Dados.tabela3D, margin=c(1,2))
Dados.tabela.margin

DadosLista <- list(Dados.tabela3D, Dados.tabela2D)
names(DadosLista) <- c("Dados.tabela3D", "Dados.tabela2D")
saveRDS(object=DadosLista, file="DadosLista.rds")
rm(DadosLista)
tabelas <- readRDS(file="DadosLista.rds")
ftable(tabelas$Dados.tabela3D)
ftable(tabelas$Dados.tabela2D)

