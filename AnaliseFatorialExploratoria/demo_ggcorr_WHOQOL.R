Dados <- readRDS("WHOQOL.rds")

r <- qgraph::cor_auto(Dados[,4:27], detectOrdinal=FALSE, 
                      forcePD=TRUE, missing="fiml")
print (
  GGally::ggcorr(data=NULL,
                 name="WHOQOL-BREF",
                 cor_matrix=r,
                 geom="tile",
                 min_size=0,
                 max_size=10, 
                 nbreaks=6,
                 digits=2,
                 label=FALSE,
                 label_round=2,
                 label_size=4)
)

