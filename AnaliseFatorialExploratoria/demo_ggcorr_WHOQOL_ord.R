Dados <- readRDS("WHOQOL.rds")

ro <- qgraph::cor_auto(Dados[,4:27], detectOrdinal=TRUE, 
                       forcePD=TRUE, missing="pairwise")
print (
  GGally::ggcorr(data=NULL,
                 name="WHOQOL-BREF",
                 cor_matrix=ro,
                 geom="tile",
                 min_size=0,
                 max_size=10, 
                 nbreaks=6,
                 digits=2,
                 label=FALSE,
                 label_round=2,
                 label_size=4)
)
