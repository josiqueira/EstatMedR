Dados <- readRDS("WHOQOL.rds")

ro <- qgraph::cor_auto(Dados[,4:27], detectOrdinal=TRUE, 
                       forcePD=TRUE, missing="pairwise")
n <- nrow(na.omit(Dados))

qgraph::qgraph(ro, 
               graph="glasso", 
               sampleSize=n,
               layout="spring",
               shape="rectangle",
               vsize=5,
               label.cex=2,
               labels=colnames(ro),
               label.prop=0,
               theme="gray",
               title="WHOQOL (glasso)", 
               details=FALSE)

qgraph::qgraph(ro, 
               graph="cor", 
               sampleSize=n,
               #minimum="sig",
               bonf=TRUE,
               layout="spring", 
               shape="rectangle",
               vsize=5,
               label.cex=2,
               labels=colnames(ro),
               label.prop=0,
               theme="gray",
               title="WHOQOL (cor)",
               details=FALSE)
