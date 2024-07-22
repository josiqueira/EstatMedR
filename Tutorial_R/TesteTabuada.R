source ("tabuada.R")

cat("Chamando as tabuadas\n")
for (t in 0:10)
{
  m <- tabuada(t, echo=FALSE)
  cat("-Tabuada do ",t,":\n",sep="")
  cat("\t",as.character(m[1:nrow(m),4]),"\n")
}
