# eiras.bartitle.R
#   to show text between bars  

bartitle <- function (text)
{
  length <- nchar(text)
  bar <- rep("-",length)
  cat("\n",bar,"\n",sep="")
  cat(text)
  cat("\n",bar,"\n",sep="")
}

