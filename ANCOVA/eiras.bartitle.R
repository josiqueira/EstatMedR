# eiras.bartitle.R
#   to show text between bars  

bartitle <- function (text, level=1)
{
  newtext <- unlist(as.vector(strsplit(text,"\n")))
  length <- max(nchar(newtext))
  bar <- rep("-",length)
  
  l <- 1
  tab <- ""
  while (l<level)
  {
    tab <- paste(tab,"\t",sep="")
    l <- l+1
  }
  
  cat("\n",tab,bar,"\n",sep="")
  cat(tab,text,sep="")
  cat("\n",tab,bar,"\n",sep="")
}

