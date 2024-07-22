tabuada <- function (multiplicando=1)
{
  cat("Apresentando a tabuada do ",multiplicando,":\n",sep="")
  for (n in 0:10) 
  {
    cat(n,"x",multiplicando,"=",n*multiplicando,"\n",sep=" | ")
  }
}