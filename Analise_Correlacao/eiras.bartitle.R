# eiras.bartitle.R
#   to show text between bars  

bartitle <- function (text, show="n")
{
  output <- ""
  length <- nchar(text)
  if (length>0)
  {
    bar <- ""
    for (l in 1:length)
    {
      bar <- paste(bar,"-",sep="")
    }
    output <- paste("\n",bar,"\n",
                    text,
                    "\n",bar,"\n",sep="")
    if (show=="y")
    {
      print(output)
    }
  }
  return(output)
}

