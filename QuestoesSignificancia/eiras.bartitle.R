# eiras.bartitle.R
#   to show text between bars  

bartitle <- function (text, show="n")
{
  length <- nchar(text)
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
  return(output)
}

