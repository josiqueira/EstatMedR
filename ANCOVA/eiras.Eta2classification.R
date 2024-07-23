# eiras.Eta2classification.R
Eta2classification <- function(eta2, show = TRUE)
{
  if (eta2<0 | eta2>1)
  {
    cat("\nWarning: Eta^2 valid range is [0,1]\n")
    faixa_txt <- NA
    level <- 1
  } else
  {
    faixa_txt = c(
      "negligible", 
      "small", 
      "medium", 
      "large" 
    )
    faixa_lim = c(
      0,
      0.01, 
      0.06, 
      0.14)
    level <- NA
    if(eta2>=max(faixa_lim)){level <- length(faixa_lim)} 
    if (is.na(level))
    {
      for (f in 2:length(faixa_lim))
      {
        if (eta2>=faixa_lim[f-1]&eta2<faixa_lim[f]) {level=f-1}
      }
    }
    if (show == TRUE)
    {
      cat("\n\tEta^2 = ", eta2,"\n",sep="")
      cat("\t",faixa_txt[level]," effect size\n", sep="")
    }
  }
  return(faixa_txt[level])
}
