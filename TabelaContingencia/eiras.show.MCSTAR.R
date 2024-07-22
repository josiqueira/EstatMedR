show.MCSTAR <- function (MCSTAR, alpha, df)
{
  cat("\nResiduos ajustados standardizados corrigidos por momento (MCSTAR):\n")
  alphaBonf <- alpha/df
  zcrit <- abs(qnorm(alphaBonf/2))
  cat("\n|MCSTAR critico| (alphaBonferroni=",round(alpha*100,0),"%/",df,") = ",zcrit,"\n\n", sep="")
  
  MCSTARtxt <- round(MCSTAR,2)
  for (r in 1:nrow(MCSTAR))
  {
    for (c in 1:ncol(MCSTAR))
    {
      if (MCSTAR[r,c] > zcrit)
      {
        MCSTARtxt[r,c] <- paste("#",MCSTARtxt[r,c],sep="")
      } else
      {
        MCSTARtxt[r,c] <- paste(" ",MCSTARtxt[r,c],sep="")
      }
    }
  }
  prmatrix(MCSTARtxt, quote=FALSE, right=TRUE)
  pSTAR <- (1-pnorm(abs(MCSTAR)))*2
  pSTAR <- abs(MCSTAR)<abs(zcrit)
  pSTAR <- pSTAR*1
  if(prod(pSTAR)!=1)
  {
    corrplot::corrplot(MCSTAR, is.corr = FALSE, 
                       insig="label_sig",
                       p.mat=pSTAR,
                       pch="*", 
                       pch.cex=5, 
                       pch.col="yellow")  
  } else
  {
    pSTAR <- pSTAR*0
    corrplot::corrplot(MCSTAR, is.corr = FALSE, 
                       p.mat=pSTAR)  
  }
  
  return(MCSTARtxt)
}

