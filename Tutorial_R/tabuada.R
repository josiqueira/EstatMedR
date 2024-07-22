tabuada <- function (multiplicando=1, echo=TRUE)
{
  executar <- 1
  if (!is.numeric(multiplicando))
  {
    cat("Por favor use um número\n")
    executar <- 0
  } 
  if (executar == 1 & (multiplicando<0 | multiplicando>10))
  {
    cat("Somente números de 0 a 10 sao permitidos\n")
    executar <- 0
  } 

  if (executar == 1)
  {
    if (echo)
    {
      cat("Tabuada do ",multiplicando,"\n",sep="")
    }  
    matriz <- matrix(data=NA, ncol=4, nrow=11)
    colnames(matriz) <- c("multiplicando", "multiplicado", "produto", "texto")
    for (m in seq(from=0,to=10,by=1))
    {
      produto <- m*multiplicando
      matriz[m+1,1] <- m
      matriz[m+1,2] <- multiplicando
      matriz[m+1,3] <- produto
      matriz[m+1,4] <- paste(m,"x",multiplicando,"=",produto,sep="")
      if (echo)
      {
        cat(matriz[m+1,4],"\n")
      }
    }
    return(matriz)    
  }
  

}