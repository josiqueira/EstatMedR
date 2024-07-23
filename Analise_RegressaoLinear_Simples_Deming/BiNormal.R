# Simulacao de distribuicao binormal padrao entre duas variaveis hipoteticas 
suppressMessages(library(MASS, warn.conflicts = FALSE))

binormal <- function (avg1=0, avg2=0, 
                      sd1=1, sd2=1, 
                      n=100, correl=0.5, 
                      pathname=NA, standardize=FALSE,
                      plot="n", axes=TRUE)
{
  if (is.na(pathname))
  {
    pathname <- getwd()
  }
  cov <- correl*sd1*sd2
  # covariance matrix
  covar <- matrix(c(sd1^2, cov, cov, sd2^2), 2) # Covariance matrix
  xy <- MASS::mvrnorm(n, mu = c(avg1, avg2), Sigma = covar)
  dados <- as.data.frame(xy)
  names(dados) <- c("X", "Y")
  if (standardize==TRUE)
  {
    # padronizacao
    dados$X <- scale(dados$X)
    dados$Y <- scale(dados$Y)
  }
  # correlacao
  r.Pearson <- with(dados, cor.test(X, Y, 
                                    method="pearson",
                                    exact = TRUE, 
                                    alternative="two.sided",
                                    na.rm=TRUE)$estimate)
  if (plot != "n")
  {
    output <- paste(pathname,"/output_",correl,sep="")
    if (is.na(pathname))
    {
      output <- paste(output,".png",sep="")
      cat(output,"\n")
      png(output, width = 350, height = 400)
    }
    if (axes==TRUE)
    {
      main <- paste("n = ", n, ", r = ", round(r.Pearson,3), sep="")
      xlab <- "X"
      ylab <- "Y"
    } else
    {
      main <- NA
      xlab <- ""
      ylab <- ""
    }
    plot(dados,
         main=main,
         xlab=xlab,
         ylab=ylab,
         axes=axes,
         # xlim=c(min(dados$X),max(dados$X)),
         # ylim=c(min(dados$Y),max(dados$Y)),
         xlim=c(20,100),
         ylim=c(20,100),
         pch=21, col="#00000088", bg="#6195CF99")
    if (is.na(pathname))
    {
      dev.off()
    }
  }
  return (list(r.Pearson, dados))
}

s <- round(runif(1,1,10000))
cat(s,"\n")
# s <- 888
set.seed(s)

binormal (avg1=70, avg2=70, 
          sd1=10, sd2=10, 
          n=20, correl=0.9, plot="y")
  
binormal (avg1=70, avg2=75, 
          sd1=13, sd2=10, 
          n=20, correl=0.9, plot="y")
