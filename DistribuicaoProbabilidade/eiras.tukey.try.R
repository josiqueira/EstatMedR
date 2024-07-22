# eiras.tukey.try.R
# dado um vetor de dados, rastreia transformacoes lambda de Tukey
# indo de -3 a +3 em passos de 0.5
# relata o valor que minimiza o coeficiente de assimetria de Pearson
# show = c("all","final","no")

library(lawstat)

source("eiras.bartitle.R")
source("eiras.density_and_normal.R")

tukey.try <- function(values, B=0, xlab="values", show="no")
{
  eqpower <- c()
  eqtext <- c()
  # transformacoes power = [-2.2]
  min_assimetria <- NA
  min_lambda <- NA
  idx <- 0
  sym.p.best.txt <- ""    
  sym.p.best <- NA    
  equation <- ""
  for (lambda in seq(from=-3, to=3, by=0.5))
  {
    idx <- idx+1
    
    lambda <- round(lambda,1)
    if(lambda<0)
    {
      values_tukey <- -1/(values^abs(lambda))
      eqtext <- c(eqtext, paste("-1/(",xlab,"^",abs(lambda),")",sep=""))
    }
    if (lambda==0)
    {
      values_tukey <- log(values)
      eqtext <- c(eqtext, paste("ln[",xlab,"]",sep=""))
    }
    if(lambda>0)
    {
      values_tukey <- values^lambda
      if (lambda==1)
      {
        eqtext <- c(eqtext, paste(xlab,sep=""))
      } else
      {
        eqtext <- c(eqtext, paste(xlab,"^",lambda,sep=""))
      }
    }
    eqpower <- c(eqpower, lambda)
    values_tukey <- values_tukey[is.finite(values_tukey)]
    if (show == "all")
    {
      cat(bartitle(paste("Tukey's lambda = ",lambda,sep="")))
      label_x <- eqtext[length(eqtext)]
      titulo <- paste("Tukey's transformation: ",label_x,sep="")    
      if (lambda==1)
      {
        titulo = "Original values"
      } 
    }
    # calcula
    mean <- mean(values_tukey, na.rm = TRUE)
    stdev <- sd(values_tukey, na.rm = TRUE)
    # quartis
    median <- median(values_tukey, na.rm = TRUE)
    # moda
    d <- density(values_tukey)
    mode <- d$x[which.max(d$y)]
    # intervalo IQ
    iqr <- IQR(values_tukey, na.rm=TRUE)
    # teste de simetria
    sym.p.txt <- ""
    if (B>0)
    {
      symtest <- lawstat::symmetry.test(values_tukey, B=B)    
      sym.p <- symtest$p.value 
      if (sym.p<0.0001)
      {
        if (sym.p<1e-18)
        {
          sym.p.txt <- "p < 1e-18"
        } else
        {
          sym.p.txt <- sprintf("p = %.2e",sym.p)
        }
      } else
      {
        sym.p.txt <- sprintf("p = %.4f",sym.p)
      }
    }
    if (show == "all")
    {
      cat ("mean:",mean,"\n")
      cat ("st.dev.:",stdev,"\n")
      cat ("median:",median,"\n")
      cat ("mode:",mode,"\n")
      cat ("iqr:",iqr,"\n")
    }
    # assimetria de Pearson
    # assim.Pearson <- 3 * (mean - median) / stdev
    # assimetria de Siqueira
    assimetria <- (mode - mean) / iqr
    if (show == "all")
    {
      cat ("Asymmetry coefficient:",assimetria,"\n")
      if (B>0)
      {
        cat ("Statistical symmetry test, p-value: ")
        if(sym.p==0) {cat("< 1e-18")} else
        {cat(sym.p.txt)}
        cat("\n")
      }
    }
    if (is.na(min_assimetria) || min_assimetria > abs(assimetria))
    {
      min_assimetria <- abs(assimetria)
      min_lambda <- lambda
      values_tukey_best <- values_tukey
      idx_best <- idx
      if (B>0)
      {
        sym.p.best <- sym.p
        sym.p.best.txt <- sym.p.txt
      }
    }
    
    # adiciona a assimetria
    if (show == "all")
    {
      titulo <- paste(titulo,"\nAsymmetry = ",round(assimetria,4), sep="")
      if (nchar(sym.p.txt)>0) {titulo <- paste(titulo,", ", sym.p.txt,sep="")}
      density_and_normal(values_tukey, 
                         main=titulo,
                         col="black",
                         xlab=label_x, ylab="density")
    }
  }
  
  # relata o minimo encontrado
  if (show == "final" | show =="all")
  {
    cat(bartitle("Best transformation"))
    cat(paste("Tukey's lambda = ",min_lambda,"\n",sep=""))
    if (min_lambda == 0)
    {
      cat("\t(logarithm transformation)\n")
    }
    cat(paste("Asymmetry coefficient = ",min_assimetria,"\n",sep=""))
    if (B>0)
    {
      cat ("Statistical symmetry test, p-value: ")
      if(sym.p.best==0) {cat("< 1e-18")} else
      {cat(sym.p.best)}
      cat("\n")
    }
    cat(paste("eqtext: ",eqtext[idx_best],"\n",sep=""))
    label_x <- eqtext[idx_best]
    titulo <- paste("Tukey's best transformation: ",label_x,sep="")    
    if (min_lambda==1)
    {
      titulo = "Original values"
    } 
    titulo <- paste(titulo,"\nAsymmetry = ",round(min_assimetria,4), sep="")
    if (nchar(sym.p.best.txt)>0) {titulo <- paste(titulo,", ", sym.p.best.txt,sep="")}
    
    # exibe o grafico
    density_and_normal(values_tukey_best,
                             main=titulo,
                             col="black",
                             xlab=label_x, ylab="density")
    
  }
  equation <- eqtext[which(eqpower==min_lambda)]
  equation <- data.frame(min_lambda,min_assimetria,sym.p.best,equation)
  colnames(equation) <- c("power","asymmetry","p.value","equation")
  values <- values_tukey_best
  
  return <- list(equation,values)
  
  return (return)
}
