# eiras.tukey.try.R
# dado um vetor de dados, rastreia transformacoes lambda de Tukey
# indo de -3 a +3 em passos de 0.5
# relata o valor que minimiza o coeficiente de assimetria de Pearson
# show = c("all","final","no")

suppressMessages(library(lawstat, warn.conflicts = FALSE))

source("eiras.bartitle.R")
source("eiras.density_and_normal.R")

tukey.try <- function(values, B=0, xlab="values", alpha=0.05, show="no")
{
  colnames <- c("power","asymmetry","p.value","p.txt","equation")
  if (B<0) {B <- 0}
  if (B>0)
  {
    colnames <- c(colnames,"B")
  }
  dt_tukey <- data.frame(matrix(nrow=0,ncol=length(colnames)))
  names(dt_tukey) <- colnames
  
  # transformacoes power = [-2.2]
  for (lambda in seq(from=-3, to=3, by=0.5))
  {
    lambda <- round(lambda,1)
    if(lambda<0)
    {
      values_tukey <- -1/(values^abs(lambda))
      eqtext <- paste("-1/(",xlab,"^",abs(lambda),")",sep="")
    }
    if (lambda==0)
    {
      values_tukey <- log(values)
      eqtext <- paste("ln[",xlab,"]",sep="")
    }
    if(lambda>0)
    {
      values_tukey <- values^lambda
      if (lambda==1)
      {
        eqtext <- paste(xlab,sep="")
      } else
      {
        eqtext <- paste(xlab,"^",lambda,sep="")
      }
    }
    values_tukey <- values_tukey[is.finite(values_tukey)]
    if (show == "all")
    {
      cat(bartitle(paste("Tukey's lambda = ",lambda,sep="")))
      titulo <- paste("Tukey's transformation: ",eqtext,sep="")    
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
    } else
    {
      symtest <- lawstat::symmetry.test(values_tukey, boot=FALSE)    
      sym.p <- symtest$p.value 
    }
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
      cat ("\t",sym.p.txt,"\n",sep="")
    }
    
    # adiciona a assimetria
    if (show == "all")
    {
      titulo <- paste(titulo,"\nAsymmetry = ",round(assimetria,4), sep="")
      if (nchar(sym.p.txt)>0) {titulo <- paste(titulo,", ", sym.p.txt,sep="")}
      density_and_normal(values_tukey, 
                         main=titulo,
                         col="black",
                         xlab=eqtext, ylab="density")
    }
    dt_tmp <- data.frame(lambda,assimetria,sym.p,sym.p.txt,eqtext)
    if(B>0)
    {
      dt_tmp <- data.frame(lambda,assimetria,sym.p,sym.p.txt,eqtext,B)
    }
    names(dt_tmp) <- colnames
    dt_tukey <- rbind(dt_tukey,dt_tmp)
  } # lambda
  
  # minimo encontrado
  dt_best <- dt_tukey[dt_tukey$p.value>=0.05,]
  if(nrow(dt_best)==0)
  {
    pmax <- max(dt_tukey$p.value)
    dt_best <- dt_tukey[dt_tukey$p.value>=pmax,]
  }
  if(nrow(dt_best)>1)
  {
    min_assimetria <- min(abs(dt_best$asymmetry))
    dt_best <- dt_best[abs(dt_best$asymmetry)==min_assimetria,]
  }
  if(nrow(dt_best)>1)
  {
    dt_best <- dt_best[nrow(dt_best),]
  }
  if (show == "final" | show =="all")
  {
    cat(bartitle("Best transformation"))
    
    cat(paste("Tukey's lambda = ",dt_best$power,"\n",sep=""))
    if (dt_best$power == 0)
    {
      cat("\t(logarithm transformation)\n")
    }
    cat(paste("Asymmetry coefficient = ",dt_best$asymmetry,"\n",sep=""))
    cat ("\t",dt_best$p.txt,"\n",sep="")
    cat(paste("equation: ",dt_best$equation,"\n",sep=""))
    titulo <- paste("Tukey's best transformation: ",dt_best$equation,sep="")    
    if (dt_best$power==1)
    {
      titulo = "Original values"
    } 
    titulo <- paste(titulo,"\nAsymmetry = ",round(dt_best$asymmetry,4), sep="")
    if (nchar(dt_best$p.txt)>0) {titulo <- paste(titulo,", ", dt_best$p.txt,sep="")}
    
    lambda <- round(lambda,1)
    if(lambda<0)
    {
      values_tukey <- -1/(values^abs(lambda))
      eqtext <- paste("-1/(",xlab,"^",abs(lambda),")",sep="")
    }
    if (lambda==0)
    {
      values_tukey <- log(values)
      eqtext <- paste("ln[",xlab,"]",sep="")
    }
    if(lambda>0)
    {
      values_tukey <- values^lambda
      if (lambda==1)
      {
        eqtext <- paste(xlab,sep="")
      } else
      {
        eqtext <- paste(xlab,"^",lambda,sep="")
      }
    }
    lambda <- round(dt_best$power,1)
    if(lambda<0)
    {
      values_tukey_best <- -1/(values^abs(lambda))
    }
    if (lambda==0)
    {
      values_tukey_best <- log(values)
    }
    if(lambda>0)
    {
      values_tukey_best <- values^lambda
    }
    values_tukey_best <- values_tukey_best[is.finite(values_tukey_best)]

    # exibe o grafico
    density_and_normal(values_tukey_best,
                             main=titulo,
                             col="black",
                             xlab=dt_best$equation, ylab="density")
    
  }
  values <- values_tukey_best
  
  return <- list(dt_best,dt_tukey,values)
  
  return (return)
}
