# eiras.chisqgof.R
#   Goodness of fit
#   (teste de aderencia)
# Usage
# chisqof(dataframe, alpha=0.05, simulate.p.value = TRUE, B=1e4)
# Arguments
# dataframe         data frame with three columns:
#                   column 1:  labels (factors)
#                   column 2:  numeric vector of observed, absolute numbers
#                   column 3:  expected probabilities

source("eiras.bartitle.R")
source("eiras.cramerV.R")

chisqgof <- function (dataframe, alpha=0.05,
											simulate.p.value=TRUE, B=1e5)
{
  preference<-unlist(dataframe[,2])
  probs<-unlist(dataframe[,3])
  preference <- as.vector(unlist(preference))
  probs <- as.vector(unlist(probs))

  ################## core #####################
  if (simulate.p.value == TRUE)
  {
    chi2test <- chisq.test(preference, p=probs,
                           simulate.p.value=TRUE, B=B)
    kind.of.test <- "robust test (bootstrapping)"
  } else
  {
    # traditional test
    chi2test <- chisq.test(preference, p=probs,
                           correct=FALSE)
    kind.of.test <- "classical test"
  }
  #############################################

  L <- nrow(dataframe)
  df <- L-1
  chi2test$parameter <- df
  X2 <- chi2test$statistic
  p <- chi2test$p.value
  n <- sum(chi2test$observed)

  # results
  bartitle(paste("Goodness of Fit, ",kind.of.test,sep=""))
  bartitle("Data")
  prmatrix(dataframe, rowlab = rep(" ",nrow(dataframe)), quote=FALSE )

  bartitle("Analysis of Statistical Significance")
  cat("\n\tX^2 = ", X2 , "\tdf = ",  df, "\tp.value = ", p, "\n\n", sep="")
  if (simulate.p.value == TRUE)
  {
    cat("Chi-squared test for given probabilities with\n",
        "simulated p-value (based on ",B," replicates)\n", sep="")
  }

  cat("\nHeuristic for significance\n")
  cat("(rejection of H0 is expected if X^2/df > 2)\n")
  cat("\tcomputed X^2/df = ", X2/df, "\n", sep="")

  bartitle("Standardized residuals")
  residual <- matrix(data="", ncol=2, nrow=nrow(dataframe))
  for (i in 1:nrow(dataframe))
  {
    residual[i,1] <- as.character(dataframe[i,1])
    residual[i,2] <- chi2test$stdres[i]
  }
  residual[,2] <- round(as.numeric(residual[,2]),3) # round
  prmatrix(residual,
           collab = rep(" ",ncol(residual)),
           rowlab = rep(" ",nrow(residual)),
           quote=FALSE )

  # aplica o teste par a par
  bartitle("Post hoc analysis")
  if (chi2test$p.value >= alpha)
  {
    cat("\tThis analysis is not required\n",
        "\t(omnibus test has p=",chi2test$p.value,").\n",sep="")
    pares <- NA
  } else
  {
    # print(dataframe)
    pares <- RVAideMemoire::multinomial.multcomp(preference,
    																						 p.method = "bonferroni")
    colnames(pares$p.value) <- dataframe[1:(nrow(dataframe)-1),1]
    rownames(pares$p.value) <- dataframe[2:nrow(dataframe),1]
    print (pares)
    cat("\n")
    cat("\tPopulational inference, between:\n",sep="")
    for (r in 1:nrow(pares$p.value))
    {
      for (c in 1:ncol(pares$p.value))
      {
        if (!is.na(pares$p.value[r,c]))
        {
          cat("\t\t- ",rownames(pares$p.value)[r]," e ",colnames(pares$p.value)[c],": ",sep="")
          cat ("there is ")
          if (pares$p.value[r,c] >= alpha)
          {
            cat ("no ")
          }
          cat ("difference of proportion for ",names(dataframe)[1],"\n",sep="")
        }
      }
    }
  }

  bartitle("Analysis of Practical Significance")
  V <- cramerV(kind.of.test="chisqgof",statistic=X2, n=n, L=L)

  return(list(chi2test,pares,V))
}

