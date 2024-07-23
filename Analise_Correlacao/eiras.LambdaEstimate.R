# eiras.LambdaEstimate.R

source("eiras.findcommonchars.R")

LambdaEstimate <- function(data, 
                           reference.cols,
                           newmethod.cols)
{
  df_resultado <- matrix (ncol=1, nrow=2)
  rownames(df_resultado) <- c("lambda","status")
  df_avgdata <- data.frame(matrix(ncol=2, nrow=nrow(data)))
  colnames(df_avgdata) <- c("reference","new")
  
  # if there is only two columns
  if (length(reference.cols)==1 & length(newmethod.cols)==1)
  {
    c1 <- reference.cols[1]
    c2 <- newmethod.cols[1]
    df_resultado[2,1] <- paste("Assuming:\n",
                               "\t- Explanatory variable: ",colnames(data)[c1],";\n",
                               "\t- Dependent variable: ",colnames(data)[c2],".\n"
                 ,sep="")
    colnames(df_avgdata) <- c(colnames(data)[c1],colnames(data)[c2])
    df_avgdata[,1] <- data[,c1]
    df_avgdata[,2] <- data[,c2]
    x <- data[,c2] # new method
    y <- data[,c1] # reference method
    n <- nrow(data)
    
    
    # remove pairs with NA
    df_tmp <- data.frame(x,y)
    names(df_tmp) <- c("x","y")
    df_tmp <- df_tmp[!is.na(df_tmp$x),]
    df_tmp <- df_tmp[!is.na(df_tmp$y),]
    x <- df_tmp$x
    y <- df_tmp$y
    # Estimacao do IC95% do lambda sem medidas repetidas 
    # Medida única ou para a média das repeticoes
    # A ordem de x e y importa
    # Shukla (1973), p. 376.
    a <- var(x,na.rm=TRUE) - cov(x,y)
    b <- var(y,na.rm=TRUE) - cov(x,y)
    gl <- n - 2
    alpha <- 0.05
    t <- qt(1 - alpha/2, gl)
    P <- (t^2)*(var(x)*var(y) - cov(x,y)^2)/(n - 2)
    LI <- abs((b - sqrt(P))/(a + sqrt(P)))
    LS <- abs((b + sqrt(P))/(a - sqrt(P)))
    # cat("IC95%(lambda) = [", round(LI,4), ",", round(LS,4),"]\n")
    # cat("Estimativa pontual de lambda = ",round((LI + LS)/2,4)) # lambda = 1.545    
    df_resultado[1,1] <- (LI + LS)/2
  }
  
  
  if (length(reference.cols)>1 | length(newmethod.cols)>1)
  {
    firsthalf <- paste(colnames(data)[reference.cols])      
    secondhalf <- paste(colnames(data)[newmethod.cols])
    # find names to two column sets
    nomeref <- findcommonchars(firsthalf)
    nomeref.concatenate <- ""
    comma <- ""
    for (i in reference.cols)
    {
      nomeref.concatenate <- paste(nomeref.concatenate,comma,colnames(data)[i],sep="")
      comma <- ", "
    }
    nomenew <- findcommonchars(secondhalf)
    nomenew.concatenate <- ""
    comma <- ""
    for (i in newmethod.cols)
    {
      nomenew.concatenate <- paste(nomenew.concatenate,comma,colnames(data)[i],sep="")
      comma <- ", "
    }
    if (nomeref==nomenew)
    {
      nomeref <- paste("ref",nomeref,sep="")
      nomenew <- paste("new",nomenew,sep="")
    }
    df_resultado[2,1] <- paste("Assuming repeated measures per method:\n",
                               "\t- reference method (",length(reference.cols),"): ",nomeref," (",nomeref.concatenate,");\n",
                               "\t- new method (",length(newmethod.cols),"): ",nomenew," (",nomenew.concatenate,").\n"
                               ,sep="")
    # rename columns
    colnames(df_avgdata) <- c(nomeref,nomenew)
    for (r in 1:nrow(data))
    {
      df_avgdata[r,1] <- mean(as.numeric(data[r,reference.cols]), na.rm=TRUE)
      df_avgdata[r,2] <- mean(as.numeric(data[r,newmethod.cols]), na.rm=TRUE)
    }
    
    # Estimacao pontual do lambda para medidas repetidas usada na Regressao de Deming
    # A ordem de x e y importa!
    # Chapter 303 do NCSS 11 (2016): Deming regression
    reference <- df_avgdata[,1]
    referenceg <- mean(reference, na.rm=TRUE)
    new <- df_avgdata[,2]
    newg <- mean(new, na.rm=TRUE)
    n <- nrow(data)
    VARdelta <- 0
    for (c in reference.cols)
    {
      VARdelta <- VARdelta + sum((data[,c] - df_avgdata[,1])^2, na.rm=TRUE)
    }
    VARdelta <- VARdelta/n
    VARepsilon <- 0
    for (c in newmethod.cols)
    {
      VARepsilon <- VARepsilon + sum((data[,c] - df_avgdata[,2])^2, na.rm=TRUE)
    }
    VARepsilon <- VARepsilon/n
    # lambda <- VARepsilon/VARdelta # lambda = 1.692
    df_resultado[1,1] <- (VARepsilon*length(reference.cols))/(VARdelta*length(newmethod.cols))
  }

  return (list(df_avgdata, df_resultado))
}


# # check the number os columns
# numcols <- ncol(data)
# df_resultado <- matrix (ncol=1, nrow=2)
# rownames(df_resultado) <- c("lambda","status")
# df_avgdata <- data.frame(matrix(ncol=2, nrow=nrow(data)))
# colnames(df_avgdata) <- c("reference","new")
# # odd number of columns is error
# if ((numcols%%2)!=0)
# {
#   df_resultado[2,1] <- paste("Even number of columns is necessary:\n",
#                              "\t- first half are assumed as repeated measures\n",
#                              "\t\tof a new method and second half as measures",
#                              "\t\tof a reference method."
#                              ,sep="")
# } else
# {
#   # if there is only two columns
#   if (numcols==2)
#   {
#     df_resultado[2,1] <- paste("Assuming:\n",
#                                "\t- reference method: ",colnames(data)[1],"\n",
#                                "\t- new method: ",colnames(data)[2],".\n"
#                                ,sep="")
#     colnames(df_avgdata) <- colnames(data)
#     df_avgdata[,1] <- data[,1]
#     df_avgdata[,2] <- data[,2]
#     x <- data[,2] # new method
#     y <- data[,1] # reference method
#     n <- nrow(data)
#     # Estimacao do IC95% do lambda sem medidas repetidas 
#     # Medida única ou para a média das repeticoes
#     # A ordem de x e y importa
#     # Shukla (1973), p. 376.
#     a <- var(x) - cov(x,y)
#     b <- var(y) - cov(x,y)
#     gl <- n - 2
#     alpha <- 0.05
#     t <- qt(1 - alpha/2, gl)
#     P <- (t^2)*(var(x)*var(y) - cov(x,y)^2)/(n - 2)
#     LI <- abs((b - sqrt(P))/(a + sqrt(P)))
#     LS <- abs((b + sqrt(P))/(a - sqrt(P)))
#     # cat("IC95%(lambda) = [", round(LI,4), ",", round(LS,4),"]\n")
#     # cat("Estimativa pontual de lambda = ",round((LI + LS)/2,4)) # lambda = 1.545    
#     df_resultado[1,1] <- (LI + LS)/2
#   } else
#   {
#     half <- numcols/2
#     firsthalf <- paste(colnames(data)[1:half])      
#     secondhalf <- paste(colnames(data)[(half+1):numcols])
#     # find names to two column sets
#     nomeref <- findcommonchars(firsthalf)
#     nomeref.concatenate <- ""
#     nomenew.concatenate <- ""
#     comma <- ""
#     for (i in 1:half)
#     {
#       nomeref.concatenate <- paste(nomeref.concatenate,comma,firsthalf[i],sep="")
#       nomenew.concatenate <- paste(nomenew.concatenate,comma,secondhalf[i],sep="")
#       comma <- ", "
#     }
#     nomenew <- findcommonchars(secondhalf)
#     if (nomeref==nomenew)
#     {
#       nomeref <- paste("ref",nomeref,sep="")
#       nomenew <- paste("new",nomenew,sep="")
#     }
#     df_resultado[2,1] <- paste("Assuming ",half," measures per method:\n",
#                                "\t- reference method: ",nomeref," (",nomeref.concatenate,")\n",
#                                "\t- new method: ",nomenew," (",nomenew.concatenate,").\n"
#                                ,sep="")
#     # rename columns
#     colnames(df_avgdata) <- c(nomeref,nomenew)
#     for (r in 1:nrow(data))
#     {
#       df_avgdata[r,1] <- mean(as.numeric(data[r,1:half]), na.rm=TRUE)
#       df_avgdata[r,2] <- mean(as.numeric(data[r,(half+1):numcols]), na.rm=TRUE)
#     }
#     
#     # Estimacao pontual do lambda para medidas repetidas usada na Regressao de Deming
#     # A ordem de x e y importa!
#     # Chapter 303 do NCSS 11 (2016): Deming regression
#     reference <- df_avgdata[,1]
#     referenceg <- mean(reference, na.rm=TRUE)
#     new <- df_avgdata[,2]
#     newg <- mean(new, na.rm=TRUE)
#     n <- nrow(data)
#     VARdelta <- 0
#     for (c in 1:half)
#     {
#       VARdelta <- VARdelta + sum((data[,c] - df_avgdata[,1])^2, na.rm=TRUE)
#     }
#     VARdelta <- VARdelta/n
#     VARepsilon <- 0
#     for (c in (half+1):numcols)
#     {
#       VARepsilon <- VARepsilon + sum((data[,c] - df_avgdata[,2])^2, na.rm=TRUE)
#     }
#     VARepsilon <- VARepsilon/n
#     # lambda <- VARepsilon/VARdelta # lambda = 1.692
#     df_resultado[1,1] <- VARepsilon/VARdelta
#   }
# } # no error
# 
# return (list(df_avgdata, df_resultado))

# library(readxl)
# x = PEFRmini
# y = PEFR
# Estimacao pontual do lambda para medidas repetidas usada na Regressao de Deming
# A ordem de x e y importa!
# Chapter 303 do NCSS 11 (2016): Deming regression
# PEFRDATA <- read_excel("PEFRDATA.xls")
# PEFR.media <- (PEFRDATA$PEFR1 + PEFRDATA$PEFR2)/2
# PEFR.mediag <- mean(PEFR.media, na.rm=TRUE)
# PEFRmini.media <- (PEFRDATA$PEFRmini1 + PEFRDATA$PEFRmini2)/2
# PEFRmini.mediag <- mean(PEFRmini.media, na.rm=TRUE)
# n <- nrow(PEFRDATA)
# VARdelta <- (sum((PEFRDATA$PEFR1 - PEFR.media)^2, na.rm=TRUE) + 
#                sum((PEFRDATA$PEFR2 - PEFR.media)^2, na.rm=TRUE))/n
# VARepsilon <- (sum((PEFRDATA$PEFRmini1 - PEFRmini.media)^2, na.rm=TRUE) + 
#                sum((PEFRDATA$PEFRmini2 - PEFRmini.media)^2, na.rm=TRUE))/n
# lambda <- VARepsilon/VARdelta # lambda = 1.692

# # Estimacao do IC95% do lambda sem medidas repetidas 
# # Medida única ou para a média das repeticoes
# # A ordem de x e y importa
# # Shukla (1973), p. 376.
# a <- var(PEFR.media) - cov(PEFR.media,PEFRmini.media)
# b <- var(PEFRmini.media) - cov(PEFR.media,PEFRmini.media)
# gl <- n - 2
# alpha <- 0.05
# t <- qt(1 - alpha/2, gl)
# P <- (t^2)*(var(PEFR.media)*var(PEFRmini.media) - cov(PEFR.media,PEFRmini.media)^2)/(n - 2)
# LI <- abs((b - sqrt(P))/(a + sqrt(P)))
# LS <- abs((b + sqrt(P))/(a - sqrt(P)))
# cat("IC95%(lambda) = [", round(LI,4), ",", round(LS,4),"]\n")
# cat("Estimativa pontual de lambda = ",round((LI + LS)/2,4)) # lambda = 1.545