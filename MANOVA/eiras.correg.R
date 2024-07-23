# eiras.correg.R
# method ... c(raw, preview, pearson, spearman, lm, lm_robust)
# - raw ... simple scatterplot + descriptive statistics, como os receber, e fará descritiva de 5 ptos + media e sd + n dos pares válidos.
# 3. preview fará o mesmo que raw, mas quero adicionar lowess porque esta ajuda; não vejo o porquê de suprimi-la; serve para julgar se os dados são lineares. Clareza, sem verdade, pois a teoria atrapalha. Se não for para usar lowess, inventarei uma spline ou ajustarei um polinômio.
# 4. para os outros 4 metodos, se recebe B>0, faz a versão bootstrapping
# 4. pearson ou spearman calculam correlação. Pearson exibirá o gráfico obrigatoriamente em z, ensinando que correlação é com variável padronizada, com a elipse de 95% e os eixos principal e secundários destacados que não ficarão distorcidos pela escala. Spearman exibirá os valores em ordem, (quase como se fossem postos, mas perservando a distância entre os valores) mostrando um conjunto de linhas no background dando crescimentos e decrescimentos em duas cores, dando a sensação do que leva ao seu valor.
# 5. lm e lm_robust  exibirão os dados brutos ou padronizados, por um parâmetro normaliza=c(TRUE, FALSE). Graficamente exibirão a reta de regressão com a banda de confiança. Podemos mostrar lowess no background, embora eu ache um pouco poluído (gosto da lowess no preview; aqui já foi definido que o modelo é linear.

suppressMessages(library(car, warn.conflicts = FALSE))
suppressMessages(library(dplyr, warn.conflicts = FALSE))
suppressMessages(library(estimatr, warn.conflicts = FALSE))
source("eiras.friendlycolor.R")
source("eiras.jitter.R")
source("eiras.col2rgbstring.R")
source("eiras.bartitle.R")
source("eiras.ellipseaxis.R")
source("eiras.LambdaEstimate.R")
source("eiras.createobj.htest.R")  
source("eiras.ConfidenceBand.R")

correg <- function (x, y,
                    method = "raw",
                    lowess = TRUE,
                    conf.band = TRUE,
                    pred.band = FALSE,
                    show.equation = TRUE,
                    suppress.graph=FALSE,
                    suppress.plot=FALSE,
                    suppress.text=FALSE,
                    main="",
                    xlab="x", ylab="y",
                    xlim=NA, ylim=NA,
                    segments=100,
                    jitter=NA,
                    col="black", bg="black", pch=21,
                    alpha=0.05,
                    standardize=FALSE,
                    B=0,
                    add=FALSE
)
{
  o.opt <- options()
  options(warn=-1) # disable warnings
  options(width=120)
  report <- ""
  report.title <- ""
  equation <- ""
  df_reg <- ""
  title <- main
  subtitle <- ""
  # to be sure, integer
  B <- round(as.numeric(B),0)   
  if(is.na(B)) {B=0}
  if(B<0) {B=0}
  # to be sure, all numeric
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (standardize==TRUE)
  {
    x <- scale(x)
    y <- scale(y)
  }
  # descriptive statists (all methods)
  dsc.cols <- c( "Variable", 
                 "Mean", "Sd",
                 "Min.","1st Qu.","Median","3rd Qu.","Max.",
                 "n", "NA's")
  df_dsc <- data.frame(matrix(nrow=0, ncol=length(dsc.cols)))
  names(df_dsc) <- dsc.cols
  for (c in 1:2)
  {
    if (c==1) {values=x; lab=xlab}
    if (c==2) {values=y; lab=ylab}
    for(i in 1:length(values))
    {
      if (!is.na(values[i]))
      {
        if(nchar(values[i])==0) {values[i] <- NA}
      }
    }
    dsc.values <- as.numeric(values)
    sd <- sd(values,na.rm=TRUE)
    n <- length(values)
    na <- sum(is.na(values))
    n <- n-na
    v <- c(lab,
           round(dsc.values[4],4),round(sd,4),
           round(dsc.values[1],4),round(dsc.values[2],4),round(dsc.values[3],4),
           dsc.values[5],dsc.values[6],
           n,na)
    df_tmp <- data.frame(matrix(data=v,nrow=1))
    names(df_tmp) <- dsc.cols
    df_dsc <- rbind(df_dsc,df_tmp)
  }
  # valid pairs
  df_xy <- data.frame(x,y)
  df_xy <- df_xy[!is.na(df_xy$x),]
  df_xy <- df_xy[!is.na(df_xy$y),]
  names(df_xy) <- c(xlab,ylab)
  lst_lambda <- LambdaEstimate(df_xy, 1, 2)
  df_raw <- lst_lambda[[1]]
  df_res <- lst_lambda[[2]]

  if (method=="raw")
  {
    if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
    title <- paste(title,"Raw data")
  }  
  if (method=="preview")
  {
    if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
    title <- paste(title,"Raw data + Robust line")
  }  
  # correlation
  if (method=="pearson")
  {
    report.title <- "Correlation"
    # normalization
    df_xy[,1] <- scale(df_xy[,1])
    df_xy[,2] <- scale(df_xy[,2])
    # test
    if(B==0)
    {
      report <- cor.test(df_xy[,1],df_xy[,2],method="pearson")
      report$data.name <- paste(xlab," and ",ylab,sep="")
      if (report$p.value<1e4)
      {
        p.txt <- sprintf("%.2e",report$p.value)
      } else
      {
        p.txt <- sprintf("%.4f",report$p.value)
      }
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Correlation: r=",round(report$estimate,4),
                     ", p=",p.txt,sep="")
    } else # bootstrapping
    {
      method.name <- paste("Bootstrapp r, ",B," resamples",sep="")
      data.name <- paste(xlab," and ",ylab,sep="")
      estimate <- c()
      df_samples <- data.frame(matrix(nrow=0,ncol=2))
      names(df_samples) <- c("x","y")
      for (b in 1:B)
      {
        df_sample <- dplyr::sample_n(df_xy, size=nrow(df_xy), replace=TRUE)
        names(df_sample) <- c("x","y")
        df_tmp <- data.frame(matrix(nrow=1,ncol=2))
        names(df_tmp) <- c("x","y")
        df_tmp$x <- list(df_sample$x)
        df_tmp$y <- list(df_sample$y)
        df_samples <- rbind(df_samples,df_tmp)
        estimate <- c(estimate, cor(df_sample$x,df_sample$y,method="pearson"))
      }
      r <- quantile(estimate, probs=c(alpha/2, 0.5, 1-alpha/2))

      report <- createobj.htest(method = method.name, 
                                data.name = data.name,
                                null.name = "correlation", null.value = 0, alternative = "two.sided",
                                estimate.name = "r (bootstrap)", estimate = r[2], 
                                conf.int = c(r[1],r[3]), alpha = 0.05, 
                                statistic.name = NA, statistic = NA, 
                                p.value = NA)
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Correlation (B=",B,"): r=",round(report$estimate,4),
                     " [",round(report$conf.int[1],4),
                     ", ",round(report$conf.int[2],4),"]",sep="")
    } # B
    # add z to graph labels
    standardize <- TRUE
    xlim <- c(-4,4)
    ylim <- c(-4,4)
  }
  if (method=="spearman")
  {
    report.title <- "Correlation"
    # order of x axis
    df_xy <- df_xy[order(df_xy[,1],df_xy[,2]),]
    # test
    # test
    if(B==0)
    {
      cat(df_xy[,1],"\n",df_xy[,2],"\n")
      report <- cor.test(df_xy[,1],df_xy[,2],method="spearman")
      report$data.name <- paste(xlab," and ",ylab,sep="")
      if (report$p.value<1e4)
      {
        p.txt <- sprintf("%.2e",report$p.value)
      } else
      {
        p.txt <- sprintf("%.4f",report$p.value)
      }
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Correlation: s=",round(report$estimate,4),
                     ", p=",p.txt,sep="")
    } else
    {
      # bootstrapping
      method.name <- paste("Bootstrapp s, ",B," resamples",sep="")
      data.name <- paste(xlab," and ",ylab,sep="")
      estimate <- c()
      df_samples <- data.frame(matrix(nrow=0,ncol=2))
      names(df_samples) <- c("x","y")
      for (b in 1:B)
      {
        df_sample <- dplyr::sample_n(df_xy, size=nrow(df_xy), replace=TRUE)
        names(df_sample) <- c("x","y")
        df_tmp <- data.frame(matrix(nrow=1,ncol=2))
        names(df_tmp) <- c("x","y")
        df_tmp$x <- list(df_sample$x)
        df_tmp$y <- list(df_sample$y)
        df_samples <- rbind(df_samples,df_tmp)
        clr <- cor.test(df_sample$x,df_sample$y,method="spearman")
        estimate <- c(estimate, clr$estimate)
      }
      s <- quantile(estimate, probs=c(alpha/2, 0.5, 1-alpha/2))
      
      report <- createobj.htest(method = method.name, 
                                data.name = data.name,
                                null.name = "correlation", null.value = 0, alternative = "two.sided",
                                estimate.name = "s (bootstrap)", estimate = s[2], 
                                conf.int = c(s[1],s[3]), alpha = 0.05, 
                                statistic.name = NA, statistic = NA, 
                                p.value = NA)
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Correlation (B=",B,"): s=",round(report$estimate,4),
                     " [",round(report$conf.int[1],4),
                     ", ",round(report$conf.int[2],4),"]",sep="")
    }
  }
  if (method=="lm" | method=="lm_robust")
  {
    report.title <- "Simple linear regression"
    centr_x <- mean(df_xy[,1], na.rm=TRUE)
    centr_y <- mean(df_xy[,2], na.rm=TRUE)
    if (B==0)
    {
      if (method=="lm")
      {
        rls <- lm(df_xy[,2] ~ df_xy[,1])
      }
      if (method=="lm_robust")
      {
        rls <- estimatr::lm_robust(df_xy[,2] ~ df_xy[,1])
      }
      report <- summary(rls)
      if (report$coefficients[2,4]<1e4)
      {
        p.txt <- sprintf("%.2e",report$coefficients[2,4])
      } else
      {
        p.txt <- sprintf("%.4f",report$coefficients[2,4])
      }
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Regression")
      if (show.equation)
      {
        title <- paste(title,": R^2=",round(report$r.squared,4),
                       ", p=",p.txt,sep="")
      }
      # equation for graph
      if (report$coefficients[1,1]<1e-4)
      {
        itc.txt <- sprintf("%.2e",report$coefficients[1,1])
      } else
      {
        itc.txt <- sprintf("%.4f",report$coefficients[1,1])
      }
      if (report$coefficients[2,1]<1e-4)
      {
        slp.txt <- sprintf("%.2e",abs(report$coefficients[2,1]))
      } else
      {
        slp.txt <- sprintf("%.4f",abs(report$coefficients[2,1]))
      }
      signal <- " + "
      if (report$coefficients[2,1]<0) {signal <- " - "}
      if(show.equation)
      {
        subtitle <- paste("mean[",ylab,"]=",itc.txt,
                          signal,
                          slp.txt," . ",xlab,sep="")
      } else
      {
        subtitle <- ""
      }
      # less rounded for text output
      equation <- paste("mean[",ylab,"]=",round(report$coefficients[1,1],8),
                        signal,
                        round(abs(report$coefficients[2,1]),8)," . ",xlab,sep="")
      report$call <- paste(method,"(formula = ",
                           ylab," ~ ",xlab,")",sep="")

      # computing confidence bands
      if (method=="lm") {posname<-4}
      if (method=="lm_robust") {posname<-12}
      rownames(report[[posname]])[2] <- xlab
      
      b0 <- report$coefficients[1]
      b1 <- report$coefficients[2]
      reg_x <- seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=2)
      reg_y <- b0+b1*reg_x
      conf_interval <- predict(rls, interval="confidence", alpha = alpha)
      conf_interval <- as.data.frame(conf_interval)
      names(conf_interval) <- c("fit","lwr","upr")
      if(b1!=0)
      {
        i.x <- (conf_interval$fit-b0)/b1
      } else
      {
        i.x <- seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=nrow(conf_interval))
      }
      conf_interval$x <- i.x
      conf_interval <- conf_interval[order(conf_interval$x),]
      pred_interval <- predict(rls, interval="prediction", alpha = alpha)
      pred_interval <- as.data.frame(pred_interval)
      names(pred_interval) <- c("fit","lwr","upr")
      if(b1!=0)
      {
        i.x <- (pred_interval$fit-b0)/b1
      } else
      {
        i.x <- seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=nrow(pred_interval))
      }
      pred_interval$x <- i.x
      pred_interval <- pred_interval[order(pred_interval$x),]
      df_reg <- data.frame(conf_interval$x, conf_interval$fit,
                           conf_interval$lwr,conf_interval$upr,
                           pred_interval$lwr,pred_interval$upr)
      names(df_reg) <- c("x","yhat","ci.LB","ci.UB","pi.LB","pi.UB")
    } else
    {
      # bootstrapping
      method.name <- paste("SLR: ",B," resamples",sep="")
      data.name <- paste(ylab," ~ ",xlab,sep="")
      
      # bootstrapping regression
      lst <- ConfidenceBand(df_xy[,1],df_xy[,2],alpha=alpha,B=B)
      intercepto <- lst[[3]]$boot_intercept
      inclinacao <- lst[[3]]$boot_slope
      reg_x <- lst[[2]]$X
      reg_y <- mean(lst[[3]]$boot_intercept)+mean(lst[[3]]$boot_slope)*lst[[2]]$X
      # confidence bands
      if(conf.band)
      {
        LB <- lst[[2]]$LB
        UB <- lst[[2]]$UB
      }
      df_reg <- data.frame(reg_x, reg_y,
                           LB,UB)
      names(df_reg) <- c("x","yhat","ci.LB","ci.UB")

      report <- createobj.htest(method = method.name,
                                data.name = data.name,
                                null.name = "slope", null.value = 0, alternative = "two.sided",
                                coefficients.name = "coefficients",
                                intercept = intercepto, slope = inclinacao,
                                alpha=alpha
      )
      
      signal <- " + "
      if (report$coefficients[2,1]<0) {signal <- " - "}
      # equation for graph
      v.txt <- rep(NA,6)
      cols <- c(1,6,7)
      v <- 0
      for (r in 1:2)
      {
        for (c in 1:3)
        {
          v <- v+1
          if (report$coefficients[r,cols[c]]<1e-4)
          {
            v.txt[v] <- sprintf("%.2e",report$coefficients[r,cols[c]])
          } else
          {
            v.txt[v] <- sprintf("%.4f",report$coefficients[r,cols[c]])
          }
        }
      }
      
      if(show.equation)
      {
        subtitle <- paste("mean[y] = ",v.txt[1],
                          "[",v.txt[2],",",v.txt[3],"]",
                          " + ",
                          v.txt[4],
                          "[",v.txt[5],",",v.txt[6],"]",
                          "x",sep="")
      } else
      {
        subtitle <- ""
      }
      # equation less rounded for text output
      equation <- paste("mean[",ylab,"] = ",round(report$coefficients[1,1],8),
                        " [",round(report$coefficients[1,6],8),", ",round(report$coefficients[1,7],8),"]",
                        signal, "\n\t\t",
                        round(abs(report$coefficients[2,1]),8),
                        " [",round(report$coefficients[2,6],8),", ",round(report$coefficients[2,7],8),"]",
                        " . ",xlab,sep="")
      if (nchar(title)>0) {title <- paste(title,"\n",sep="")}
      title <- paste(title,"Simple Linear Regression (B=",B,")", sep="")
      # # confidence bands
      # reg_x <- as.numeric(names(df_rls))
      # reg_y <- report$coefficients[1,1]+report$coefficients[2,1]*reg_x
      # centr_x <- mean(df_xy[,1], na.rm=TRUE)
      # centr_y <- mean(df_xy[,2], na.rm=TRUE)
      # LB <- rep(NA,ncol(df_rls))
      # UB <- rep(NA,ncol(df_rls))
      # for (c in 1:ncol(df_rls))
      # {
      #   q <- quantile(df_rls[,c],probs=c(alpha/2, 0.5, 1-alpha/2))
      #   LB[c] <- q[1]
      #   UB[c] <- q[3]
      # }
    }
  }
  if (standardize==TRUE)
  {
    # add z to graph labels
    xlab <- paste(xlab," (z)",sep="")
    ylab <- paste(ylab," (z)",sep="")
  }
  if (suppress.graph==FALSE)
  {
    if (suppress.plot==FALSE)
    {
      if (is.na(xlim[1]))
      {
        xmin <- min(df_xy[,1])
        xmax <- max(df_xy[,1])
      } else
      {
        xmin <- xlim[1]
        xmax <- xlim[2]
      }
      if (is.na(ylim[1]))
      {
        ymin <- min(df_xy[,2])
        ymax <- max(df_xy[,2])
      } else
      {
        ymin <- ylim[1]
        ymax <- ylim[2]
      }
      if (method=="lm" | method=="lm_robust")
      {
        if(conf.band)
        {
          ymin <- min(ymin,df_reg$ci.LB,df_reg$ci.UB)
          ymax <- max(ymax,df_reg$ci.LB,df_reg$ci.UB)
        }
        if(pred.band)
        {
          ymin <- min(ymin,df_reg$pi.LB,df_reg$pi.UB)
          ymax <- max(ymax,df_reg$pi.LB,df_reg$pi.UB)
        }
      }      
      if (method=="spearman")
      {
        jitter <- 0
      }
      df_xy$xj <- add.jitter(df_xy[,1],jitter)
      df_xy$yj <- add.jitter(df_xy[,2],jitter)
      if(!add)
      {
        plot(NA, 
             main=title, sub=subtitle,
             xlab=xlab, ylab=ylab,
             xlim=c(xmin,xmax), ylim=c(ymin,ymax))
      }
    }
    if (standardize==TRUE)
    {
      abline(v=0,lwd=0.7,lty=2,col="#444444")
      abline(h=0,lwd=0.7,lty=2,col="#444444")
    }
    if (method=="raw")
    {
      points(df_xy$xj, 
             df_xy$yj, 
             col=col, bg=bg, pch=pch)
    }
    if (method=="preview")
    {
      points(df_xy$xj, 
             df_xy$yj, 
             col=col, bg=bg, pch=pch)
      if(lowess)
      {
        ls <- lowess(df_xy[,1], df_xy[,2])
        col2="#ffffff"
        if (col=="white" | col=="#ffffff") {col2="#000000"}
        lines(ls,col=col2,lwd=3  ,lty=1)
        lines(ls,col=col ,lwd=1.5,lty=2)
      }
    }
    if (method=="pearson")
    {
      if (B==0)
      {
        points(df_xy$xj, 
               df_xy$yj, 
               col=col, bg=bg, pch=pch)
        ellipseaxis(df_xy[,1], df_xy[,2], col=col, level=1-alpha)
      } else # bootstrapping
      {
        colp <- paste(substr(col,1,7),"10",sep="")
        probshow <- 1000/B # do not show all ellipses
        # ellipse bands 
        for (b in 1:B)
        {
          if (runif(1)<=probshow)
          {
            ellipse <- ellipseaxis( x = unlist(df_samples$x[b]), 
                                    y = unlist(df_samples$y[b]), 
                                    segments=segments,
                                    col=colp, level=1-alpha,
                                    axis=FALSE)
          }
        }
        points(df_xy$xj,
               df_xy$yj,
               cex=1.1,
               col="white", bg="transparent", pch=pch)
        points(df_xy$xj, 
               df_xy$yj, 
               col=col, bg=bg, pch=pch)
        d.estimate <- density(estimate)
        if(!add)
        {
          plot(d.estimate,
               main=paste(xlab," x ",ylab,"\n",title,sep=""),
               xlab="r", ylab="Density",
               col=col, lwd=2,
               type="l")
        } else
        {
          lines(d.estimate, col=col, lwd=2)
        }
        m.estimate <- mean(estimate, na.rm=TRUE)
        s.estimate <- sd(estimate, na.rm=TRUE)
        y.estimate <- dnorm(d.estimate$x, mean=m.estimate, sd=s.estimate)
        lines(d.estimate$x,y.estimate,col=col,lty=2)
      } # bootstrapping
    }
    if (method=="spearman")
    {
      colpos <- paste(friendlycolor( 9),"80",sep="")
      colneg <- paste(friendlycolor(25),"80",sep="")
      if (B==0)
      {
        for (i in 2:nrow(df_xy))
        {
          colpn <- "#00000050"
          if ((df_xy[i,2]-df_xy[i-1,2])>0) {colpn <- colpos}
          if ((df_xy[i,2]-df_xy[i-1,2])<0) {colpn <- colneg}
          lines(c(df_xy$xj[i-1],df_xy$xj[i]),
                c(df_xy$yj[i-1],df_xy$yj[i]),
                col=colpn,lwd=3)
        }
      } else 
      {
        # bootstrapping
        colp <- paste(substr(col,1,7),"10",sep="")
        bgp <- paste(substr(bg,1,7),"10",sep="")
        probshow <- 1000/B # do not show all lines
        for (b in 1:B)
        {
          if (runif(1)<=probshow)
          {
            df_tmp = data.frame(unlist(df_samples$x[b]), 
                                unlist(df_samples$y[b]))
            names(df_tmp) <- c("x","y")
            df_tmp <- df_tmp[order(df_tmp[,1],df_tmp[,2]),]
            x <- df_tmp$x
            y <- df_tmp$y
            xg <- add.jitter(x)
            yg <- add.jitter(y)
            for (i in 2:length(x))
            {
              colpn <- "#00000050"
              if ((y[i]-y[i-1])>0) {colpn <- colpos}
              if ((y[i]-y[i-1])<0) {colpn <- colneg}
              lines(c(xg[i-1],xg[i]),
                    c(yg[i-1],yg[i]),
                    col=colpn,lwd=0.3)
            }
            points(xg, 
                   yg, 
                   col=colp, bg=bgp, pch=pch)
          }
        }
        d.estimate <- density(estimate)
        if(!add)
        {
          plot(d.estimate,
               main=paste(xlab," x ",ylab,"\n",title,sep=""),
               xlab="r", ylab="Density",
               col=col, lwd=2,
               type="l")
        } else
        {
          lines(d.estimate, col=col, lwd=2)
        }
        
        {
          
        }
        m.estimate <- mean(estimate, na.rm=TRUE)
        s.estimate <- sd(estimate, na.rm=TRUE)
        y.estimate <- dnorm(d.estimate$x, mean=m.estimate, sd=s.estimate)
        lines(d.estimate$x,y.estimate,col=col,lty=2)
      }
      points(df_xy$xj, 
             df_xy$yj, 
             col=col, bg=bg, pch=pch)
    }
    if (method=="lm" | method=="lm_robust")
    {
      if (B==0)
      {
        if(conf.band)
        {
          # polygon 
          border.x <- c(df_reg$x,rev(df_reg$x))
          border.y <- c(df_reg$ci.UB,rev(df_reg$ci.LB))
          colp <- paste(substr(col,1,7),"44",sep="")
          polygon(border.x,border.y,border=NA,col=colp)
        }
        if(pred.band)
        {
          # polygon 
          border.x <- c(df_reg$x,rev(df_reg$x))
          border.y <- c(df_reg$pi.UB,rev(df_reg$pi.LB))
          colp <- paste(substr(col,1,7),"44",sep="")
          polygon(border.x,border.y,border=NA,col=colp)
        }
      } else
      {
        # bootstrapping
        colp <- paste(substr(col,1,7),"60",sep="")
        probshow <- 1000/B # do not show all ellipses
        for (r in 1:length(inclinacao))
        {
          if (runif(1)<=probshow)
          {
            x_ic <- c(min(reg_x),max(reg_x))
            y_ic <- intercepto[r] + inclinacao[r]*x_ic
            lines(x_ic,y_ic,col=colp,lwd=0.3)
          }
        }
      }
      col2="#ffffff"
      if (col=="white" | col=="#ffffff") {col2="#000000"}
      if(lowess)
      {
        # linha de regressao robusta
        ls <- lowess(df_xy[,1], df_xy[,2])
        lines(ls,col=col2,lwd=1,lty=1)
        lines(ls,col=col ,lwd=1,lty=2)
      }
      # pontos
      points(df_xy$xj, 
             df_xy$yj, 
             col=col, bg=bg, pch=pch)
      # regression line 
      lines(reg_x,reg_y,col=col2,lwd=3.5,lty=1)
      lines(reg_x,reg_y,col=col ,lwd=2  ,lty=1)
      # centroid
      points(centr_x, centr_y, cex = 0.8, pch=21, col="white", bg="black")
      # IC
      if(conf.band)
      {
        lines(df_reg$x, df_reg$ci.UB, col=col2,lwd=3  , lty=1)
        lines(df_reg$x, df_reg$ci.UB, col=col ,lwd=1.5, lty=2)
        lines(df_reg$x, df_reg$ci.LB, col=col2,lwd=3  , lty=1)
        lines(df_reg$x, df_reg$ci.LB, col=col ,lwd=1.5, lty=2)
      }  
      if(pred.band)
      {
        lines(df_reg$x, df_reg$pi.UB, col=col2,lwd=3  , lty=1)
        lines(df_reg$x, df_reg$pi.UB, col=col ,lwd=1.5, lty=2)
        lines(df_reg$x, df_reg$pi.LB, col=col2,lwd=3  , lty=1)
        lines(df_reg$x, df_reg$pi.LB, col=col ,lwd=1.5, lty=2)
      }  
    }
  }
  if (suppress.text==FALSE)
  {
    print(df_dsc)
    cat("lambda = ",df_res[1],"\n")
    cat(df_res[2])
    if (nchar(report.title)>0)
    {
      cat(bartitle(report.title))
      print(report)
      if(B>0 & substr(method,1,2)=="lm")
      {
        print(report$coefficients)
      }
      if (substr(method,1,2)=="lm")
      {
        cat("\nEquation:\n  ",equation,"\n",sep="")    
      }
    }
  }
  
  options(o.opt) # restore
  return(list(df_dsc,df_raw,df_res,df_reg,report,equation))
}

# library(readxl)
# Gestantes <- readxl::read_excel("Gestantes.xlsx")
# estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
# massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# 
# lst <- correg(Gestantes$HT, Gestantes$HB, method="raw",
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# # lst[[1]] ... Tukey's 5-number statistics + mean(sd) + n
# # lst[[2]] ... raw data of valid pairs
# # lst[[3]] ... lst[[3]][1] ... lambda
# #              cat(lst[[3]][2]) ... column names report
# lst <- correg(Gestantes$HT, Gestantes$HB, method="raw",
#               col=friendlycolor(31), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# 
# lst <- correg(Gestantes$HT, Gestantes$HB, method="preview",
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="preview",
#               col=friendlycolor(7),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="preview",
#               col=friendlycolor(7), bg="transparent",
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# 
# lst <- correg(Gestantes$HT, Gestantes$HB, method="pearson",
#               col=friendlycolor(31), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="pearson",
#               col=friendlycolor(31), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)",
#               B=1e3)
# 
# lst <- correg(Gestantes$HT, Gestantes$HB, method="spearman",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="spearman",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)",
#               B=1e3)
# 
# lst <- correg(estatura, massa, method="spearman",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               main="Spearman",
#               xlab="Estatura (cm)", ylab="MCT (kg)")
# 
# lst <- correg(Gestantes$HT, Gestantes$HB, method="pearson",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="lm_robust",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="lm",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)",
#               B=1e3)
# 
# lst <- correg(estatura, massa, method="lm",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Estatura (cm)", ylab="MCT (kg)")
# lst <- correg(estatura, massa, method="lm_robust",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Estatura (cm)", ylab="MCT (kg)")
# 
# lst <- correg(estatura, massa, method="pearson",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Estatura (cm)", ylab="MCT (kg)")
# lst <- correg(estatura, massa, method="pearson", B=1e3,
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Estatura (cm)", ylab="MCT (kg)")
# lst <- correg(Gestantes$HT, Gestantes$HB, method="pearson",
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# lst <- correg(Gestantes$HT, Gestantes$LEUC, method="pearson", B=1e3,
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")

# lst <- correg(Gestantes$HT, Gestantes$LEUC, method="lm_robust", B=1e3,
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
# 
# estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
# massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# 
# # # scatterplot e regressao linear simples
# lst <- correg(estatura, massa, method="lm_robust", B=3e3,
#               xlab="Estatura (cm)", ylab="Massa corporal (kg)",
#               pch=21, col="#000000", bg=friendlycolor(24))
# 
# lst <- correg(Gestantes$HT, Gestantes$LEUC, method="lm_robust", B=1e3,
#               col=friendlycolor(7), bg=paste(friendlycolor(7),"50",sep=""),
#               xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")
