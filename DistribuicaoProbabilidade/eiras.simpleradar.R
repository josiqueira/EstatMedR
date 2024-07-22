# eiras.simpleradar.R

library(readxl)

source("eiras.friendlycolor.R")
source("eiras.text.leading.R")

simpleradar <- function(data,
                               levels=NA,
                               title=NA,
                               col.index=13,
                               cex=1,
                               center.on="median",
                               shownumbers = TRUE,
                               percentage = TRUE,
                               decimals = 1,
                               png.file=FALSE, png.width=480)
{
  options(warn=-1)
  
  if (is.na(levels[1]))
  {
    for (c in 1:ncol(data))
    {
      data[,c] <- as.numeric(unlist(data[,c]))
    }
    levels <- min(data,na.rm=TRUE):max(data,na.rm=TRUE)
  }
  df.counting <- data.frame(matrix(nrow=0, ncol=(length(levels)+1)))
  names(df.counting) <- c("item",levels)
  for (c in 1:ncol(data))
  {
    data[,c] <- factor(ordered(as.numeric(unlist(data[,c]))), levels=levels)
    table <- table(data[,c])
    df.tmp <- data.frame(matrix(data=c(names(data)[c],as.numeric(table)),
                                nrow=1,
                                ncol=(length(levels)+1),byrow=TRUE))
    names(df.tmp) <-  c("item",levels)
    df.counting <- rbind(df.counting,df.tmp)
  }
  count.max <- max(as.numeric(unlist(df.counting[,2:ncol(df.counting)])),na.rm=TRUE)
  x.max <- y.max <- 100
  y.item <- 2*pi/(nrow(df.counting))
  if (!(png.file==FALSE))
  {
    png(png.file, width = png.width, height = png.width) # squared
  }
  plot(NA, 
       main=title,
       xlab="", ylab="",
       xlim=c(0,x.max), ylim=c(0,y.max), 
       axes=FALSE)
  x.center <- x.max/2
  y.center <- y.max/2
  space.txt <- max(nchar(df.counting[,1]))*2
  h <- (x.max-2*space.txt)/2 # hypotenuse
  
  # values
  names.tmp <- c(names(df.counting)[1],center.on)
  quantiles <- seq(0,1,0.05) # it has to be an odd length
  names.tmp <- c(names.tmp,quantiles)
  df.tmp <- data.frame(matrix(ncol=length(names.tmp),nrow=nrow(df.counting)))
  names(df.tmp) <- names.tmp
  df.tmp[,1] <- df.counting[,1]
  for (r in 1:nrow(df.counting))
  {
    # w <- as.numeric(df.counting[r,2:ncol(df.counting)])/count.max
    # df.tmp[r,2] <- sum(levels*w,na.rm=TRUE)/sum(w,na.rm=TRUE)
    w <- as.numeric(df.counting[r,2:ncol(df.counting)])
    v <- c()
    for (i in 1:length(w))
    {
      v <- c(v, rep(i,w[i]))
    }
    if (center.on=="mean") 
    {
      df.tmp[r,2] <- mean(v, na.rm=TRUE)
    }
    if (center.on=="median") 
    {
      df.tmp[r,2] <- median(v, na.rm=TRUE)
    }
    for (q in 1:length(quantiles))
    {
      df.tmp[r,2+q] <- quantile(v,probs = quantiles[q])
    }
  }
  q2 <- (length(quantiles)-1)/2
  df.xyCI <- data.frame(matrix(nrow=nrow(df.tmp), ncol=4*q2+2))
  names(df.xyCI) <- c(
    "xc", "yc",
    paste("xcLI_",as.character(quantiles[1:q2]),sep=""),
    paste("ycLI_",as.character(quantiles[1:q2]),sep=""),
    paste("xcUI_",as.character(quantiles[length(quantiles):(q2+2)]),sep=""),
    paste("ycUI_",as.character(quantiles[length(quantiles):(q2+2)]),sep="")
  )
  ang <- 0
  for (r in 1:nrow(df.tmp))
  {
    dh <- (h*df.tmp[r,2])/max(levels)
    df.xyCI$xc[r] <- x.center+sin(ang)*dh
    df.xyCI$yc[r] <- y.center+cos(ang)*dh
    for (q in 1:q2)
    {
      dh <- (h*df.tmp[r,2+q])/max(levels)
      df.xyCI[r,q+2] <- x.center+sin(ang)*dh
      df.xyCI[r,q+2+q2] <- x.center+cos(ang)*dh
      dh <- (h*df.tmp[r,ncol(df.tmp)-(q-1)])/max(levels)
      df.xyCI[r,q+2+q2*2] <- x.center+sin(ang)*dh
      df.xyCI[r,q+2+q2*2+q2] <- x.center+cos(ang)*dh
    }

    ang <- ang+y.item
  }
  xp <- c(df.xyCI$xc,df.xyCI$xc[1])
  yp <- c(df.xyCI$yc,df.xyCI$yc[1])
  qs <- 1:q2
  # qs <- qs[qs!=1]
  for (q in qs)
  {
    xpb <- c(df.xyCI[,2+q],df.xyCI[1,2+q])
    ypb <- c(df.xyCI[,2+q+q2],df.xyCI[1,2+q+q2])
    lines(xpb, ypb, col=friendlycolor(col.index), lty=3, lwd=0.5)
    xpstep <- (xp-xpb)/50
    ypstep <- (yp-ypb)/50
    for (i in 1:50)
    {
      lines(xpb+xpstep*i, ypb+ypstep*i, col=paste(friendlycolor(col.index),"05",sep=""), lwd=5)
    }
    xpb <- c(df.xyCI[,2+2*q2+q],df.xyCI[1,2+2*q2+q])
    ypb <- c(df.xyCI[,2+2*q2+q+q2],df.xyCI[1,2+2*q2+q+q2])
    lines(xpb, ypb, col=friendlycolor(col.index), lty=3, lwd=0.5)
    xpstep <- (xp-xpb)/50
    ypstep <- (yp-ypb)/50
    for (i in 1:50)
    {
      lines(xpb+xpstep*i, ypb+ypstep*i, col=paste(friendlycolor(col.index),"05",sep=""), lwd=5)
    }
  }
  for (l in levels)
  {
    ang <- 0
    xc <- c()
    yc <- c()
    dh <- h*(levels[l]/max(levels))
    for (c in 1:nrow(df.counting))
    {
      x <- x.center+sin(ang)*dh
      xc <- c(xc,x)
      y <- y.center+cos(ang)*dh
      yc <- c(yc,y)
      ang <- ang+y.item
    }
    for (i in 1:length(xc))
    {
      lines(c(x.center,xc[i]), c(y.center,yc[i]), lty=1, lwd=0.5, col="#bbbbbb",bg="#bbbbbb")
    }
    lines(c(xc,xc[1]), c(yc,yc[1]), lty=1, lwd=0.5, col="#bbbbbb", bg="#bbbbbb")
  }
  points(x.center,y.center, pch=21, cex=0.5, col="#000000", bg="#000000")
  points(xc, yc, pch=21, cex=0.5, col="#000000", bg="#000000")
  
  # linhas principais
  lines(xp, yp, lty=1, lwd=4, col="white")
  points(df.xyCI$xc, df.xyCI$yc, cex=1, pch=21, col="white", bg="white")
  lines(xp, yp, lty=1, lwd=2, col=friendlycolor(col.index))
  points(df.xyCI$xc, df.xyCI$yc, cex=0.8, pch=21, col=friendlycolor(col.index), bg=friendlycolor(col.index))
  
  # labels
  dh <- (x.max-(x.center-h))/2
  ang <- 0
  for (c in 1:nrow(df.counting))
  {
    x <- x.center+sin(ang)*dh
    y <- y.center+cos(ang)*dh
    deg <- (ang*180)/pi
    text(x,y,df.counting[c,1],srt=90-deg,cex=cex)
    ang <- ang+y.item
  }
  if (!(png.file==FALSE))
  {
    dev.off()
  }
  
  options(warn=0)
}

# Dados <- readxl::read_excel("Felicidade.xlsx")
# Dados <- Dados[,c(3:12)]
# simpleradar(data=Dados, title="Felicidade",cex=0.8)
# simpleradar(data=Dados, title="Felicidade",cex=0.8, center.on="mean")
# 
# Dados <- readxl::read_excel("DREEM.xlsx")
# Dados <- Dados[,c(2:ncol(Dados))]
# simpleradar(data=Dados, title="DREEM",cex=0.6, center.on = "mean")
