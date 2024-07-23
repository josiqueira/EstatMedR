# eiras.ellipseaxis.R

suppressMessages(library(car, warn.conflicts = FALSE))

ellipseaxis <- function (x, y, 
                         draw=TRUE, axis=TRUE, 
                         segments=1000, 
                         col="#000000", level=0.5)
{
  # remove invalid points
  xy <- data.frame(x,y)
  xy <- xy[!is.na(xy$x),]
  xy <- xy[!is.na(xy$y),]
  x <- xy$x
  y <- xy$y
  # ellipse points
  ellipse <- car::dataEllipse(x, y,
                              segments=segments, # even numbers
                              levels=level,
                              robust=TRUE,
                              draw=FALSE)
  ellipse <- data.frame(ellipse)
  if (axis==TRUE)
  {
    # ellipse segments 
    semiellipse <- (nrow(ellipse)-1)/2 # (e.g., 12 segments -> (13rows-1)/2==6 ) 
    colmax <- c("r1","r2","dist2")
    df_xy <- data.frame(matrix(nrow=0,ncol=3))
    names(df_xy) <- colmax
    # distance between points e.g., 12 rows -> 1:5
    for (r in 1:semiellipse)
    {
      r1 <- r
      r2 <- r+semiellipse
      d2 <- ((ellipse$x[r1]-ellipse$x[r2])^2)+((ellipse$y[r1]-ellipse$y[r2])^2)
      df_tmp <- data.frame(r1,r2,d2)
      names(df_tmp) <- colmax
      df_xy <- rbind(df_xy,df_tmp)
    }
    # minimum distance
    df_min <- df_xy[df_xy$dist2==min(df_xy$dist2),]
    xmin1 <- ellipse$x[df_min$r1]
    xmin2 <- ellipse$x[df_min$r2]
    ymin1 <- ellipse$y[df_min$r1]
    ymin2 <- ellipse$y[df_min$r2]
    # ellipse axis y = e.b0 + e.b1*x
    while (xmin2==xmin1){xmin2<-xmin1+runif(1,0,xmin1/1e8)}
    e.b1 <- (ymin2-ymin1)/(xmin2-xmin1)
    e.b0 <- ymin2 - e.b1*xmin2
    e.xmin <- c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))
    e.ymin <- e.b0 + e.b1*e.xmin
    # maximum distance
    df_max <- df_xy[df_xy$dist2==max(df_xy$dist2),]
    xmax1 <- ellipse$x[df_max$r1]
    xmax2 <- ellipse$x[df_max$r2]
    ymax1 <- ellipse$y[df_max$r1]
    ymax2 <- ellipse$y[df_max$r2]
    # ellipse axis y = e.b0 + e.b1*x
    while (xmax2==xmax1){xmax2<-xmax1+runif(1,0,xmax1/1e8)}
    e.b1 <- (ymax2-ymax1)/(xmax2-xmax1)
    e.b0 <- ymax2 - e.b1*xmax2
    e.xmax <- c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))
    e.ymax <- e.b0 + e.b1*e.xmax
  } # axis==TRUE
  if (draw==TRUE)
  {
    if (axis==TRUE)
    {
      col2="#ffffff"
      if (col=="white" | col=="#ffffff") {col2="#000000"}
      lines(e.xmax,e.ymax, col=col2, lwd=4  , lty=2)
      lines(e.xmax,e.ymax, col=col , lwd=1.5, lty=2)
      lines(ellipse[,1],ellipse[,2], col=col2, lwd=4  , lty=2)
      lines(ellipse[,1],ellipse[,2], col=col , lwd=1.5, lty=2)
      points(c(xmax1,xmax2),c(ymax1,ymax2),pch=21,cex=0.4,bg=col,col=col)
    } else
    {
      # thin elipse only
      lines(ellipse[,1],ellipse[,2], col=col , lwd=0.5, lty=1)
    }
  }
  
  # cat(1-(df_min$dist2/df_max$dist2),"\n")

  return(ellipse)
}
# 
# suppressMessages(library(readxl, warn.conflicts = FALSE))
# 
# Gestantes <- readxl::read_excel("Gestantes.xlsx")
# x <- Gestantes$HT
# y <- Gestantes$HB+30
# plot (x,y,xlim=c(40,45),ylim=c(40,45))
# ellipseaxis(x,y,level=0.8,col="blue")
# 
# 
# estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
# massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# estatura <- scale(estatura)
# massa <- scale(massa)
# plot(estatura,massa)
# ellipseaxis(estatura,massa,level=0.5,col="blue")
