# eiras.likert.propgraph.R

library(readxl)

source("eiras.friendlycolor.R")
source("eiras.text.leading.R")

likert.propgraph <- function(data, levels, 
                             title=NA, 
                             col.index=13, 
                             cex=1, 
                             png.file=FALSE, png.width=480, png.height=600)
{
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
  y.max <- 100
  y.item <- y.max/(nrow(df.counting)-1)
  y.pos <- y.max
  x.txt <- max(nchar(df.counting[,1]))
  x.max <- x.txt+(ncol(df.counting)-1)*10
  x.pos <- seq(x.txt+10,x.max,length.out = (ncol(df.counting)-1))
  x.posmid <- x.pos-5
  if (!(png.file==FALSE))
  {
    png(png.file, width = png.width, height = png.height)
  }
  plot(NA, 
       main="",
       xlab="levels", ylab="",
       xlim=c(0,x.max), ylim=c(0,y.max+y.item/2), 
       axes=FALSE, cex.lab=cex)
  axis(1, at=c(x.txt,x.posmid,x.max), 
       line=0, lwd.ticks=0,
       labels=c("",levels,""), 
       cex.lab=cex, cex.axis=cex)
  title(main=title, line=0, cex.main=cex)  
  title(ylab="item", line=0, cex.lab=cex)  
  lines(c(x.txt,x.txt),c(0-y.item/2,y.max+y.item/2))
  # abline(v=x.pos,lty=2)
  # abline(v=x.posmid,lty=3)
  for (r in 1:nrow(df.counting))
  {
    # item name
    text(0, y.pos, df.counting[r,1],cex=cex)
    for (c in 2:ncol(df.counting))
    {
      hot <- as.numeric(df.counting[r,c])/count.max
      col <- friendlycolor(col.index)
      a <- text.leading(as.hexmode(200-round((1-hot)*150,0)),2,"0")
      col <- paste(col,a,sep="")
      x <- c(x.posmid[c-1]-5, x.posmid[c-1]+5, x.posmid[c-1]+5, x.posmid[c-1]-5, x.posmid[c-1]-5)
      y <- c(y.pos-y.item/2, y.pos-y.item/2, y.pos+y.item/2, y.pos+y.item/2, y.pos-y.item/2 )
      polygon(x,y,col=col,border=NA)
      proportion <- as.numeric(df.counting[r,c])/sum(as.numeric(unlist(df.counting[r,2:ncol(df.counting)])),na.rm=TRUE)
      text(x.posmid[c-1],y.pos,paste(round(proportion*100,1),"%",sep=""),cex=(0.8*cex))
    }
    # next item position
    y.pos <- y.pos - y.item
  }
  if (!(png.file==FALSE))
  {
    dev.off()
  }
}

# Dados <- readxl::read_excel("Felicidade.xlsx")
# Dados <- Dados[,c(3:12)]
# likert.propgraph(data=Dados, levels=1:5, title="Felicidade",cex=1)

Dados <- readxl::read_excel("DREEM.xlsx")
Dados <- Dados[,c(2:ncol(Dados))]
likert.propgraph(data=Dados, levels=1:5, title="DREEM",cex=0.6)
# ,
#                  png.file="teste2.png", png.width=900, png.height=1200)
