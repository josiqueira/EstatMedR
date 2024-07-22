# eiras.likert.simplegraph.R

library(readxl)

source("eiras.friendlycolor.R")
source("eiras.text.leading.R")

likert.simplegraph <- function(data, col.index=13)
{
  for (c in 1:ncol(data))
  {
    data[,c] <- as.numeric(unlist(data[,c]))
  }
  levels <- min(data,na.rm=TRUE):max(data,na.rm=TRUE)
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
  plot(NA, 
       xlab="levels", ylab="item",
       xlim=c(0,x.max), ylim=c(0,y.max+y.item/5), 
       axes=FALSE)
  axis(1, at=c(x.txt,x.posmid,x.max), labels=c("",levels,""))
  abline(v=x.txt)
  # abline(v=x.pos,lty=2)
  # abline(v=x.posmid,lty=3)
  for (r in 1:nrow(df.counting))
  {
    # item name
    text(0, y.pos, df.counting[r,1])
    for (c in 2:ncol(df.counting))
    {
      hot <- as.numeric(df.counting[r,c])/count.max
      col <- friendlycolor(7)
      a <- text.leading(as.hexmode(200-round((1-hot)*150,0)),2,"0")
      col <- paste(col,a,sep="")
      x <- c(x.posmid[c-1]-5, x.posmid[c-1]+5, x.posmid[c-1]+5, x.posmid[c-1]-5, x.posmid[c-1]-5)
      y <- c(y.pos-y.item/2, y.pos-y.item/2, y.pos+y.item/2, y.pos+y.item/2, y.pos-y.item/2 )
      polygon(x,y,col=col,border=NA)
      proportion <- as.numeric(df.counting[r,c])/sum(as.numeric(unlist(df.counting[r,2:ncol(df.counting)])),na.rm=TRUE)
      text(x.posmid[c-1],y.pos,paste(round(proportion*100,1),"%",sep=""),cex=0.7)
    }
    # next item position
    y.pos <- y.pos - y.item
  }
}

# Dados <- readxl::read_excel("Felicidade.xlsx")
# Dados <- Dados[,c(3:12)]

Dados <- readxl::read_excel("DREEM.xlsx")
Dados <- Dados[,c(2:ncol(Dados))]

likert.simplegraph(data=Dados)
