# eiras.sampling.R

source("eiras.plot.barmeansd.R")
source("eiras.text.leading.R")

sampling <- function(values, B, n, replace=FALSE, graph=FALSE, col="#000000", y.pos=NA)
{
  amo_med <- c() # guardando as medias amostras
  amo_sd <- c() # guardando os d.p. amostras
  tp <- 99 - 5*log(B)
  lwd <- 5/(100-tp)
  if (tp<4){tp <- 4}
  if (lwd<0.2){lwd <- 0.2}
  if (lwd>  2){lwd <-   2}
  tp <- text.leading(as.character(round(tp,0)),2,"0")
  
  # fracao das linhas a exibir
  fexib <- 500/B
  if (fexib > 1) {fexib <- 1}
  # bootstrap
  d.max <- 0
  for (a in 1:B)
  {
    sample <- sample(values, n, replace=replace)
    amo_med <- c(amo_med,mean(sample))
    amo_sd <- c(amo_sd,sd(sample))
    amo_dens <- density(sample)
    if (graph==TRUE)
    {
      if (runif(1)<=fexib)
      {
        lines(amo_dens, col=paste(col,tp,sep=""), 
              lwd=lwd)
      }
      d.max <- max(d.max,amo_dens$y,na.rm=TRUE)
    }
  }
  if (is.na(y.pos)) {y.pos <- d.max}
  if (graph==TRUE)
  {
    plot.showmeansd(mean(amo_med,na.rm=TRUE), 
                    mean(amo_sd,na.rm=TRUE), 
                    color=col,
                    y.pos=y.pos)  
  }
  
  return(list(sample,amo_med,amo_sd))
}

