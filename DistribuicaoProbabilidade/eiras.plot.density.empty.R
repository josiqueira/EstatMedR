# eiras.plot.density.empty.R

plot.density.empty <- function(main,xlab,ylab,xlim,ylim)
{
  plot (NA, 
        main=main, 
        xlab=xlab, ylab=ylab,
        xlim = xlim,
        ylim = ylim
  )
}

