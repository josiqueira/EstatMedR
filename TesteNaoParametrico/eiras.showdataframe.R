# eiras.showdataframe.R

showdataframe <- function(dt, head=NA, tail=NA)
{
  colnames <- names(dt)
  if (is.na(head) & is.na(tail))
  {
    dt_new <- dt
  } else
  {
    dt_new <- data.frame(matrix(nrow=0,ncol=length(colnames)))
    names(dt_new) <- colnames
    if (!is.na(head))
    {
      dt_tmp <- dt[1:head,]
      dt_new <- rbind (dt_new,dt_tmp)
    }
    dt_tmp <- data.frame(matrix(data=rep("...",length(colnames)),nrow=1,ncol=length(colnames)))
    names(dt_tmp) <- colnames
    dt_new <- rbind (dt_new,dt_tmp)
    if (!is.na(tail))
    {
      dt_tmp <- dt[(nrow(dt)-tail+1):nrow(dt),]
      dt_new <- rbind (dt_new,dt_tmp)
    }
  }
  
  prmatrix((dt_new),quote=FALSE,rowlab = rep("",nrow(dt_new)))
}