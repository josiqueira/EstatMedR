# eiras.tab.dCohen.R

tab.dCohen <- function(d)
{
  d <- abs(d)
  d <- sort(d)
  dc <- rep(NA, times=length(d))
  # Sawilowsky, S (2009) New effect size rules of thumb. 
  # Journal of Modern Applied Statistical Methods, 8(2): 467-74.
  for (i in 1:length(d))
  {
    if (0 <= d[i] && d[i] < 0.01) {dc[i] <- "negligible"}
    if (0.01 <= d[i] && d[i] < 0.2) {dc[i] <- "very small"}
    if (0.2 <= d[i] && d[i] < 0.5) {dc[i] <- "small"}
    if (0.5 <= d[i] && d[i] < 0.8) {dc[i] <- "medium"}
    if (0.8 <= d[i] && d[i] < 1.2) {dc[i] <- "large"}
    if (1.2 <= d[i] && d[i] < 2) {dc[i] <- "very large"}
    if (2 <= d[i] && d[i] < Inf) {dc[i] <- "huge"}
  }
  
  if (length(dc)==2)
  {
    if (dc[1]==dc[2])
    {
      dc <- dc[1]
    } else
    {
      dc <- paste("between ",dc[1]," and ",dc[2],sep="")
    }
  }
  return(dc)  
}
