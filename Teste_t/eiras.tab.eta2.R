# eiras.tab.eta2.R

tab.eta2 <- function(eta2)
{
  eta2 <- sort(eta2)
  ec <- rep(NA, times=length(eta2))
  for (i in 1:length(eta2))
  {
    if (0 <= eta2[i] && eta2[i] < 0.01) {ec[i] <- "negligible"}
    if (0.01 <= eta2[i] && eta2[i] < 0.06) {ec[i] <- "small"}
    if (0.06 <= eta2[i] && eta2[i] < 0.14) {ec[i] <- "medium"}
    if (0.14 <= eta2[i]) {ec[i] <- "large"}
  }
  
  if (length(ec)==2)
  {
    if (ec[1]==ec[2])
    {
      ec <- ec[1]
    } else
    {
      ec <- paste("between ",ec[1]," and ",ec[2],sep="")
    }
  }
  return(ec)  
}
