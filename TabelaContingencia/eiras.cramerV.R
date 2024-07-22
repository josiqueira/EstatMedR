# eiras.cramerV.R
#   to show a precomputed value of Cramer's V

cramerV <- function(kind.of.test, show = TRUE,
                    statistic = NA, n = NA, L = NA, C = NA # X^2
)
{
  if (kind.of.test=="chisqgof")
  {
    V <- sqrt((statistic/n)/(L-1))
    txt <- "non adherence"
  }
  if (kind.of.test=="chisqtc")
  {
    V <- sqrt((statistic/n)/min(L-1,C-1))
    txt <- "association between nominal variables"
  }
  if (0 <= V & V < 0.1) {gV <- "minimal"}
  if (0.1 <= V & V < 0.3) {gV <- "small"}
  if (0.3 <= V & V < 0.5) {gV <- "intermediate"}
  if (0.5 <= V & V <= 1.0) {gV <- "large"}
  
  if (show == TRUE)
  {
    cat("\n\tCramer V = ", V,"\n",sep="")
    cat("\t",gV," degree of ",txt,"\n", sep="")
  }
  return (V)
}

# cramerVboot ()
# {
#   # verificar
#   # rcompanion::cramerV(TC,ci=TRUE,R=1e5)  
#   
#   
# }