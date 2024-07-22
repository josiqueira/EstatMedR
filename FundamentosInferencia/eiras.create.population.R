# eiras.create.population.R
# Create a population with normal distribution
# or many normal distributions combined 
# (n, mean and sd of equal lengths)
#   kind ... c("normal")
#   round ... decimal digits (FALSE means not round) 
#   n ... number or vector of individuals  (many normal distr. combined)
#   param1 ... parameter 1 
#         mean for normal
#   param2 ... value or vector
#         sd for normal

source ("eiras.exit.R")

create.population <- function(kind, round=FALSE, n, param1, param2)
{
  if (length(n)!=length(param1) | length(param1)!=length(param2))
  {
    cat("\nError in create.population():\n")
    cat("\tlength of n and parameters are to be equal:\n")
    eiras.exit()
  }
  pop_values <- c()
  for (pop in 1:length(n))
  {
    if (kind=="normal")
    {
      pop_values <- c(pop_values, 
                      rnorm(n[pop], 
                                  mean=param1[pop], 
                                  sd=param2[pop])
                      )
    }  
  }
  return(pop_values)
}
