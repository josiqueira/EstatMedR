# eiras.create.population.R
# Create a population with normal distribution
# or many normal distributions combined 
# (n, mean and sd of equal lengths)
#   n ... number or vector of individuals  (many normal distr. combined)
#   mean ... mean for normal 
#   sd ... sd for normal

create.population <- function(n, mean, sd)
{
  if (length(n)!=length(mean) | length(mean)!=length(sd))
  {
    cat("\nError in create.population():\n")
    cat("\tlength of n and parameters are to be equal:\n")
    eiras::exit()
  }
  pop_values <- c()
  for (pop in 1:length(n))
  {
    pop_values <- c(pop_values, 
                    rnorm(n[pop], 
                          mean=mean[pop], 
                          sd=sd[pop])
    )
  }
  return(pop_values)
}
