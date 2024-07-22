# Define the input variables
a <- 118 # Number of agreements in the first category
b <- 5 # Number of disagreements in the first category
c <- 2  # Number of disagreements in the second category
d <- 0 # Number of agreements in the second category

# Sample size
n <- a + b + c + d
df <- n - 1

# Probabilities
p11 <- a / n
p12 <- b / n
p21 <- c / n
p22 <- d / n

# Overall agreement probability
pa <- p11 + p22

# Marginal probabilities
pR1 <- (a + b) / n
pR2 <- (c + d) / n
pC1 <- (a + c) / n
pC2 <- (b + d) / n

# Average marginal probabilities
p_bar1 <- (pR1 + pC1) / 2
p_bar2 <- (pR2 + pC2) / 2

# Chance-agreement probabilities
pc_kappa <- (pR1 * pC1) + (pR2 * pC2)
pc_AC1 <- p_bar1 * (1 - p_bar1) + p_bar2 * (1 - p_bar2)
pc_G <- 0.5

# Agreement measures
kappa <- (pa - pc_kappa) / (1 - pc_kappa)
AC1 <- (a^2 + d^2 - (b + c)^2 / 2) / (a^2 + d^2 + (b + c)^2 / 2 + (a + d) * (b + c))
G <- ((a + d) - (b + c)) / n

# Standard errors
se_kappa2 <- (1 / (n * (1 - pc_kappa)^2)) * (pa * (1 - pa)
							 - 4 * (1 - kappa) * (p11 * p_bar1 +
																	  p22 * p_bar2 -
							 										 	pa * pc_kappa) +
							 +	4 * (1 - kappa)^2 * (p11 * (pR1 + pC1)^2 / 4
							 											 + p12 * (pR1 + pC2)^2 / 4
							 											 + p21 * (pR2 + pC1)^2 / 4
							 											 + p22 * (pR2 + pC2)^2 / 4
							 											 - pc_kappa^2))
se_kappa <- sqrt(se_kappa2)

se_AC12 <- (1 / (n * (1 - pc_AC1)^2)) * (pa * (1 - pa)
							 - 4 * (1 - AC1) * (p11 * (1 - p_bar1) +
            	 									 	p22 * (1 - p_bar2) -
							 									 	pa * pc_AC1)
						   +	4 * (1 - AC1)^2 * (p11 * (1 - (p_bar1 + p_bar1) / 2)^2
						   										 + p12 * (1 - (p_bar1 + p_bar2) / 2)^2
						   										 + p21 * (1 - (p_bar2 + p_bar1) / 2)^2
						   										 + p22 * (1 - (p_bar2 + p_bar2) / 2)^2
						   										 - pc_AC1^2))
se_AC1 <- sqrt(se_AC12)

se_G2 <- (4 / n) * pa * (1 - pa)
se_G <- sqrt(se_G2)

# t-values
t_kappa <- kappa / se_kappa
t_AC1 <- AC1 / se_AC1
t_G <- G / se_G

# p-values (two-tailed)
p_kappa <- 2 * (1 - pt(abs(t_kappa), df = n - 1))
p_AC1 <- 2 * (1 - pt(abs(t_AC1), df = n - 1))
p_G <- 2 * (1 - pt(abs(t_G), df = n - 1))

# Print the results
cat("Kappa: ", kappa, "\n")
cat("AC1: ", AC1, "\n")
cat("G: ", G, "\n")
cat("Standard Error (Kappa): ", se_kappa, "\n")
cat("Standard Error (AC1): ", se_AC1, "\n")
cat("Standard Error (G): ", se_G, "\n")
cat("Degrees of Freedom: ", df, "\n")
cat("t-value (Kappa): ", t_kappa, "\n")
cat("p-value (Kappa): ", p_kappa, "\n")
cat("t-value (AC1): ", t_AC1, "\n")
cat("p-value (AC1): ", p_AC1, "\n")
cat("t-value (G): ", t_G, "\n")
cat("p-value (G): ", p_G, "\n")
