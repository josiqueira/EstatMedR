mu <- 173
sigma <- 7
alfa <- 0.05
estatura <- c(169, 174, 175, 186)
fit <- BSDA::z.test(x=estatura, 
                    sigma.x=sigma, 
                    mu=mu,
                    alternative="two.sided", 
                    conf.level=1-alfa)
print(fit)
