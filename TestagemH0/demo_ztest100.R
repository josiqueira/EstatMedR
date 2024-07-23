mu <- 173
sigma <- 7
alfa <- 0.05
set.seed(3)
estatura <- rnorm(mean=176, sd=sigma, n=99)
v <- 100*176-99*mean(estatura)
estatura <- c(estatura, v)
fit <- BSDA::z.test(x=estatura, 
                    sigma.x=sigma, 
                    mu=mu,
                    alternative="two.sided", 
                    conf.level=1-alfa)
print(fit)
