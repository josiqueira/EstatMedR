# Applied Multivariate Statistics with R - Zelterman - 2015
head(datasets::swiss)
bivCI <- function(s, xbar, n, alpha, m)
  # returns m (x,y) coordinates of 1-alpha joint confidence ellipse of mean
{
  x <- sin(2 * pi * (0 : (m - 1)) / (m - 1)) # m points on a unit circle
  y <- cos(2 * pi * (0 : (m - 1)) / (m - 1))
  cv <- qchisq(1 - alpha, 2) # chi-squared critical value
  cv <- cv / n # value of quadratic form
  for (i in 1 : m)
  {
    pair <- c(x[i], y[i]) # ith (x,y) pair
    q <- pair %*% solve(s,pair) # quadratic form
    x[i] <- x[i] * sqrt(cv / q) + xbar[1]
    y[i] <- y[i] * sqrt(cv / q) + xbar[2]
  }
  cbind(x, y)
}
# biv <- swiss[,2:3]
# plot(biv, col = "black", pch = 16, cex.lab = 1.5)
# lines(bivCI(s = var(biv), xbar = colMeans(biv), n = dim(biv)[1],
#             alpha = .05, m = 1000),
#       type = "l", col = "black", lwd = 1)
# # Add ‘‘+’’ sign
# lines(colMeans(biv)[1], colMeans(biv)[2], pch = 3, cex = .8,
#       type = "p", lwd = 1)
