x <- 1:5
str(x)
xc <- as.numeric(x) # intervalar continua
str(xc)
xi <- as.integer(x) # intervalar discreta
str(xi)
xn <- factor(x) # nominal 
str(xn)
xo <- ordered(x) # ordinal
str(xo)
xf <- factor(x,levels=c("5","4","3","2","1"),ordered=TRUE) #ordinal, inverse order
str(xf)

