# https://en.wikipedia.org/wiki/Chi-square_distribution

# media
curve(x*1,0,10,ylab="momento",xlab="df")

# mediana
curve(x*(1-2/(9*x))^3,0,10,add=TRUE, lty=2)

# variancia
curve(x*2,0,10,add=TRUE,lty=3) 

# assimetria
curve(sqrt(8/x),0,10,add=TRUE,lty=4) 

# excesso de curtose
curve(12/x,0,10,add=TRUE,lty=5) 

legend ("right",
        c("média","mediana","variância", "assimetria", "curtose"),
        lty=c(1,2,3,4,5),
        box.lwd=0, bg="transparent")
