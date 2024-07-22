moto.taxa.mes <- 366/12
pedestre.taxa.mes <- 349/12

moto.mortes <- c()
pedestre.mortes <- c()
tempo <- c()
sleep <- 0.1
t <- 0
maxy <- 0
while(1)
{
  t <- t+1
  tempo <- c(tempo,t)
  y <- rpois(n=1,lambda=moto.taxa.mes)
  moto.mortes <- c(moto.mortes,y)
  y <- rpois(n=1,lambda=pedestre.taxa.mes)
  pedestre.mortes <- c(pedestre.mortes,y)
  plot(tempo,moto.mortes, 
       pch=21,col="darkorange",bg="transparent",
       xlab="Meses", ylab="moto.mortes", ylim=c(0,80), axes=FALSE)
  axis(1,at=seq(tempo[1],tempo[length(tempo)],by=1))
  axis(2)
  lines(tempo,moto.mortes,col="darkorange",lty=2)
  points(tempo,pedestre.mortes,pch=22,col="royalblue",bg="transparent")
  lines(tempo,pedestre.mortes,col="royalblue",lty=2)
  
  if(t>12)
  {
    t0 <- which(tempo==t-12)
    t1 <- length(tempo)
    # mortes moto
    points(tempo[c(t0,t1)],moto.mortes[c(t0,t1)],pch=21,col="darkorange",bg="darkorange")
    lines(c(tempo[t0],tempo[t0],tempo[t1],tempo[t1]),
          c(moto.mortes[t0],5,5,moto.mortes[t1]), lty=3, col="darkorange")
    text(tempo[t0]+(tempo[t1]-tempo[t0])/2, 5,
         paste0("Motociclistas: ",round((moto.mortes[t1]-moto.mortes[t0])/moto.mortes[t1]*100,1),"%"),
         col="darkorange",pos=3, cex=1.5)
    # mortes pedestre
    points(tempo[c(t0,t1)],pedestre.mortes[c(t0,t1)],pch=21,col="royalblue",bg="royalblue")
    lines(c(tempo[t0],tempo[t0],tempo[t1],tempo[t1]),
          c(pedestre.mortes[t0],55,55,pedestre.mortes[t1]), lty=3, col="royalblue")
    text(tempo[t0]+(tempo[t1]-tempo[t0])/2, 55,
         paste0("Pedestres: ",round((pedestre.mortes[t1]-pedestre.mortes[t0])/pedestre.mortes[t1]*100,1),"%"),
         col="royalblue",pos=1, cex=1.5)
    
  }
  if(t > 28)
  {
    tempo <- tempo[(length(tempo)-28):length(tempo)]
    moto.mortes <- moto.mortes[(length(moto.mortes)-28):length(moto.mortes)]
    pedestre.mortes <- pedestre.mortes[(length(pedestre.mortes)-28):length(pedestre.mortes)]
    total.hoje.motos <- sum(moto.mortes[(length(moto.mortes)-11):length(moto.mortes)])
    total.antes.motos <- sum(moto.mortes[(length(moto.mortes)-11-12):(length(moto.mortes)-12)])
    total.hoje.pedestres <- sum(pedestre.mortes[(length(pedestre.mortes)-11):length(pedestre.mortes)])
    total.antes.pedestres <- sum(pedestre.mortes[(length(pedestre.mortes)-11-12):(length(pedestre.mortes)-12)])
    total.hoje <- total.hoje.motos+total.hoje.pedestres
    total.antes <- total.antes.motos+total.antes.pedestres
    diferenca <- round((total.hoje-total.antes)/total.hoje*100,1)
    dtxt <- format(abs(diferenca),digits=1,nsmall=1)
    if(diferenca == 0) {mmtxt <- "a mesma quantidade"}
    if(diferenca < 0) {mmtxt <- paste0(dtxt,"% a menos")}
    if(diferenca > 0) {mmtxt <- paste0(dtxt,"% a mais")}
    txt <- "Mortes de "
    if(total.hoje.motos>total.hoje.pedestres)
    {
      diftxt <- paste0("motociclistas superam as de pedestres")
    } 
    if(total.hoje.motos<total.hoje.pedestres)
    {
      diftxt <- paste0("pedestres superam as de motociclistas")
    }
    if(total.hoje.motos==total.hoje.pedestres)
    {
      diftxt <- paste0("pedestres igualam as de motociclistas")
    }  
    txt <- paste0(txt,diftxt," no trânsito de SP, diz relatório.\n\n",
                  "Foram ",total.hoje.motos," motociclistas e ",
                  total.hoje.pedestres," pedestres. Ao todo, a cidade de São Paulo\n",
                  "totalizou ",total.hoje," mortes no trânsito, ",
                  mmtxt," que nos 12 meses anteriores,\n",
                  "segundo a bobagem que tanto gostamos de noticiar.")
    text(tempo[1]+(tempo[length(tempo)]-tempo[1])/2,80,txt,pos=1)
    sleep <- 0.5
  }
  
  Sys.sleep(sleep)  
}
