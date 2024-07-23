
lty <- 1
B <- 1e4
n <- c(10)
nep <- seq(1,3,length.out=20)
namesnep <- c("n", "H1", "total", "k")
dt_ep <- data.frame(matrix(nrow=0,ncol=length(namesnep)))
names(dt_ep) <- namesnep

# plot (NA, xlim = c(0.8,0.999), ylim = c(0.005, 0.3))
# abline(h=0.05, lty=2)
# cat("\n")

a<-1
df <- 2
c<-10
ic_zsL <- c()
ic_zsU <- c()
eps <- seq(1,5,length.out = 100)
for (k in eps)
{
  conta_H1 <- 0
  for (b in 1:B)
  {
    amostra <- rchisq(n[a],df)
    mean_z <- mean(amostra)
    ep_z <- sd(amostra)/sqrt(n[a])
    ic_z <- mean_z + k*c(-ep_z, ep_z) # qt(0.975,n[a]-1)
    if (!(ic_z[1]<df & ic_z[2]>df)) {conta_H1 <- conta_H1+1}
    # ic_zsL <- c(ic_zsL, ic_z[1])
    # ic_zsU <- c(ic_zsU, ic_z[2])
  }
  cat(k,conta_H1/B,"\n")
  # namesnep <- c("n", "H1", "total", "k")
  dt_tmp <- data.frame(n[a],conta_H1,B,k)
  names(dt_tmp) <- namesnep
  dt_ep <- rbind(dt_ep,dt_tmp)
} # k
dt_ep$prop=dt_ep$H1/dt_ep$total
plot(dt_ep$k,dt_ep$prop)
abline(h=0.05, lty=2)
abline(v=qt(0.975,n[a]-1), lty=2)
