source("eiras_plotIC.r")
Dados <- readRDS("Nutricao3.rds")
# random seed
s <- round(runif(1,1,1e5),0)
cat("\nseed = ",s,"\n",sep="")

cat("\nBootstrapped independent one-Way ANOVA\n")
set.seed(s)
print(WRS2::t1waybt(Sodium ~ Instructor, 
                    data=Dados, 
                    tr=0, 
                    nboot=1e4))

cat("\npost hoc")
fatores <- unique(as.character(Dados$Instructor))
letra <- "A"
legenda <- c()
cat("\nLegenda:\n")
Dados$InstructorShort <- NA
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  Dados$InstructorShort[Dados$Instructor==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
Dados$InstructorShort <- as.factor(Dados$InstructorShort)

set.seed(s)
print(posthoc <- WRS2::mcppb20(Sodium~InstructorShort, 
                               data=Dados, 
                               tr=0.2, 
                               nboot=1e5))

grp.aux <- max(posthoc$comp[,c(1,2)])
g <- 64
for (f in 1:grp.aux)
{
  letra <- rawToChar(as.raw(g+f))
  print(letra)
  posthoc$comp[,1][posthoc$comp[,1]==f] <- letra
  posthoc$comp[,2][posthoc$comp[,2]==f] <- letra
}

df_plot <- data.frame(
  paste0(posthoc$comp[,1],"-",posthoc$comp[,2]),
  as.numeric(posthoc$comp[,3]),
  as.numeric(posthoc$comp[,4]),
  as.numeric(posthoc$comp[,5]),
  as.numeric(posthoc$comp[,6])*3
)

df_plot[,5][df_plot[,5]>1] <- 1

eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,
       cex=0.6,box.lwd=0, bty="n")
