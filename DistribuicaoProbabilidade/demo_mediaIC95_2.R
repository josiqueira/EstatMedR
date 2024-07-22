suppressMessages(library(sandwich, warn.conflicts = FALSE))
suppressMessages(library(car, warn.conflicts = FALSE))
library(RcmdrMisc)
with(datasets::ToothGrowth, 
     RcmdrMisc::plotMeans(len, supp, as.factor(dose),  
                          error.bars="conf.int", level=.95, connect=FALSE,
                          xlab="Suppliment", ylab="Lenght", main="CI95%",
                          legend.pos="top"))
with(datasets::ToothGrowth, 
     RcmdrMisc::plotMeans(len, as.factor(dose), supp,  
                          error.bars="conf.int", level=.95, connect=FALSE,
                          xlab="Suppliment", ylab="Lenght", main="CI95%",
                          legend.pos="topleft"))
