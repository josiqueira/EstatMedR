alfa <- 0.05
# The magnitude is assessed using the thresholds provided in (Cohen 1992), i.e. 
# |d|<0.2 "negligible", |d|<0.5 "small", |d|<0.8 "medium", otherwise "large"
dC <- effsize::cohen.d(Sodium~Instructor, 
                       data=Dados[Dados$Instructor=="Brendon Small" | 
                                    Dados$Instructor=="Coach McGuirk",], 
                       paired=FALSE,
                       conf.level=1-alfa,
                       na.rm=TRUE)
print(dC)
es <- effectsize::interpret_cohens_d(d=dC$estimate, 
                                     rules="sawilowsky2009")
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)

dC <- bootES::bootES(data=Dados[Dados$Instructor=="Brendon Small" | 
                                  Dados$Instructor=="Coach McGuirk",
                                c("Instructor","Sodium")],
                     effect.type="cohens.d",
                     ci.type="perc",
                     R=1e5,
                     data.col="Sodium", 
                     group.col="Instructor", 
                     contrast=c("Brendon Small", "Coach McGuirk"))
print(dC)
print(effectsize::interpret_cohens_d(d=dC$t0, rules="cohen1988"))

