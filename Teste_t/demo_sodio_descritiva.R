print(psych::describeBy(Sodium~Instructor, data=data.frame(Dados)))
boxplot(data=Dados, 
        Sodium~Instructor)
alfa <- 0.05
Rmisc::group.CI(Sodium~Instructor, 
                data=Dados, 
                ci=1-alfa/2)
gplots::plotmeans(data=Dados,
                  subset=Instructor=="Brendon Small" | 
                         Instructor=="Coach McGuirk",
                  Sodium~Instructor,
                  p=1-alfa/2,
                  connect=FALSE,
                  barcol="black",
                  main="IC95% Bonferroni")
