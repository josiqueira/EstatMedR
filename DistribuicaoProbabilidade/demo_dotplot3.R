library(readxl)
library(lattice)

TH <- readxl::read_excel("Sodio_2.xlsx")

TH$Instructor <- factor(TH$Instructor, levels=unique(TH$Instructor))
grafico <- lattice::xyplot(Sodium ~ Instructor, data=TH, type=c("p","a"),
                           jitter.x=TRUE, jitter.y=TRUE, col="black")
print(grafico)
