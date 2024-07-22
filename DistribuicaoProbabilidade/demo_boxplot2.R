boxplot(len~dose*supp, 
        data=datasets::ToothGrowth, 
        main="Tooth Growth", xlab="Suppliment and Dose", ylab="Length")
boxplot(len~supp*dose, 
        data=datasets::ToothGrowth, 
        main="Tooth Growth", xlab="Suppliment and Dose", ylab="Length")