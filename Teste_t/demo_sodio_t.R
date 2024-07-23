t_out <- t.test(data=Dados,
                subset=Instructor=="Brendon Small" | 
                       Instructor=="Coach McGuirk",
                Sodium~Instructor,
                conf.level=1-alfa)
print(t_out)