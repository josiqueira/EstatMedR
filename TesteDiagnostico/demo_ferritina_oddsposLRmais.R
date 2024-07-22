lr.pos <- 5.9252459
odds.pos <- odds.pre * lr.pos 
prob.pos <- odds.pos /(1+odds.pos) 
cat("odds.pos = ",odds.pos,"\n")
cat("prob.pos = ",prob.pos,"\n")
