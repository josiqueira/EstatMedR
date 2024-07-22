lr.neg <- 0.1137463
odds.pos <- odds.pre * lr.neg 
prob.pos <- odds.pos /(1+odds.pos) 
cat("odds.pos = ",odds.pos,"\n")
cat("prob.pos = ",prob.pos,"\n")
