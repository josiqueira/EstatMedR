# Anscombe.R

cat("\n\n####--- media, d.p. e quartis")
for (r.idx in 0:3)
{
  cat("\n\nConjunto ",r.idx+1,":\n")  
  print(res[[1+6*r.idx]])
}

cat("\n\n####--- retas de regressão (intercepto, coef. angular e estatistica)")
for (r.idx in 0:3)
{
  cat("\n\nConjunto ",r.idx+1,":\n")  
  print(res[[5+6*r.idx]]$coefficients)
}

cat("\n\n####--- lambda (razao entre variancias)")
for (r.idx in 0:3)
{
  cat("\n\nConjunto ",r.idx+1,":\n")  
  print(res[[3+6*r.idx]])
}
