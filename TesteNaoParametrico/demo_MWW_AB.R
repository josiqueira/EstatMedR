A <- c(288,283,120,119,432,274,890)
B <- c(119,43,153,854,588)
print(wilcox.test(A,B,
                  exact=FALSE,
                  correct=FALSE,
                  conf.int=TRUE,
                  conf.level=0.95))


