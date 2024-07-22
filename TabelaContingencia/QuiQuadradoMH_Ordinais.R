Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("< 15k",
              													 "15-25k",
              													 "25-40k",
              													 "> 40k"),
                              satisfaction = c("VeryD",
                              								 "LittleD",
                                               "ModerateS",
                              								 "VeryS")))
print(Job)
cat("QuiQuadradoMHRobusto_Ordinais")
DescTools::MHChisqTest(Job)
B = 1e6
chisq.test(Job, simulate.p.value = TRUE, B=B)
coin::cmh_test(jobsatisfaction,
							 distribution = coin::approximate(nresample = B))
coin::lbl_test(jobsatisfaction,
							 distribution = coin::approximate(nresample = B))

