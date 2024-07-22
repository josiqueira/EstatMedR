n <- sum(tabela)
prevalencia <- (tabela[1,1]+tabela[2,1])/n
sensibilidade <- tabela[1,1]/(tabela[1,1]+tabela[2,1])
especificidade <- tabela[2,2]/(tabela[1,2]+tabela[2,2])
cat("prevalencia = ",prevalencia,"\n",sep="")
cat("sensibilidade = ",sensibilidade,"\n",sep="")
cat("especificidade = ",especificidade,"\n",sep="")
cat("n = ",n,"\n",sep="")
