suppressMessages(library(readxl, warn.conflicts = FALSE))
suppressMessages(library(EnvStats, warn.conflicts = FALSE))

sigma <- 4
df.ratos <- readxl::read_excel("Table 8.2 RawBirthWeight.xls")
dados <- df.ratos$RBW 

out <- EnvStats::varTest(x=dados, sigma.squared = sigma^2)
cat("Teste de variancia:\n")
print(out)
cat("Conversão em desvio-padrão:\n")
cat("\tDesvio-padrao amostral = ",sd(dados,na.rm=TRUE),"\n",sep="")
IC95dp <- sqrt(out$conf.int)
cat("\tIC95(dp) = [",IC95dp[1],",",IC95dp[2],"]",sep="")

