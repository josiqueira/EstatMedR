suppressMessages(library(psych, warn.conflicts = FALSE))
suppressMessages(library(likert, warn.conflicts = FALSE))

Input <- ("
  Pooh    Leitao  Tigrao
  3       2       4
  5       4       4
  4       2       4
  4       2       4
  4       1       5
  4       2       3
  4       3       5
  4       2       4
  5       2       4
  5       3       3
")
Dados <- read.table(textConnection(Input),header=TRUE)
Dados$Pooh <- factor(Dados$Pooh,
                     levels = c("1", "2", "3", "4", "5"),
                     ordered = TRUE)
Dados$Leitao <- factor(Dados$Leitao,
                       levels = c("1", "2", "3", "4", "5"),
                       ordered = TRUE)
Dados$Tigrao <- factor(Dados$Tigrao,
                       levels = c("1", "2", "3", "4", "5"),
                       ordered = TRUE)
psych::headTail(Dados)
print(str(Dados))
print(summary(Dados))
print(likert::likert(Dados)) # Percent responses in each group
out <- likert::likert(Dados)
print(summary(out))
print(plot(out,type="bar"))
print(plot(out,type="heat",low.color = "white",high.color = "blue",
     text.color = "black",text.size = 4,wrap = 50))
print(plot(out,type="density",facet = TRUE,bw = 0.5))
