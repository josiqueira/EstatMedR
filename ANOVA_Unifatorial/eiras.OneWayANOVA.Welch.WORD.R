# ANOVA unifatorial independente de Welch sem dados brutos
OnewayWelch.WithoutRawData <- function(data, 
                                       factor, mean, sd, n, 
                                       alpha=0.05, echo=TRUE)
{
  namescomp <- c("Group.1","Group.2",
                 "Mean.1", "st.dev.1","n.1",
                 "Mean.2", "st.dev.2","n.2",
                 "p.adj.","sig.","eta^2","effect.size")
  dt_comparison <- data.frame(matrix(nrow=0,ncol=length(namescomp)))
  names(dt_comparison) <- namescomp
  
  col.factor <- which(names(data)==factor)
  col.mean <- which(names(data)==mean)
  col.sd <- which(names(data)==sd) 
  col.n <- which(names(data)==n)
  groups <- data[[col.factor]]
  print(data)
  
  # Teste ANOVA unifatorial independente desbalanceado heterocesdático de Welch
  # Modern Statistics for the Social and Behavioral Sciences in R - Wilcox - 2012, p. 442-6
  c_n <- as.numeric(data[[col.n]])
  c_mean <- as.numeric(data[[col.mean]])
  c_sd <- as.numeric(data[[col.sd]])
  J <- length(c_n)
  balanc <- max(c_sd)/min(c_sd)
  var <- c_sd^2
  w <- c_n/var
  U <- sum(w)
  X_til <- as.numeric(w%*%c_mean/U)
  gl_num <- J - 1
  A <- as.numeric(w%*%((c_mean - X_til)^2)/(J-1))
  B <- as.numeric((2*(J - 2)/(J^2 - 1))*(((1 - w/U)/(c_n-1))%*%(1 - w/U)))
  gl_denom <- 1/(((3/2)/(J-2))*B)
  F <- A/(1+B)
  p <- pf(F,gl_num,gl_denom,lower.tail=FALSE)
  eta2 <- gl_num*F/(gl_num*F + gl_denom)
  es <- effectsize::interpret_eta_squared(eta2)
  mag_eta2 <- es[1]
  
  if (echo)
  {
    cat("\n------------\nOmnibus\n------------\n",sep="")
    cat("Statistical significance:\n")
    cat("\tF(",gl_num,",",gl_denom,") = ",F,", p = ",p,"\n",sep="")
    cat("Effect size:\n")
    cat("\teta^2 = R^2 = ",eta2," (",mag_eta2,")\n",sep="")
    cat("Homocedasticity heuristic (ratio<2):\n")
    cat("\tmax(sd)/min(sd) = ", balanc,"\n\n")
  }
  
  namesANOVA <- c("Homocedasticity","F","df1","df2","p","eta^2","effect.size")
  dt_anova <- data.frame(matrix(nrow=1,ncol=length(namesANOVA)))
  names(dt_anova) <- namesANOVA
  dt_anova$Homocedasticity <- balanc
  dt_anova$F <- F
  dt_anova$df1 <- gl_num
  dt_anova$df2 <- gl_denom
  dt_anova$p <- p
  dt_anova$`eta^2` <- eta2
  dt_anova$effect.size <- mag_eta2
  
  if (echo)
  {
    cat("\n----------------\nPost hoc\n----------------\n",sep="")
    cat("Bonferroni correction for p values (",choose(nrow(data),2)," comparisons 2 x 2)\n",sep="")
    
    cat("\nComparing groups:")
    for (e.aux in 1:length(groups))
    {
      cat("\n- ",groups[e.aux],sep="")
    }
    cat("\n\n")
  }
  for (g1 in 1:(nrow(data)-1))
  {
    for (g2 in (g1+1):nrow(data))
    {
      nA <- c_n[g1]
      nB <- c_n[g2]
      meanA <- c_mean[g1]
      meanB <- c_mean[g2]
      sdA <- c_sd[g1]
      sdB <- c_sd[g2]
      
      # Teste t de Welch bilateral sem dados brutos
      dif <- meanB - meanA
      dfA <- nA - 1
      dfB <- nB - 1
      ep2A <- sdA^2/nA; ep2B <- sdB^2/nB
      df <- ((ep2A+ep2B)^2)/((ep2A^2)/dfA+(ep2B^2)/dfB)
      t <- dif/sqrt(ep2A+ep2B)
      p <- 2*pt(-abs(t),df) * choose(nrow(data),2) # Bonferroni correction
      if(p>1) {p<-1}
      eta2 <- t^2/(t^2 + df)
      es <- effectsize::interpret_eta_squared(eta2)
      mag_eta2 <- es[1]
      
      dt_tmp <- data.frame(matrix(nrow=1,ncol=length(namescomp)))
      names(dt_tmp) <- namescomp
      dt_tmp$`Group.1` <- data[g1,col.factor]
      dt_tmp$`Group.2` <- data[g2,col.factor]
      dt_tmp$`Mean.1` <- meanA
      dt_tmp$`st.dev.1` <- sdA
      dt_tmp$`n.1` <- nA
      dt_tmp$`Mean.2` <- meanB
      dt_tmp$`st.dev.2` <- sdB
      dt_tmp$`n.2` <- nB
      dt_tmp$`p.adj.` <- p
      if (p<alpha) {dt_tmp$`sig.` <- "*"} else {dt_tmp$`sig.` <- "ns"}
      dt_tmp$`eta^2` <- round(eta2,3)
      dt_tmp$`effect.size` <- paste0("(",mag_eta2,")")
      dt_comparison <- rbind(dt_comparison,dt_tmp)
    }
  }
  
  if (echo)
  {
    prmatrix(dt_comparison,quote=FALSE,rowlab=rep("",nrow(dt_comparison)))
  }
  
  return(list(dt_anova,dt_comparison))
}
