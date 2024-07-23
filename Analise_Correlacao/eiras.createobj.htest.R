# eiras.createobj.htest.R

createobj.htest<- function (method, 
                            data.name,
                            null.name, 
                              null.value = 0, alternative = "two.sided",
                            estimate.name = NA, estimate = NA, 
                            conf.int = NA, # optional
                            statistic.name = NA, statistic = NA,
                            coefficients.name = NA,
                              intercept = NA, slope = NA, 
                            p.value = NA, alpha = 0.05)
{
  # https://stats.stackexchange.com/questions/441651/how-do-you-program-a-custom-hypothesis-test-in-r
  # https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/htest.object
  standard.obj <- list()

  # character string giving the name of the test used.
  standard.obj <- c(standard.obj, method=method) # e.g., "Bootstrap regression"
  
  # character string containing the actual name(s) of the input data.
  if(!is.na(data.name))
  {
    standard.obj <- c(standard.obj, data.name=data.name) # e.g., "x vs. y"
  }
  
  # null.value
  #  numeric vector containing the value(s) of the population parameter(s) 
  #  specified by the null hypothesis. 
  #  This vector has a names attribute describing its elements.
  # alternative
  #  character string indicating the alternative hypothesis 
  #  (the value of the input argument alternative). 
  #  Possible values are "greater", "less", or "two-sided"
  attr(null.value, "names") <- null.name # e.g., "correlation";
  standard.obj <- c(standard.obj, null.value=list(null.value))
  standard.obj <- c(standard.obj, alternative=alternative)
  
  # optional items
  
  # p.value
  #  numeric scalar containing the p-value for the test under the null hypothesis.  
  if (!is.na(p.value))
  {
    attr(p.value, "names") <- NULL;
    standard.obj <- c(standard.obj, p.value=p.value)
  }
  
  # estimate
  #  numeric vector containing the value(s) of the estimated population parameter(s) 
  #  involved in the null hypothesis. 
  #  This vector has a names attribute describing its element(s).  
  if (!is.na(estimate.name))
  {
    attr(estimate, "names") <- estimate.name # e.g., "cor";
    standard.obj <- c(standard.obj, estimate=list(estimate))
  }
  
  # statistic
  #  numeric scalar containing the value of the test statistic, 
  #  with a names attribute indicating the null distribution.  
  if (!is.na(statistic.name))
  {
    attr(statistic, "names") <- statistic.name # e.g., "t";
    standard.obj <- c(standard.obj, statistic=list(statistic))
  }
  
  # conf.int
  #  numeric vector of length 2 containing lower and upper confidence limits 
  #  for the estimated population parameter. 
  #  This vector has an attribute called "conf.level" that is a numeric scalar 
  #  indicating the confidence level associated with the confidence interval.  
  if (is.na(conf.int[1]))
  {
    conf.int = 0 # to avoid error in the next if
  }
  if (length(conf.int)==2) # optional
  {
    attr(conf.int, "conf.level") <- round(1-alpha,2);
    standard.obj <- c(standard.obj, conf.int=list(conf.int))
  }
  
  # simple linear regression (must have slope, intercept vectors)
  if (!is.na(coefficients.name))
  {
    # create matrix
    q_itc <- quantile(intercept,probs=c(seq(0,1,0.25),alpha/2,1-alpha/2),na.rm=TRUE)
    q_slp <- quantile(slope,probs=c(seq(0,1,0.25),alpha/2,1-alpha/2),na.rm=TRUE)
    m_coef <- matrix(ncol=7,nrow=2)
    rownames(m_coef) <- c("intercept","slope") 
    colnames(m_coef) <- c("median","min","1st.Q","3rd.Q","max",
                          paste("IC",round((1-alpha)*100,0),".1",sep=""),
                          paste("IC",round((1-alpha)*100,0),".2",sep="")
                          ) 
    m_coef[1,] <- c(q_itc[3],q_itc[1:2],q_itc[4:7])
    m_coef[2,] <- c(q_slp[3],q_slp[1:2],q_slp[4:7])
    standard.obj <- c(standard.obj, coefficients=list(m_coef))
  }
  

  class(standard.obj) <- "htest";

  return (standard.obj)
}

# report <- createobj.htest(method = "Bootstrapping r",
#                 data.name = "Estatura (cm) and MCT (kg)",
#                 null.name = "correlation", 
#                   null.value = 0, alternative = "two.sided",
#                 estimate.name = "r (boot)", estimate = 0.666,
#                 conf.int = c(1,2),
#                 statistic.name = "zezo", statistic = 1.456,
#                 p.value = 0.2, alpha = 0.05)
# print(report)