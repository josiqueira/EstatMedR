# eiras.ConfidenceBand.R
# method ... c("lm_robust","deming")

# CI for lm_robust regression
ConfidenceBand <- function(x, y, alpha=0.05, B=1e4)
{
  suppressMessages(library(estimatr, warn.conflicts = FALSE))
  suppressMessages(library(dplyr, warn.conflicts = FALSE))

  lm.band <- NULL
  
  df_xyorg <- data.frame(x,y)
  df_xyorg <- df_xyorg[!is.na(df_xyorg$x),]
  df_xyorg <- df_xyorg[!is.na(df_xyorg$y),]
  size <- nrow(df_xyorg) 
  
  # ressampling
  beta_hat.sample <- c()
  alpha_hat.sample <- c()
  for (i in 1:B)
  {
    df_xy.sample <- dplyr::sample_n(df_xyorg, size, replace = TRUE) 
    regression <- lm(df_xy.sample$y ~ df_xy.sample$x)  
    alpha_hat.sample <- c(alpha_hat.sample, as.numeric(regression$coefficients[1]))
    beta_hat.sample <- c(beta_hat.sample,as.numeric(regression$coefficients[2]))
  }
  X <- c()
  LB <- c()
  UB <- c()
  X <- c()
  LB <- c()
  UB <- c()
  band.x <- seq(min(x,na.rm = TRUE), max(x,na.rm = TRUE), length.out = 1000)
  for (i in 1:length(band.x))
  {
    y_hat <- c()
    for (s in 1:length(beta_hat.sample))
    {
      y_hat <- c(y_hat, beta_hat.sample[s]*band.x[i]+alpha_hat.sample[s])
    }
    b <- quantile(y_hat,probs=c(alpha/2,1-alpha/2),na.rm = TRUE)
    X <- c(X,band.x[i])
    LB <- c(LB,as.numeric(b[1]))
    UB <- c(UB,as.numeric(b[2]))
  }
  beta_hat <- median(beta_hat.sample,na.rm = TRUE)
  beta_ic <- quantile(beta_hat.sample,probs=c(alpha/2,1-alpha/2),na.rm = TRUE)
  alpha_hat <- median(alpha_hat.sample,na.rm = TRUE)
  alpha_ic <- quantile(alpha_hat.sample,probs=c(alpha/2,1-alpha/2),na.rm = TRUE)
  lm.xy <- data.frame(X,LB,UB)
  lm.ic <- data.frame(matrix(nrow=0,ncol=3))
  names(lm.ic) <- c("coefficients","LCI","UCI")
  # intercept
  dt_tmp <- data.frame(alpha_hat,alpha_ic[1],alpha_ic[2])
  names(dt_tmp) <- c("coefficients","LCI","UCI")
  lm.ic <- rbind(lm.ic,dt_tmp)
  # slope
  dt_tmp <- data.frame(beta_hat,beta_ic[1],beta_ic[2])
  names(dt_tmp) <- c("coefficients","LCI","UCI")
  lm.ic <- rbind(lm.ic,dt_tmp)
  rownames(lm.ic) <- c("intercept","slope")
  
  lm.coefficients <- data.frame(beta_hat.sample,alpha_hat.sample)
  names(lm.coefficients) <- c("boot_slope","boot_intercept")
  
  lm.band <- list(lm.ic,lm.xy,lm.coefficients)
    
  return(lm.band)
}

# estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
# massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# lst <- ConfidenceBand(estatura, massa, alpha=0.05, B=1e3)

# library(readxl)
# 
# df_adm <- readxl::read_excel("Adm2008.xlsx")
# 
# lst <- ConfidenceBand(x=df_adm$Estatura, y=df_adm$MCT, alpha=0.05, B=1e3)
# band <- lst[[2]]
# 
# plot(df_adm$Estatura,df_adm$MCT)
# lines(band$X, band$LB)
# lines(band$X, band$UB)
