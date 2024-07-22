print(head(Dados))
alfa <- 0.05
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
s <- Rmisc::summarySE(data=Dados, 
                      measurevar="MCT", 
                      groupvars=c("Sexo","Sedentarismo"),
                      na.rm=TRUE, 
                      conf.interval=1-alfa/(length(unique(Dados$Sexo))*
                                            length(unique(Dados$Sedentarismo))))

pd <- ggplot2::position_dodge(0.5) # move them .05 to the left and right

# Intervalo de confianca de 95%: Standard error of the mean (se)
grf <- ggplot2::ggplot(s, 
                       ggplot2::aes(x=Sexo, 
                                    y=MCT, 
                                    colour=Sedentarismo)) + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-ci, 
                                      ymax=MCT+ci), 
                         width=.3, 
                         position=pd) +
  # ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
                      size=3,
                      shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("IC95% Bonferroni de MCT ") +
  ggplot2::ylim(50,80) +  
  ggplot2::theme_bw() +
  ggplot2::theme(legend.justification=c(1,0),
                 legend.position=c(1,0))
print(grf)

# 95% Error bar: se
# grf <- ggplot2::ggplot(s, 
#                        ggplot2::aes(x=Sexo, 
#                                     y=MCT, 
#                                     # group=Sedentarismo,
#                                     colour=Sedentarismo)) + 
#   ggplot2::geom_bar(position=ggplot2::position_dodge(1), 
#                     stat="identity",
#                     width=.8,
#                     fill="white") +
#   ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-ci, 
#                                       ymax=MCT+ci), 
#                          width=.3, 
#                          position=pd) +
#   # ggplot2::geom_line(position=pd) +
#   ggplot2::geom_point(position=pd, 
#                       size=3,
#                       shape=21) +
#   ggplot2::xlab("Sexo") +
#   ggplot2::ylab("MCT (kg)") +
#   ggplot2::ggtitle("Intervalo de confiança de 95% de MCT") +
#   ggplot2::scale_y_continuous(breaks=0:100*10) +
#   ggplot2::coord_cartesian(ylim=c(50,100)) +
#   ggplot2::theme_bw()
# print(grf)


