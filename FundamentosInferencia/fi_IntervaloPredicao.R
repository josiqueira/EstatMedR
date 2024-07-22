# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
s <- Rmisc::summarySE(Dados, 
                      measurevar="MCT", 
                      groupvars=c("Sexo","Sedentarismo"),
                      na.rm=TRUE)

# >=75% Error bar: sd
# Intervalo de predicao >= 75%: Standard deviation (sd)
pd <- ggplot2::position_dodge(0.5) # move them .05 to the left and right
k75 <- 2
grf <- ggplot2::ggplot(s, 
                       ggplot2::aes(x=Sexo, 
                                    y=MCT, 
                                    colour=Sedentarismo)) + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-k75*sd, 
                                      ymax=MCT+k75*sd), 
                         width=.3, 
                         position=pd) +
  # ggplot2::geom_line(position=pd, group=2) +
  ggplot2::geom_point(position=pd, 
                      size=3,
                      shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("Intervalo de predição >= 75% de MCT") +
  ggplot2::ylim(30,100) + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.justification=c(1,0),
                 legend.position=c(1,0))
print(grf)

# pd <- ggplot2::position_dodge(0.5) # move them .05 to the left and right
# k75 <- 2
# grf <- ggplot2::ggplot(s, 
#                        ggplot2::aes(x=Sexo, 
#                                     y=MCT, 
#                                     # group=Sedentarismo,
#                                     colour=Sedentarismo)) + 
#   ggplot2::geom_bar(position=ggplot2::position_dodge(1), 
#                     stat="identity",
#                     width=.8,
#                     fill="white") +
#   ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-k75*sd, 
#                                       ymax=MCT+k75*sd), 
#                          width=.3, 
#                          position=pd) +
#   # ggplot2::geom_line(position=pd) +
#   ggplot2::geom_point(position=pd, 
#                       size=3,
#                       shape=21) +
#   ggplot2::xlab("Sexo") +
#   ggplot2::ylab("MCT (kg)") +
#   ggplot2::ggtitle("Intervalo de predição >= 75% de MCT") +
#   ggplot2::scale_y_continuous(breaks=0:100*10) +
#   ggplot2::coord_cartesian(ylim=c(30,100)) +
#   ggplot2::theme_bw()
# print(grf)

