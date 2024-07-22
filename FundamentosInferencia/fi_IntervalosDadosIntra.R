# Within-subject CI
alfa <- 0.05
dfw <- read.table(header=TRUE, 
                  text='
 subject pretest posttest
       1    59.4     64.5
       2    46.4     52.4
       3    46.0     49.7
       4    49.0     48.7
       5    32.5     37.4
       6    45.2     49.5
       7    60.3     59.9
       8    54.3     54.1
       9    45.4     49.6
      10    38.9     48.5
 ')

# Treat subject ID as a factor
dfw$subject <- factor(dfw$subject)

# Convert to long format
dfw_long <- reshape2::melt(dfw,
                           id.vars="subject",
                           measure.vars=c("pretest","posttest"),
                           variable.name="condition")
print(dfw_long)

dfwc <- summarySEwithin2(dfw_long, 
                               measurevar="value", 
                               withinvars="condition",
                               idvar="subject", 
                               na.rm=TRUE, 
                               conf.interval=1-alfa/2)
print(dfwc)

# Make the graph with the 95% confidence interval
grf <- ggplot2::ggplot(dfwc, 
                ggplot2::aes(x=condition, 
                             y=value)) +
  # ggplot2::geom_line() +
  ggplot2::geom_errorbar(width=.1, 
                ggplot2::aes(ymin=value-ci, 
                             ymax=value+ci)) +
  ggplot2::geom_point(shape=21, 
             size=3, 
             fill="white") +
  ggplot2::ylim(40,60) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Within-subject CI95 Bonferroni") +
  ggplot2::theme_bw()
print(grf)

# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dfwc_between <- Rmisc::summarySE(data=dfw_long, 
                                 measurevar="value", 
                                 groupvars="condition", 
                                 na.rm=TRUE, 
                                 conf.interval=1-alfa/2)
print(dfwc_between)

# Show the between-S CI's in red, and the within-S CI's in black
grf <- ggplot2::ggplot(dfwc_between, 
                ggplot2::aes(x=condition, 
                             y=value)) +
  # ggplot2::geom_line() +
  ggplot2::geom_errorbar(width=.1, 
                ggplot2::aes(ymin=value-ci, 
                             ymax=value+ci), 
                colour="gray",
                data=dfwc_between) +
  ggplot2::geom_errorbar(width=.1, 
                ggplot2::aes(ymin=value-ci, 
                             ymax=value+ci), 
                colour="black",
                data=dfwc) +
  ggplot2::geom_point(shape=21, 
             size=3, 
             fill="white") +
  ggplot2::ylim(40, 60) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Within vs. Between-subject CI95 Bonferroni") +
  ggplot2::theme_bw()
print(grf)

# Use a consistent y range
ymax <- max(dfw_long$value)
ymin <- min(dfw_long$value)

# Plot the individuals
grf <- ggplot2::ggplot(dfw_long, 
                ggplot2::aes(x=condition, 
                             y=value, 
                             colour=subject, 
                             group=subject)) +
  ggplot2::geom_line() + 
  ggplot2::geom_point(shape=21, 
             fill="white") + 
  ggplot2::ylim(ymin,ymax) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Subject profile") +
  ggplot2::theme_bw()
print(grf)

# Two within-subjects variables
cat("\nTwo within-subjects variables\n")
data <- read.table(header=TRUE, 
                   text='
 Subject RoundMono SquareMono RoundColor SquareColor
       1        41         40         41          37
       2        57         56         56          53
       3        52         53         53          50
       4        49         47         47          47
       5        47         48         48          47
       6        37         34         35          36
       7        47         50         47          46
       8        41         40         38          40
       9        48         47         49          45
      10        37         35         36          35
      11        32         31         31          33
      12        47         42         42          42
')
print(data)
# Convert it to long format
data_long <- reshape2::melt(data=data, 
                            id.var="Subject",
                            measure.vars=c("RoundMono", 
                                           "SquareMono", 
                                           "RoundColor", 
                                           "SquareColor"),
                            variable.name="Condition")
names(data_long)[names(data_long)=="value"] <- "Time"

# Split Condition column into Shape and ColorScheme
data_long$Shape <- NA
data_long$Shape[grepl("^Round",  data_long$Condition)] <- "Round"
data_long$Shape[grepl("^Square", data_long$Condition)] <- "Square"
data_long$Shape <- factor(data_long$Shape)

data_long$ColorScheme <- NA
data_long$ColorScheme[grepl("Mono$",  data_long$Condition)] <- "Monochromatic"
data_long$ColorScheme[grepl("Color$", data_long$Condition)] <- "Colored"
data_long$ColorScheme <- factor(data_long$ColorScheme, levels=c("Monochromatic","Colored"))

# Remove the Condition column now
data_long$Condition <- NULL

# Look at first few rows 
head(data_long)

datac <- summarySEwithin2(data_long, 
                                measurevar="Time", 
                                withinvars=c("Shape",
                                             "ColorScheme"), 
                                idvar="Subject",
                                na.rm=TRUE,
                                conf.interval=1-alfa/4)

print(datac)

# Within-subject CI95
grf <- ggplot2::ggplot(datac, 
                ggplot2::aes(x=Shape, 
                             y=Time,
                             colour=ColorScheme)) +
  ggplot2::geom_errorbar(position=ggplot2::position_dodge(.9), 
                width=.25, 
                ggplot2::aes(ymin=Time-ci, 
                             ymax=Time+ci)) +
  # ggplot2::geom_line(position=ggplot2::position_dodge(.9)) +
  ggplot2::geom_point(shape=21, 
             size=3, 
             fill="white",
             position=ggplot2::position_dodge(.9)) +
  ggplot2::coord_cartesian(ylim=c(41,46)) +
  ggplot2::scale_fill_manual(values=c("#CCCCCC",
                             "#FFFFFF")) +
  ggplot2::scale_y_continuous(breaks=seq(1:100)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Within-subject CI95 Bonferroni")
print(grf)

# Within-subject Error Bar 95%
# grf <- ggplot2::ggplot(datac, 
#                        ggplot2::aes(x=Shape, 
#                                     y=Time, 
#                                     fill=ColorScheme)) +
#   ggplot2::geom_bar(position=ggplot2::position_dodge(.9), 
#                     colour="black", 
#                     stat="identity") +
#   ggplot2::geom_errorbar(position=ggplot2::position_dodge(.9), 
#                          width=.25, 
#                          ggplot2::aes(ymin=Time-ci, 
#                                       ymax=Time+ci)) +
#   ggplot2::coord_cartesian(ylim=c(40,46)) +
#   ggplot2::scale_fill_manual(values=c("#CCCCCC",
#                                       "#FFFFFF")) +
#   ggplot2::scale_y_continuous(breaks=seq(1:100)) +
#   ggplot2::theme_bw() +
#   ggplot2::ggtitle("Within-subject CI95")
# print(grf)

