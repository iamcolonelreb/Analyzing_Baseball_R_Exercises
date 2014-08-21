#####################################################
#
# Exercise 3.1: Hall of Fame Pitching Dataset
#
# Needs the hofpitching.csv, the original can be found
# at https://github.com/maxtoki/baseball_R. However,
# I have provided an updated version to reflect the 
# 2014 hall of fame inductees. 
# It is located in the data folder
#
#####################################################
# Setting up for the Exercise
  hofpitching <- read.csv("hofpitching.csv")
  hofpitching$BF.group <- with(hofpitching,
      cut(BF, c(0, 10000, 15000, 20000, 30000),
      labels=c("Less than 10000", "(10000, 15000)",
              "(15000, 20000)", "More than 20000")))
# Part(A): Create frequency table of BF.group using the table function
  T.BF.group <- table(hofpitching$BF.group)
  T.BF.group

# Part(B): Create bargraph from the output of (A)
  barplot(table(hofpitching$BF.group),
          xlab="Batters Faced", ylab="Frequency",
          main="Batters Faced of Hall of Fame Pitchers")

# Part(C): Create pie graph of the BF.group variable
  pie(table(hofpitching$BF.group),
      main="Batters Faced by Hall of Fame Pitchers")
