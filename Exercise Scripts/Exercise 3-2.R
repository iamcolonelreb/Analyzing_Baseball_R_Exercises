#####################################################
#
# Exercise 3.2: Hall of Fame Pitching Dataset, Continued
#
# Needs the hofpitching.csv, the original can be found
# at https://github.com/maxtoki/baseball_R. However,
# I have provided an updated version to reflect the 
# 2014 hall of fame inductees. 
# It is located in the data folder
#
#####################################################

# Part(A): Create a histogram of WAR (Wins Above Replacement) using the function hist
  hist(hofpitching$WAR,
       xlab="Wins Above Replacement", main="Hall of Fame Pitchers WAR",
       breaks=seq(0, 170, by=10))