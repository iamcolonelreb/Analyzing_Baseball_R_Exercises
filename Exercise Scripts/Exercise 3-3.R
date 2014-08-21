#####################################################
#
# Exercise 3.3: Hall of Fame Pitching Dataset, Continued
#
# Needs the hofpitching.csv, the original can be found
# at https://github.com/maxtoki/baseball_R. However,
# I have provided an updated version to reflect the 
# 2014 hall of fame inductees. 
# It is located in the data folder
#
#####################################################

# Create the variable WAR.Season for exercise
  hofpitching$War.Season <- with(hofpitching, WAR / Yrs)

# Part(A): Create parallel stripcharts of War.Season for different levels of BF.group
  stripchart(War.Season ~ BF.group, data=hofpitching,
             method="jitter", pch=1)

# Part(B): Create parallel boxplots of War.Season across BF.group
  boxplot(War.Season ~ BF.group, data=hofpitching,
          horizontal=TRUE, xlab="WAR Per Season")

# Part(C)
## By using the graphs to look how WAR per season and Batters Faced are related,
## we can see that a picther is much more likely to accumulate WAR when having a 
## higher total of batters faced. The pitchers with higher totals of batters faced
## need not pitch for more years when compared to others, for example they could have
## simply pitched longer into games more frequently than others, thus having more
## oppurtunities to accumulate WAR.
