#####################################################
#
# Exercise 3.5: Hall of Fame Pitching Dataset, Continued
#
# Needs the hofpitching.csv, the original can be found
# at https://github.com/maxtoki/baseball_R. However,
# I have provided an updated version to reflect the 
# 2014 hall of fame inductees. 
# It is located in the data folder
#
#####################################################

# Use the variables MidYear and WAR.Season from the previous exercises.
 hofpitching$MidYear <- with(hofpitching, (From + To) / 2)
 hofpitching$War.Season <- with(hofpitching, WAR / Yrs)

# Part(A)-(C): Make scatterplot of MidYear against War.Season
 with(hofpitching, plot(MidYear, War.Season,
                        xlab="Mid Career Year",
                        ylab="WAR Per Season",
                        main="HOFpitchers and WAR per Season"))
 with(hofpitching, lines(lowess(MidYear, War.Season, f=0.2)))
 with(hofpitching, identify(MidYear, War.Season, Name, n=2))
