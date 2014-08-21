#####################################################
#
# Exercise 3.4: Hall of Fame Pitching Dataset, Continued
#
# Needs the hofpitching.csv, the original can be found
# at https://github.com/maxtoki/baseball_R. However,
# I have provided an updated version to reflect the 
# 2014 hall of fame inductees. 
# It is located in the data folder
#
#####################################################

# Restrict the dataset to pitchers whose mid career year is after 1960
  hofpitching$MidYear <- with(hofpitching, (From + To) / 2)
  hofpitching.recent <- subset(hofpitching, MidYear >= 1960)
  hofpitching.recent

# Part(A): Order the rows of hofpitching.recent by War.Season
  hofpitching.recent <- hofpitching.recent[order(-hofpitching.recent$War.Season), ]

# Part(B): Create dotplot of War.Season where the labels are pitcher names
  dotchart(hofpitching.recent$War.Season,
           labels=hofpitching.recent$Name, xlab="WAR Per Season")

# Part(C):
## The two players that stand out on the dot chart are Tom Seaver and Rollie Fingers.
##### Tom Seaver 
# Tom Seaver was the only player to deliver over 5 WAR per season. A remarkable feat, but 
# looking on Baseball Reference, Greg Maddux was worth 0.6, and -0.1 WAR in his last two
# seasons. Had we retired after his 20th year like Seaver, he would have averaged over 5
# WAR as well. He would actually have had a higher value than Seaver too!!!
##### Rollie Fingers
# His value in WAR is far and away the lowest WAR per season amongst the HOFamers.
  
