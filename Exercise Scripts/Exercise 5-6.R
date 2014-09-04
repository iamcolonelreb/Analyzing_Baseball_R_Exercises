##--------Exercise 5-6: Hitting Evaluation of Players By Run Values----------
##  
## Needs: "transformed data2013.RData", this has all the necessary
## transformations of the retrosheet event files that are done in the chapter 
## and also is previous exercises.

load("transformed data2013.RData")
     
# First we must extract the different states of all players
  data2013b <- subset(data2013, BAT_EVENT_FL == TRUE)
  data2013b$RUNNERS <- substr(data2013b$STATE, 1, 3)
  

# Since I am a large Braves fan, and they seem to be a team that struggles with
# runners on. Therefore we will look at Jason Heyward, Justin Upton, Freddie
# Freeman and BJ Upton

# First we will compile the data and represent it in a table form. 
# After we have done that with the players mentioned we will save a pdf file
# of all the graphs of the players

#-----------------------Jason Heyward----------------------------------------
  heyward <- subset(data2013b, BAT_ID == "heywj001")
  table(heyward$RUNNERS)
# 000 001 010 011 100 101 110 111 
# 295   6  33   7  80  11  23   3 
  H.runs <- aggregate(heyward$RUNS.VALUE, list(heyward$RUNNERS), sum)
  names(H.runs)[2] <- "RUNS"
  H.PA <- aggregate(heyward$RUNS.VALUE, list(heyward$RUNNERS), length)
  names(H.PA)[2] <- "PA"
  H <- merge(H.PA, H.runs)
  H
# Group.1  PA        RUNS
# 1     000 295  2.64321639
# 2     001   6  1.14259713
# 3     010  33 -3.99510561
# 4     011   7 -0.34853799
# 5     100  80  6.45162302
# 6     101  11 -5.03265447
# 7     110  23 10.09532578
# 8     111   3  0.03875473
  sum(H$RUNS)
# [1] 10.99522

#------------------------Justin Upton-----------------------------------------
  j.upton <- subset(data2013b, BAT_ID == "uptoj001")
  table(j.upton$RUNNERS)
# 000 001 010 011 100 101 110 111 
# 391  17  55   9 131  19  30   9
  jup.RUNS <- aggregate(j.upton$RUNS.VALUE, list(j.upton$RUNNERS), sum)
  names(jup.RUNS)[2] <- "RUNS"
  jup.PA <- aggregate(j.upton$RUNS.VALUE, list(j.upton$RUNNERS), length)
  names(jup.PA)[2] <- "PA"
  jup <- merge(jup.PA, jup.RUNS)
  jup
# Group.1  PA       RUNS
# 1     000 391 16.8259409
# 2     001  17 -0.2497081
# 3     010  55 -3.7182713
# 4     011   9  1.1825042
# 5     100 131 -2.1866408
# 6     101  19  0.3265850
# 7     110  30  2.6136843
# 8     111   9  3.7070743
  sum(jup$RUNS)
# [1] 18.50117

#------------------------Freddie Freeman--------------------------------------
  freeman <- subset(data2013b, BAT_ID == "freef001")
  table(freeman$RUNNERS)
# 000 001 010 011 100 101 110 111 
# 339  19  68   8 133  23  43  13
  F.runs <- aggregate(freeman$RUNS.VALUE, list(freeman$RUNNERS), sum)
  names(F.runs)[2] <- "RUNS"
  F.PA <- aggregate(freeman$RUNS.VALUE, list(freeman$RUNNERS), length)
  names(F.PA)[2] <- "PA"
  F <- merge(F.PA, F.runs)
  F
#   Group.1  PA      RUNS
# 1     000 339  7.569076
# 2     001  19  4.642870
# 3     010  68  7.733410
# 4     011   8 -1.379565
# 5     100 133  2.948589
# 6     101  23  9.543679
# 7     110  43 17.164267
# 8     111  13  9.744220
  sum(F$RUNS)
# [1] 57.96655

#------------------------B.J. Upton------------------------------------------
  b.upton <- subset(data2013b, BAT_ID == "uptob001")
  table(b.upton$RUNNERS)
# 000 001 010 011 100 101 110 111 
# 265  11  32  10  66  13  44   8 
  bj.RUNS <- aggregate(b.upton$RUNS.VALUE, list(b.upton$RUNNERS), sum)
  names(bj.RUNS)[2] <- "RUNS"
  bj.PA <- aggregate(b.upton$RUNS.VALUE, list(b.upton$RUNNERS), length)
  names(bj.PA)[2] <- "PA"
  bj <- merge(bj.PA, bj.RUNS)
  bj
    Group.1  PA      RUNS
# 1     000 265 -4.955706
# 2     001  11 -1.942891
# 3     010  32 -4.827703
# 4     011  10 -2.927865
# 5     100  66 -3.080404
# 6     101  13 -3.799599
# 7     110  44 -3.429843
# 8     111   8 -2.947421
  sum(bj$RUNS)
# [1] -27.91143
# Yes, BJ Upton was that bad.

#------------------------Creating the Dotplots--------------------------------
  pdf("Exercise 5-6, Selected Braves Hitters Dotplots.pdf")

# Jason Heyward
  with(heyward, stripchart(RUNS.VALUE ~ RUNNERS, main="Jason Heyward PA Values",
                        vertical=TRUE, jitter=0.2, 
                        xlab = "Runners", ylab= "Runs Value",
                        method="jitter", pch=1, cex=0.8))
  abline(h=0)

# Justin Upton
  with(j.upton, stripchart(RUNS.VALUE ~ RUNNERS, main="Justin Upton PA Values",
                         vertical=TRUE, jitter=0.2, 
                         xlab = "Runners", ylab= "Runs Value",
                         method="jitter", pch=1, cex=0.8))
  abline(h=0)

# Freddie Freeman
  with(freeman, stripchart(RUNS.VALUE ~ RUNNERS, main="Freddie Freeman PA Values",
                         vertical=TRUE, jitter=0.2, 
                         xlab = "Runners", ylab= "Runs Value",
                         method="jitter", pch=1, cex=0.8))
  abline(h=0)

# B.J. Upton (It's gonna be ugly)
  with(b.upton, stripchart(RUNS.VALUE ~ RUNNERS, main="B.J. Upton PA Values",
                         vertical=TRUE, jitter=0.2, 
                         xlab = "Runners", ylab= "Runs Value",
                         method="jitter", pch=1, cex=0.8))
  abline(h=0)
# Save pdf by function dev.off()
  dev.off()
