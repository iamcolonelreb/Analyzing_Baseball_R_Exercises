##-------Exercise 5-2: Value of Different Ways of Reaching First Base----------
##
## Needs: events2013.csv, These are located in the data folder, 2013 events are 
## used to be up to date and compare how the run scoring environment has 
## changed in recent years.
##
## It is helpful to load the Exercise 5-1.RData from the R Data folder, this is
## because you would have to perform the steps from Part (A) again

  load("transformed data2013.RData")

#-----------------Run Value of Walk with Runner on First-----------------------
  walk <- subset(data2013, EVENT_CD == 14)
# subset by beggining state with runner on first
  walk.100 <- subset(walk, 
                     STATE == "100 0" | STATE == "100 1" | STATE == "100 2")
  table(walk.100$STATE)
  mean.walk.100 <- mean(walk.100$RUNS.VALUE)

#-----------------Run Value of HBP with Runner on First-----------------------
  HBP <- subset(data2013, EVENT_CD == 16)
  # subset by beggining state with runner on first
  HBP.100 <- subset(HBP, 
                    STATE == "100 0" | STATE == "100 1" | STATE == "100 2")
  mean.HBP.100 <- mean(HBP.100$RUNS.VALUE)

#-----------------Run Value of Single with Runner on First----------------------
  d.single <- subset(data2013, EVENT_CD == 20)
  # subset by beggining state with runner on first
  single.100 <- subset(d.single,
                       STATE == "100 0" | STATE == "100 1" | STATE == "100 2")
  mean.single.100 <- mean(single.100$RUNS.VALUE)
 
#------------Create Graphs To Visualize Run Values-----------------------------
library(MASS)
pdf("Exercise 5-2 Walk, HBP, Single Values With Runner on First.pdf")

# Histogram of Walk
  truehist(walk.100$RUNS.VALUE, main="Walk With Runner on First", col="blue")
  abline(v=mean.walk.100, lwd=3)
  axis(side=1, at=seq(-.3,.9,.2), labels=seq(-.3,.9,.2))
  axis(side=2, at=seq(1,5,2), labels=seq(1,5,2))
  text(.35, 6, "Mean Runs Value", pos=4)

# Histogram of HBP
  truehist(HBP.100$RUNS.VALUE, main="Hit By Pitch With Runner on First", col="red2")
  abline(v=mean.HBP.100, lwd=3)
  text(.38, 3.5, "Mean Runs Value", pos=4)

# Histogram of Single
  truehist(single.100$RUNS.VALUE, main="Single With Runner on First", col="violet")
  abline(v=mean.single.100, lwd=3)
  axis(side=1, at=seq(-.3,1.5,.1), labels=seq(-.3,1.5,.1))
  axis(side=2, at=seq(1,5,2), labels=seq(1,5,2))
  text(.4234, 5, "Mean Runs Value", pos=4)
dev.off()

