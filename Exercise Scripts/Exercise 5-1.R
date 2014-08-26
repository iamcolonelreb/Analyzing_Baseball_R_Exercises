##---------------------Exercise 5-1: Runs Values of Hits-----------------------
##  
## Needs: events2013.csv
## These are located in the data folder, 2013 events are used to be up to
## date and compare how the run scoring environment has changed in recent years


#------------------------------PART (A)-----------------------------------------
# Use similar R codes from 5.2, 5.3, and 5.8 to find mean run values of a double
# and a single

# From 5.2: Creating Runs Scored for Remainder of Inning
  data2013 <- data.table(read.csv("events2013.csv", stringsAsFactors=FALSE))
  data2013$RUNS <- with(data2013, AWAY_SCORE_CT + HOME_SCORE_CT)
  data2013$HALF.INNING <- with(data2013, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))
  data2013$RUNS.SCORED <- with(data2013, (BAT_DEST_ID > 3) +
                  (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  RUNS.SCORED.INNING <- aggregate(data2013$RUNS.SCORED, 
                                list(HALF.INNING = data2013$HALF.INNING), sum)
  RUNS.SCORED.START <- aggregate(data2013$RUNS, 
                               list(HALF.INNING = data2013$HALF.INNING), "[", 1)
  MAX <- data.table(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data2013<-left_join(data2013,MAX)
  data2013<-left_join(data2013,MAX)
  # perform left_join, merge slowed my computer down to a halt, and froze R
  # if you know of a more efficient way feel free to elaborate and change
  N <- ncol(data2013)
  names(data2013)[N] <- "MAX.RUNS"
  data2013$RUNS.ROI <- with(data2013, MAX.RUNS - RUNS)
  head(data2013)

# From 5.3:Creating the Run Expectancy Matrix
  get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
  }
  RUNNER1 <- ifelse(as.character(data2013[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data2013[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data2013[,"BASE3_RUN_ID"])=="", 0, 1)
  data2013$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2013$OUTS_CT)
  NRUNNER1 <- with(data2013, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data2013, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
  NRUNNER3 <- with(data2013, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                        RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data2013, OUTS_CT + EVENT_OUTS_CT)
  data2013$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  data2013 <- subset(data2013, (STATE!=NEW.STATE) | (RUNS.SCORED>0))
  data.outs <- ddply(data2013, .(HALF.INNING), summarize,
                   Outs.Inning = sum(EVENT_OUTS_CT))
  data.outs <- data.table(data.outs)
  data2013 <- merge(data2013, data.outs)
  head(data2013)
  data2013C <- subset(data2013, Outs.Inning == 3)
  RUNS <- with(data2013C, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

# From 5.4: Finding the values of events
  RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3",
                          "010 3","011 3","100 3","101 3","110 3","111 3") 
  data2013$RUNS.STATE <- RUNS.POTENTIAL[data2013$STATE,]
  data2013$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2013$NEW.STATE,]
  data2013$RUNS.VALUE <- data2013$RUNS.NEW.STATE - data2013$RUNS.STATE + 
    data2013$RUNS.SCORED

# Computing Mean Run Value for Double
  d.double <- subset(data2013, EVENT_CD == 21)
  table(d.double$STATE)
  round(prop.table(table(d.double$STATE)), 3)
  library(MASS)
  truehist(d.double$RUNS.VALUE)
  mean.double <- mean(d.double$RUNS.VALUE)
  mean.double
  abline(v=mean.double, lwd=3)
  text(.8, 2, "Mean Runs Value", pos=4)

# Computing Mean Run Value for Singles
  d.single <- subset(data2013, EVENT_CD == 20)
  table(d.single$STATE)
  round(prop.table(table(d.single$STATE)), 3)
  library(MASS)
  truehist(d.single$RUNS.VALUE, col="red2")
  axis(side=1, at=seq(-.5,3,.5), labels=seq(-.5,3,.5))
  mean.single <- mean(d.single$RUNS.VALUE)
  mean.single
  abline(v=mean.double, lwd=3)
  text(.8, 5, "Mean Runs Value", pos=4)

#----------------------------PART (B)------------------------------------------

# The mean run value for a double was found to be 0.741, this is significantly
# lower than what was found by Alber and Bennet in 2001. Also the value of a 
# single in 2013 was found to be .438. This value is also lower than Albert and 
# Bennet's value of 0.46. The run scoring environment has certainly changed
# since 2001. If one were to compare other batting events to Albert and Bennet's
# they woul find that these events have a comparitively lower run values as well
