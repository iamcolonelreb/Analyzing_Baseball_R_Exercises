##---------------------Exercise 5-4: Create Probability of a Run Matrix---------
##  
## Needs: events2013.csv
## These are located in the data folder, 2013 events are used to be up to
## date and compare how the run scoring environment has changed in recent years  

# Create a matrix of run expectancy where at least 1 run scores
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
  data2013 <- subset(data2013, (STATE!=NEW.STATE) | (RUNS.SCORED>1))
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

# Here is how the Run Expectancy Matrix looks when we restrict for at least 1 
# run scoring   
  RUNS.outs
# row.names	0 outs 1 out	2 outs
#  1	000	 0.43	 0.21	 0.07
#  2	001	 1.29	 0.91	 0.34
#  3	010	 1.04	 0.57	 0.24
#  4	011	 1.99	 1.36	 0.54
#  5	100	 0.81	 0.49	 0.21
#  6	101	 1.76	 1.07	 0.44
#  7	110	 1.34	 0.79	 0.34
#  8	111	 2.02	 1.35	 0.61