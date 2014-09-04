##-------Exercise 5-3: Comparing Two Players With Similar OBPS---------------
##
## Needs: The retrosheet data: rosters2011.csv, events2011.csv
## and the Lahman data: batting2011.csv

#-----Find the Runs Scored in the Remainder of Inning Like Before--------------
  library("data.table")
  data2011 <- data.table(read.csv("events2011.csv", stringsAsFactors=FALSE))
  data2011$RUNS <- with(data2011, AWAY_SCORE_CT + HOME_SCORE_CT)
  data2011$HALF.INNING <- with(data2011, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))
  data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) +
                  (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  RUNS.SCORED.INNING <- aggregate(data2011$RUNS.SCORED, 
                                list(HALF.INNING = data2011$HALF.INNING), sum)
  RUNS.SCORED.START <- aggregate(data2011$RUNS, 
                              list(HALF.INNING = data2011$HALF.INNING), "[", 1)
  MAX <- data.table(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  library("dplyr")
  data2011<-left_join(data2011,MAX)
  data2011<-left_join(data2011,MAX)
  # perform left_join, merge slowed my computer down to a halt, and froze R
  # if you know of a more efficient way feel free to elaborate and change
  N <- ncol(data2011)
  names(data2011)[N] <- "MAX.RUNS"
  data2011$RUNS.ROI <- with(data2011, MAX.RUNS - RUNS)
  head(data2011)

#--------------Calculate the Run Values of all 2011 events-------------------
  data2011 <- data.table(data2011)
  get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
  }
  RUNNER1 <- ifelse(as.character(data2011[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data2011[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data2011[,"BASE3_RUN_ID"])=="", 0, 1)
  data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)
  NRUNNER1 <- with(data2011, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data2011,
                as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
  NRUNNER3 <- with(data2011, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                        RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data2011, OUTS_CT + EVENT_OUTS_CT)
  data2011$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  RUNS <- with(data2011, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3",
                                   "010 3","011 3","100 3","101 3","110 3","111 3") 
  data2011$RUNS.STATE <- RUNS.POTENTIAL[data2011$STATE,]
  data2011$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2011$NEW.STATE,]
  data2011$RUNS.VALUE <- data2011$RUNS.NEW.STATE - data2011$RUNS.STATE + 
              data2011$RUNS.SCORED
  head(data2011)

#----Load roster2011 and Isolate Players Michael Bourn and Rickie Weeks--------
  Roster <- read.csv("rosters2011.csv")
# create ID for Rickie Weeks and Michael Bourn
  weeks.id <- subset(Roster, FIRST_NAME_TX == "Rickie" & 
                     LAST_NAME_TX == "Weeks")$PLAYER_ID
  weeks.id <- as.character(weeks.id)
  bourn.id <- subset(Roster, FIRST_NAME_TX == "Michael" & 
                     LAST_NAME_TX == "Bourn")$PLAYER_ID
  bourn.id <- as.character(bourn.id)
# now that we have isolated the players, we can calculate their contributions

#---------------------------Rickie Weeks----------------------------------------
  weeks <- subset(data2011, BAT_ID == weeks.id)
  weeks <- subset(weeks, BAT_EVENT_FL == TRUE)
  weeks$RUNNERS <- substr(weeks$STATE, 1, 3)
  with(weeks, stripchart(RUNS.VALUE ~ RUNNERS, main="Rickie Weeks PA Values",
                         vertical=TRUE, jitter=0.2, 
                         xlab = "Runners", ylab= "Runs Value",
                         method="jitter", pch=1, cex=0.8))
  abline(h=0)
  W.runs <- aggregate(weeks$RUNS.VALUE, list(weeks$RUNNERS), sum)
  names(W.runs)[2] <- "RUNS"
  W.PA <- aggregate(weeks$RUNS.VALUE, list(weeks$RUNNERS), length)
  names(W.PA)[2] <- "PA"
  W <- merge(W.PA, W.runs)
  W
#  Group.1  PA       RUNS
# 1     000 380 10.0811550
# 2     001   8  0.5965257
# 3     010  50  4.1459274
# 4     011   7  1.2153241
# 5     100  63  0.4863761
# 6     101  19 -2.0874471
# 7     110  25 -4.2302879
# 8     111   8 -6.7424980
  sum(W$RUNS)
# [1] 3.465075
# Now to find his OBP, BA, and SLG it is easier to load the Lahman batting csv
  batting2011 <- read.csv("batting2011.csv")
  weeks.stats <- subset(batting2011, playerID == "weeksri01")
# Rickie Weeks On Base Percentage
  weeks.stats$PA <- with(weeks.stats, AB + BB + SH + SF + HBP)
  weeks.stats$OBP <- with(weeks.stats, (H + BB + HBP) / PA)
  weeks.stats$OBP
# [1] 0.3495146
# Rickie Weeks Batting Average
  weeks.stats$BA <- with(weeks.stats, H / AB)
  weeks.stats$BA
# [1] 0.2693157
# Rickie Weeks Slugging Percentage
  weeks.stats$X1B <- with(weeks.stats, H - X2B - X3B - HR)
  weeks.stats$TB <- with(weeks.stats, (X1B * 1 + X2B * 2 + X3B * 3 + HR * 4))
  weeks.stats$SLG <- with(weeks.stats, TB / AB)
  weeks.stats$SLG
# [1] 0.4679912

#----------------------Michael Bourn---------------------------------
  bourn <- subset(data2011, BAT_ID == bourn.id)
  bourn <- subset(bourn, BAT_EVENT_FL == TRUE)
  bourn$RUNNERS <- substr(bourn$STATE, 1, 3)
  with(bourn, stripchart(RUNS.VALUE ~ RUNNERS, main="Michael Bourn PA Values",
                       vertical=TRUE, jitter=0.2, 
                       xlab = "Runners", ylab= "Runs Value",
                       method="jitter", pch=1, cex=0.8))
  abline(h=0)
  B.runs <- aggregate(bourn$RUNS.VALUE, list(bourn$RUNNERS), sum)
  names(B.runs)[2] <- "RUNS"
  B.PA <- aggregate(bourn$RUNS.VALUE, list(bourn$RUNNERS), length)
  names(B.PA)[2] <- "PA"
  B <- merge(B.PA, B.runs)
  B
# Group.1  PA       RUNS
# 1     000 488  1.0593489
# 2     001  14  2.8683266
# 3     010  59  0.5654683
# 4     011  20  4.1035180
# 5     100  77 -1.2025645
# 6     101  17 -4.3777439
# 7     110  33  1.7239558
# 8     111  14  1.5853032
  sum(B$RUNS)
# [1] 6.325612
# Now to find his OBP, BA, and SLG it is easier to load the Lahman batting csv
  bourn.stats <- subset(batting2011, playerID == "bournmi01")
# You will notice that Michael Bourn has two seperate lines of values for 2011
# This is because he was traded to Atlanta mid-season (good pick up for them)
# To deal with this we write a function to collapse the lines across the stint 
  library(plyr)
  CollapseStint <- function(d){
    G <- sum(d$G); AB <- sum(d$AB); R <- sum(d$R); H <- sum(d$H); 
    X2B <- sum(d$X2B); X3B <- sum(d$X3B); HR <- sum(d$HR); RBI <- sum(d$RBI);
    SB <- sum(d$SB); CS <- sum(d$CS); BB <- sum(d$BB); SH <- sum(d$SH); 
    SF <- sum(d$SF); HBP <- sum(d$HBP)
    data.frame(G=G, AB=AB, R=R, H=H, X2B=X2B, X3B=X3B, HR=HR, RBI=RBI, SB=SB,
               CS=CS, BB=BB, SH=SH, SF=SF, HBP=HBP)
  }
  bourn.stats <- ddply(bourn.stats, .(playerID, yearID), CollapseStint)
# Now that we have combined the seperate rows to for cumulative totals we can
# find his BA, OBP, and SLG
# Michael Bourn OBP
  bourn.stats$PA <- with(bourn.stats, AB + BB + SH + SF + HBP)
  bourn.stats$OBP <- with(bourn.stats, (H + BB + HBP) / PA)
  bourn.stats$OBP
# [1] 0.3462604
# Michael Bourn BA
  bourn.stats$BA <- with(bourn.stats, H / AB)
  bourn.stats$BA
# [1] 0.2942073
# Michael Bourn SLG
  bourn.stats$X1B <- with(bourn.stats, H - X2B - X3B - HR)
  bourn.stats$TB <- with(bourn.stats, (X1B * 1 + X2B * 2 + X3B * 3 + HR * 4))
  bourn.stats$SLG <- with(bourn.stats, TB / AB)
  bourn.stats$SLG
# [1] 0.3856707

#-------------------Elaboration------------------------------------
# Strictly by run values, Michael Bourn was more valuable to his team than
# Rickie Weeks. Bourn's value was 6.326 and Week's was 3.465. This result cannot
# be explained by the traditional batting stats alone. It is obvious from the
# matrices that Weeks performed poorly with men on base in 2011. On the other
# hand Bourn was a net postive with guys on base. Weeks could very likely 
# be the victim of random variation.
  
  