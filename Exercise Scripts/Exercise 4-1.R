######################################################
####Exercise 4-1: Relationship Beween Run Differential
####        and Winning Percentage Across Decades
####
#### Needs: lahman teams.csv
####### These are located in the data folder
####
####
####################################################
#Part (A)###########################################
#Isolate the data from each era 1961-1970, 1971-1980, 1981-1990, 1991-2000, 2001-2010 using subset
#Create variables for RD (run differntial) and Wpct (winning percentage) for each era
  teams <- read.csv("teams.csv")
  tail(teams)
  myteams.1960s <- subset(teams, yearID>=1961 & yearID<=1970)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  myteams.1960s$RD <- with(myteams.1960s, R - RA)
  myteams.1960s$Wpct <- with(myteams.1960s, W / (W+L))
  myteams.1970s <- subset(teams, yearID>=1971 & yearID<=1980)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  myteams.1970s$RD <- with(myteams.1970s, R - RA)
  myteams.1970s$Wpct <- with(myteams.1970s, W / (W+L))
  myteams.1980s <- subset(teams, yearID>=1981 & yearID<=1990)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  myteams.1980s$RD <- with(myteams.1980s, R - RA)
  myteams.1980s$Wpct <- with(myteams.1980s, W / (W+L))
  myteams.1990s <- subset(teams, yearID>=1991 & yearID<=2000)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  myteams.1990s$RD <- with(myteams.1990s, R - RA)
  myteams.1990s$Wpct <- with(myteams.1990s, W / (W+L))
  myteams.2000s <- subset(teams, yearID>=2001 & yearID<=2010)[ , c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  myteams.2000s$RD <- with(myteams.2000s, R - RA)
  myteams.2000s$Wpct <- with(myteams.2000s, W / (W+L))
 ## Make scatterplot of RD and WPct for each ERA with 
 ### I have set it up to save all created graphs to save as pdf
  pdf("Exercise 4-1.pdf")
 ## 1961 - 1970
  plot(myteams.1960s$RD, myteams.1960s$Wpct, pch=1, col="red", main="Run Differential and Winning Percentage",
       sub="1961-1970",
       xlab="Run Differential", ylab = "Winning Percentage")
  linfit.1960s <- lm(Wpct ~ RD, data=myteams.1960s)
  linfit.1960s
  abline(a=coef(linfit.1960s)[1], b=coef(linfit.1960s)[2], lwd=2)
 ## 1971 - 1980
  plot(myteams.1970s$RD, myteams.1970s$Wpct, pch=1, col="green", main="Run Differential and Winning Percentage",
     sub="1971-1980",
     xlab="Run Differential", ylab = "Winning Percentage")
  linfit.1970s <- lm(Wpct ~ RD, data=myteams.1970s)
  linfit.1970s
  abline(a=coef(linfit.1970s)[1], b=coef(linfit.1970s)[2], lwd=2)
 ## 1981-1990
  plot(myteams.1980s$RD, myteams.1980s$Wpct, pch=1, col="dodgerblue", main="Run Differential and Winning Percentage",
     sub="1981-1990",
     xlab="Run Differential", ylab = "Winning Percentage")
  linfit.1980s <- lm(Wpct ~ RD, data=myteams.1980s)
  linfit.1980s
  abline(a=coef(linfit.1980s)[1], b=coef(linfit.1980s)[2], lwd=2)
 ## 1991 - 2000
  plot(myteams.1990s$RD, myteams.1990s$Wpct, pch=1, col="violet", main="Run Differential and Winning Percentage",
     sub="1991-2000",
     xlab="Run Differential", ylab = "Winning Percentage")
  linfit.1990s <- lm(Wpct ~ RD, data=myteams.1990s)
  linfit.1990s
  abline(a=coef(linfit.1990s)[1], b=coef(linfit.1990s)[2], lwd=2)
 ## 2001-2010
  plot(myteams.2000s$RD, myteams.2000s$Wpct, pch=1, col="firebrick", main="Run Differential and Winning Percentage",
      sub="2001-2010",
      xlab="Run Differential", ylab = "Winning Percentage")
  linfit.2000s <- lm(Wpct ~ RD, data=myteams.2000s)
  linfit.2000s
  abline(a=coef(linfit.2000s)[1], b=coef(linfit.2000s)[2], lwd=2)
 ## save the pdf by closing using dev.off()
  dev.off()
####################################################
#Part (B)###########################################
# Use the function predict to compare predicted winnings percentages across the eras.
# The RD (run differential) we would like to compare is +10 runs
 ## 1961-1970
  linfit.1960s
  predict.1960s <- 0.499933 + 0.000704*10
  ### A team with Run Differential of +10 is predicted to have a 0.5069 winning percentage
 ## 1971-1980
  linfit.1970s
  predict.1970s <-0.4999884 + 0.0006375*10
   ### A team with Run Differential of +10 is predicted to have a 0.5064 winning percentage
 ## 1981-1990
  linfit.1980s
  predict.1980s <- 0.4999448 + 0.0007014*10
   ### A team with Run Differential of +10 is predicted to have a 0.5069 winning percentage
 ## 1991-2000
  linfit.1990s
  predict.1990s <- 0.4999994 + 0.0006276*10
   ### A team with Run Differential of +10 is predicted to have a 0.5063 winning percentage
 ## 2000-2010 
  linfit.2000s
  predict.2000s <- 0.4999909 + 0.0006216*10
   ### A team with Run Differential of +10 is predicted to have a 0.5062 winning percentage