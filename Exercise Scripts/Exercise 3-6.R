#####################################################
#
# Exercise 3.6: Working with the Lahman Batting Dataset
# 
# Needs the Lahman "Master.csv", and the "Batting.csv" data files.
# This is where you can find the csv files:
#   http://www.seanlahman.com/baseball-archive/statistics/
# To save you the trouble I have added the needed files to the data folder.
# 
#
#####################################################
#####################################################

# Part(A): Read the Master and batting csv files into R
  master <- read.csv("Master.csv", stringsAsFactors=FALSE)
  batting <- read.csv("Batting.csv", stringsAsFactors=FALSE)
  

# Part(B): Use getinfo to obtain season statistics for Ty Cobb, Ted Williams, and Pete Rose
  getinfo <- function(firstname, lastname){
   playerline <- subset(master,
                       nameFirst==firstname & nameLast==lastname)
    name.code <- as.character(playerline$playerID)
    birthyear <- playerline$birthYear
    birthmonth <- playerline$birthMonth
    birthday <- playerline$birthDay
    byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
    list(name.code=name.code, byear=byear)}
  cobb.info <- getinfo("Ty","Cobb")
  cobb.info
  cobb.data <- subset(batting, playerID == cobb.info$name.code)
  williams.info <- getinfo("Ted", "Williams")
  williams.info
  williams.data <- subset(batting, playerID == williams.info$name.code)
  rose.info <- getinfo("Pete", "Rose")
  rose.info
  rose.data <- subset(batting, playerID == "rosepe01")
# since there were two pete roses in MLB history we must specify the correct playerID

# Part (C): Add the variable Age to the created data frames.
  cobb.data$Age <- cobb.data$yearID - cobb.info$byear
  williams.data$Age <- williams.data$yearID - williams.info$byear
  rose.data$Age <- rose.data$yearID - 1941

# Part (D): Plot a line graph of the cumulative hit totals against age
  with(rose.data, plot(Age, cumsum(H), type="l", col="blue", lty=3, lwd=2,
                       xlab="Age", ylab="Career Hits",
                       xlim=c(18,45), ylim=c(0,4300)))

# Part (E): Overlay hit totals for Williams and Cobb using lines function
  with(cobb.data, lines(Age, cumsum(H), lty=2,lwd=2))
  with(williams.data, lines(Age, cumsum(H), lty=1, lwd=2))
  legend(18,4000, legend=c("Williams", "Cobb", "Rose"),
         lty=1 : 4, lwd=2)
    
  