##############################################
# Exercise 2.5: Pitcher Strikeout/Walk Ratios
#
# Needs the information provided by the pitching table in the Lahman database.
# The pitching table is recorded as a CSV file.
# I have provided a version located in the data folder that is up to date with 2013.
#
##############################################
# Part (a): Read the Lahman pitching.csv file into R under the data frame "Pitching"
Pitching <- read.csv("pitching.csv")

# Part (b): The following function computes the cumulative strikeouts, walks, mid career year,
# and total innings pitched for a pitcher whose season stats are stored in the data frame d.
# Using the function ddply together with the function stats, find the career stats for all pitchers. 
# Call this new data frame career.pitching
stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE)
  c.BB <- sum(d$BB, na.rm=TRUE)
  c.IPouts <- sum(d$IPouts, na.rm=TRUE)
  c.midYear <- median(d$yearID, na.rm=TRUE)
  data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts, midYear=c.midYear)
}
career.pitching <- ddply(Pitching, .(playerID), stats)

# Part (c): Use the merge function to merge the Pitching and career.pitching data frames
Pitching <- merge(Pitching, career.pitching, by="playerID")

# Part (D): Use the subset function to construct new data frame career.10000 consisting of data pitchers of at least 10000 IPouts
career.10000 <-subset(Pitching, IPouts.y >= 10000)

# Part (E): Using career.10000 make a scatterplot of mid career year and ratio of strikeout to walks
with(career.10000, plot(midYear, SO.y/BB.y))
