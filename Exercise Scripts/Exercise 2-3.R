##############################################
# Exercise 2.3: Pitchers in the 350 Wins Club
#
# Needs the table of 9 pitchers who have achieved 350 wins that is given in exercise.
#   Table includes the Wins, Losses, Strikeouts, and Walks for:
#     Pete Alexander, Roger Clemens, Pud Galvin, Walter Johnson, Greg Maddux,
#     Christy Mathewson, Kid Nichols, Warren Spahn, and Cy Young
#
##############################################
# part (a): Place Wins, Losses, and Names into vectors W, L, and Name respectively.
W <-c(373,354,364,417,355,373,361,363,511)
L <-c(208,184,310,279,227,188,208,245,316)
Name <-c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Sphan", "Young")

# part (b): Compute winning percentage and place in vector Win.Pct
Win.Pct <- 100 * W / (W+L)

# part (c): Use given command to place all vectors into data frame Wins.350
Wins.350 <- data.frame(Name, W, L, Win.Pct)

# part (d): order the created data frame from part c (Wins.350) by winning percentage
Wins.350 <- Wins.350[order(Wins.350$Win.Pct, decreasing =TRUE), ]
