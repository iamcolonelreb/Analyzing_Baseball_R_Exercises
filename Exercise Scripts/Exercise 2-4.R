##############################################
# Exercise 2.4: Pitchers in the 350 Wins Club, Continued
#
# Needs the table of 9 pitchers who have achieved 350 wins that is given in exercise.
#   Table includes the Wins, Losses, Strikeouts, and Walks for:
#     Pete Alexander, Roger Clemens, Pud Galvin, Walter Johnson, Greg Maddux,
#     Christy Mathewson, Kid Nichols, Warren Spahn, and Cy Young
#
##############################################

# Part (a): Place strikeout, walk, and last names into vectors SO, BB, and Name respectively.
SO <-c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803)
BB <-c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217)
Name <-c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Sphan", "Young")

# Part (b): Calculate the Strikeout to Walk ratio (SO/BB) and place in vector SO.BB.ratio
SO.BB.ratio <- SO/BB

# Part (c): Place vectors Name, SO, BB, and SO.BB.ratio into data frame named SO.BB
SO.BB <- data.frame(Name, SO, BB, SO.BB.ratio)

# Part (d): Use the subset function to find pitchers who had SO/BB > 2.8, I named the vector SO.BB.hi
SO.BB.hi <- subset(SO.BB, SO.BB.ratio >= 2.8)

# Part (e): Sort the data frame by the number of walks using the order function.
SO.BB <- SO.BB[order(SO.BB$BB, decreasing =TRUE), ]
