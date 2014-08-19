##############################################
# Excercise 2.1: Top Base Stealers in the Hall of Fame
#
# Needs the table of the 9 players given in the exercise. 
# It gives the career stolen bases, caught stealing, and games for the following players: 
# Ricky Henderson, Lou Brock, Ty Cobb, Eddie Collins, Max Carey, Joe Morgan, Luis Aparicio, Paul Molitor, and Roberto Alomar.
#
##############################################
# part(a): Place the stolen base, caught stealing, and game counts in the vectors SB, CS, G.
SB <- c(1406,938,897,741,738,689,506,504,474)
CS <- c(335,307,212,195,109,162,136,131,114)
G <- c(3081,2616,3034,2826,2476,2649,2599,2683,2379)

# part(b): Compute the number of stolen base attempts and store in the vector SB.Attempt 
SB.Attempt <- SB + CS

# part(c): Compute the success rate for all players.
Success.Rate <- SB / SB.Attempt

# part(d): Compute the number of stolen bases per game.
SB.Game <- SB / G

# part(e): Construct a scatterplot of the stolen bases per game against the success rates.
plot(SB.Game, Success.Rate)
