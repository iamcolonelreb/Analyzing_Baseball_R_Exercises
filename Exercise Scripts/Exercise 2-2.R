##############################################
# Exercise 2.2: Character, Factor, and Logical Variables in R
#
# Needs the 10 hypothetical outcomes of a batter in 10 plate appearances given in the exercise.
# 
##############################################
# part (a): Use the c function to collect outcomes into a character vector called outcomes.
outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")

# part (b): Use the table function to construct a frequency table of outcomes.
table(outcomes)

# part (c): Use the given code to convert the charcter vector into a factor variable f.outcomes
#           Then use the table function on f.outcomes to compare changes with (b). 
f.outcomes <- factor(outcomes,
  levels=c("Out", "Walk", "Single", "Double"))
table(f.outcomes)

# part(d): Isolate the scenario of a walk from the outcomes. 
outcomes == "Walk"
sum(outcomes == "Walk")
