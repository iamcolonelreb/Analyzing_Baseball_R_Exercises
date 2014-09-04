##---------------------Exercise 5-5: Runner Advancement With Single-------------
##  
## Needs: "transformed data2013.RData", this has all the necessary
## transformations of the retrosheet event files that are done in the chapter 
## and also is previous exercises.

load("transformed data2013.RData")

#--------------------------------Part (A)----------------------------------
# Use the subset function to select plays when a single was hit.
# Call this data frame d.single
  d.single <- subset(data2013, EVENT_CD == 20)

#--------------------------------Part (B)----------------------------------
# Use table function to calculate frequencies of the Starting States and 
# and the resulting New States when a single is hit
  state <- table(d.single$STATE)
  state <- as.data.frame(state)
  state

  new.state <- table(d.single$NEW.STATE)
  new.state <- as.data.frame(new.state)
  new.state
#--------------------------------Part (C)----------------------------------
# Using the table made in (B), find whether a runner is more likely to advance
# to second or third
# To make the table more manageable I am removing the starting states that don't
# have a single runner on first
# subset the singles into a beginning state with a runner on first "100"
  s.100 <- subset(d.single, STATE == "100 0" | STATE == "100 1" | STATE == "100 2")
  s.100
  table(s.100$NEW.STATE, s.100$STATE)
#             START STATE
# NEW.STATE 100 0 100 1 100 2
# 000 0      1     0     0
# 000 1      1     0     0
# 000 3      0     0     1
# 001 0      2     0     0
# 001 1     11     3     0
# 001 2      0    10     4
# 001 3      0     0     8
# 010 0      6     0     0
# 010 1      6     3     0
# 010 2      0     3    14
# 011 0     17     0     0
# 011 1      0    28     0
# 011 2      0     0    19
# 100 0      2     0     0
# 100 1     12     2     0
# 100 2      0    11     8
# 100 3      0     0    19
# 101 0    366     0     0
# 101 1      0   481     0
# 101 2      0     0   578
# 110 0   1246     0     0
# 110 1      0  1455     0
# 110 2      0     0  1214
round(prop.table(table(s.100$NEW.STATE, s.100$STATE)), 5)
#                 START STATE
#NEW.STATE  100 0   100 1   100 2
#000 0    0.00018 0.00000 0.00000
#000 1    0.00018 0.00000 0.00000
#000 3    0.00000 0.00000 0.00018
#001 0    0.00036 0.00000 0.00000
#001 1    0.00199 0.00054 0.00000
#001 2    0.00000 0.00181 0.00072
#001 3    0.00000 0.00000 0.00145
#010 0    0.00108 0.00000 0.00000
#010 1    0.00108 0.00054 0.00000
#010 2    0.00000 0.00054 0.00253
#011 0    0.00307 0.00000 0.00000
#011 1    0.00000 0.00506 0.00000
#011 2    0.00000 0.00000 0.00344
#100 0    0.00036 0.00000 0.00000
#100 1    0.00217 0.00036 0.00000
#100 2    0.00000 0.00199 0.00145
#100 3    0.00000 0.00000 0.00344
#101 0    0.06617 0.00000 0.00000
#101 1    0.00000 0.08696 0.00000
#101 2    0.00000 0.00000 0.10450
#110 0    0.22528 0.00000 0.00000
#110 1    0.00000 0.26306 0.00000
#110 2    0.00000 0.00000 0.21949

# We can see from the created tables that a runner orginally located
# on first is far more likely to end up at second than at third when a single
# is hit. The runner on first moves to 2nd 70.7% of the time, and they advance 
# to third about 25% of the time

#----------------------Part (D)----------------------------
# Estimate the probability of a run scoring on a single when there are
# runners on first and second base
 s.110 <- subset(d.single, STATE == "110 0" | STATE == "110 1" | STATE == "110 2")
 table(s.110$NEW.STATE, s.110$STATE)  
# NEW.STATE   110 0 110 1 110 2
#   000 0     1     0     0
#   000 3     0     0     1
#   001 0     1     0     0
#   001 1     2     4     0
#   001 2     0     3     1
#   001 3     0     0     4
#   010 1     2     0     0
#   010 2     1     5     8
#   010 3     0     1     2
#   011 0    10     0     0
#   011 1     3    33     0
#   011 2     0     4    42
#   100 1     2     0     0
#   100 2     0    10     1
#   100 3     0     0    24
#   101 0    55     0     0
#   101 1     1   131     0
#   101 2     0     8   233
#   101 3     0     0     6
#   110 0    83     0     0
#   110 1     5   244     0
#   110 2     0    11   316
#   110 3     0     0    46
#   111 0   205     0     0
#   111 1     0   279     0
#   111 2     0     0   151

# Let's see how many runs teams scored on average when a single was hit while 
# runners were occupying first and second.
  m.single.110 <- mean(s.110$RUNS.SCORED)
  m.single.110
# [1] 0.637442

# Let's see how many runs scoring events actually occured
  s.110.RUNS <- subset(s.110, RUNS.SCORED > 0)
  rows.110.RUNS <- nrow(s.110.RUNS)
  rows.110.RUNS
# [1] 1218
  rows.110 <- nrow(s.110)
  rows.110
# [1] 1939
  rows.110.RUNS / rows.110
# [1] 0.6281588
# Teams scored 62.8% of the time when a single was hit with a man on first and
# second