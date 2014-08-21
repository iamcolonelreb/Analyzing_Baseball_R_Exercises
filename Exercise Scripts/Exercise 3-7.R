######################################################
####Exercise 3-7: Working with Retrosheet Play-By-Play Data
####
#### Needs: retrosheet retrosheet1998.csv, and retrosheetIDs.csv
####### These are located in the data folder
####### The retrosheet event files have fields included
####### as such one will not need fields.csv
####
####
####################################################
#PART (A)###########################################
#Load retrosheet information into R and create sosa.data and mac.data like in Section 3.9
  data1998 <- read.csv("retrosheet1998.csv")
  retro.ids <- read.csv("retrosheetIDs.csv")
    ## isolate sosa.id and mac.id
    sosa.id <- as.character(subset(retro.ids,
                   first=="Sammy" & last=="Sosa")$id)
    mac.id <- as.character(subset(retro.ids,
                   first=="Mark" & last=="McGwire")$id)
    ## create data frames
    sosa.data <- subset(data1998, BAT_ID == sosa.id)
    mac.data <- subset(data1998, BAT_ID == mac.id)
####################################################
#PART (B)###########################################
#Restrict the two data frames to only where batting events occured
  mac.data <- subset(mac.data, BAT_EVENT_FL == TRUE)
  sosa.data <- subset(sosa.data, BAT_EVENT_FL == TRUE)
####################################################
#PART (C)###########################################
#Create the new variable for plate appearances (PA)
  mac.data$PA <- 1:nrow(mac.data)
  sosa.data$PA <- 1:nrow(sosa.data)
####################################################
#Part (D)###########################################
#Find # of PAs when they hit home runs##############
  mac.HR.PA <- mac.data$PA[mac.data$EVENT_CD==23]
  mac.HR.PA
  sosa.HR.PA <- sosa.data$PA[sosa.data$EVENT_CD==23]
  sosa.HR.PA
####################################################
#Part (E)###########################################
#Compute the spacings between home runs using diff##
  mac.spacings <- diff(c(0, mac.HR.PA))
  mac.spacings
  sosa.spacings <- diff(c(0, sosa.HR.PA))
  sosa.spacings
####################################################
#Part (F)###########################################
#Use summary and hist to compare home run spacings
  summary(mac.spacings)
  summary(sosa.spacings)
  ## Use pdf function to capture both histograms
  pdf("Exercise 3-7 McGwire and Sosa HR Spaceings.pdf")
  ## Create McGwire Histogram of HR Spaceings, I add an x-axis for clarity
  hist(mac.spacings, xlab="Number of Plate Appearances", col="red2", main="McGwire Home Run Spacings",
       breaks=seq(0,45, by=5))
  axis(side=1, at=seq(0,40,5), labels=seq(0,40,5))
  ## Create Sosa histogram of HR Spaceings, I add an x-axis for clarit
  hist(sosa.spacings, xlab="Number of Plate Appearances", col="blue", main="Sosa Home Run Spacings", 
       breaks=seq(0,50, by=5))
  axis(side=1, at=seq(0,50,5), labels=seq(0,50,5))
  ## To close opened pdf use dev.off() function
  dev.off()
