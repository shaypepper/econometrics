setwd('/Users/shaypepper/Documents/school/econometrics')
rm(list = ls(all = TRUE)) # clear workspace

load("data/pums/pums_NY.RData")
attach(dat_pums_NY)

library(plyr)

bronx       <- subset(dat_pums_NY, in_Bronx == 1 )
manhattan  <- subset(dat_pums_NY, in_Manhattan == 1)
statenI  <- subset(dat_pums_NY, in_StatenI == 1)
brooklyn  <- subset(dat_pums_NY, in_Brooklyn == 1)
queens        <- subset(dat_pums_NY, in_Queens == 1)
westchester  <- subset(dat_pums_NY, in_Westchester == 1)
nassau   <- subset(dat_pums_NY, in_Nassau == 1)


rm(list = ls(all = TRUE)) # clear workspace
SPData <- read.csv(
  file="data/S&P500-tenyears.csv", 
  header=TRUE, 
  sep=",", 
  dec=".", 
  stringsAsFactors=FALSE)

names(SPData)[7] <- "Change"
N <- nrow(SPData)


# Order by date
SPData["Date"] <- as.Date(SPData$Date,
                          format("%b %d, %Y"))
attach(SPData)
SPData <- SPData[order(Date),]

# Convert Change string to numeric
SPData["Change"] <- as.numeric(gsub("%", "", SPData$Change))

positive <- SPData$Change > 0
streaks <- c(1:nrow(SPData))

for (i in streaks){
  tally <- 0
  for (j in i:1) {
    if (!positive[j]){
      break
    }
    tally <- tally + 1
  }
  streaks[i] <- tally
}
SPData$PositiveChange <- positive
SPData$NConsecutivePositives <- streaks

CalcData <- data.frame()

running.total <- 0

m <- max(SPData$NConsecutivePositives)
for (i in m:0){
  n <- count(SPData$NConsecutivePositives == i)
  n <- n$freq[2]
  running.total <- running.total + n
  row.data <- c(
    i,
    n,
    running.total,
    1 - n/running.total
  )
  CalcData <- rbind(row.data, CalcData)
}

names(CalcData) <- c("ConsecutivePositiveDays", "N", "CumulativeSum", "p")
