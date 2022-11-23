who <- read.csv(file = 'life_data.csv')
who <- who[who$Year == 2015,]

#Re-code 'Status' (0 for developing, 1 for developed)
who[who$Status == 'Developing', 'Status'] <- 0
who[who$Status == 'Developed', 'Status'] <- 1

#Drop alcohol, totalexp col, too many NA
who <- who[, c(1:6, 8:13, 15:22)]

#import leaps for model/variable selection
library(leaps)
forward <- regsubsets(x = who[, c(3, )])

#plot all predictors against response
