# Authors: Brenda Lopez Rodas and Rajiv Iyengar
# Life Expectancy Regression Analysis

# Import WHO Life Expectancy data
who <- read.csv(file = 'life_data.csv')
who <- who[who$Year == 2015,]

# Re-code 'Status' (0 for developing, 1 for developed)
who[who$Status == 'Developing', 'Status'] <- 0
who[who$Status == 'Developed', 'Status'] <- 1

# Drop columns alcohol and totalexp (too many NA values)
who <- who[, c(1:6, 8:13, 15:22)]
who <- who[c('Life.expectancy',
             'Status',
             'percentage.expenditure', 
             'Hepatitis.B',
             'Measles',
             'BMI',
             'Polio',
             'GDP',
             'Income.composition.of.resources',
             'HIV.AIDS',
             'Diphtheria',
             'Schooling')]
# Delete rows with NA values
who <- na.omit(who)

# Check for absence of a linear relationship to determine transformations
# # Plots the response against each predictor in matrix m
# plots <- function(y, m, labels) {
#   for (i in 1:ncol(m)) {
#     png(paste("Life Expectancy vs ", labels[i], ".png"))
#     plot(m[,i], y,
#          main = paste("Life Expectancy vs ", labels[i]),
#          xlab = labels[i],
#          ylab = "Life Expectancy")
#     dev.off()
#   }
# }

# Create matrix with quantitative predictors
# m <- array(
#   c(
#     who$Adult.Mortality,
#     who$infant.deaths,
#     who$percentage.expenditure,
#     who$Hepatitis.B,
#     who$Measles,
#     who$BMI,
#     who$under.five.deaths,
#     who$Polio,
#     who$Total.expenditure,
#     who$Diphtheria,
#     who$HIV.AIDS,
#     who$GDP,
#     who$Population,
#     who$Income.composition.of.resources,
#     who$Schooling),
#   dim = c(nrow(who), 15))
# Create vector of labels for quantitative predictors
# labels <- c(
  # "Adult Mortality",
  # "Infant Deaths",
  # "Percentage Expenditure",
  # "Hepatitis B",
  # "Measles",
  # "BMI",
  # "Deaths of Children Under 5",
  # "Polio",
  # "Total Expenditure",
  # "Diphtheria",
  # "HIV/AIDS",
  # "GDP",
  # "Population",
  # "Income Composition of Resources",
  # "Schooling")
# Call plots function
# plots(who$Life.expectancy, m, labels)

# Variable Transformations
who$GDP <- log(who$GDP)
who$HIV.AIDS <- exp(-(who$HIV.AIDS))
who$Measles <- log(who$Measles)

# Variable Selection
library(leaps)
attach(who)

forward <- regsubsets(
    x = cbind(BMI, Polio, Diphtheria, HIV.AIDS, GDP, Income.composition.of.resources, Schooling),
    y = Life.expectancy,
    method = "forward"
)
summary_forward <- summary(forward)
backward <- regsubsets(
    x = cbind(BMI, Polio, Diphtheria, HIV.AIDS, GDP, Income.composition.of.resources, Schooling),
    y = Life.expectancy,
    method = "backward"
)
summary_backward <- summary(backward)
