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

# Check for absence of a linear relationship to determine transformations
# Plots the response against each predictor in matrix m
plots <- function(response, predictors, labels) {
  for (i in 1:ncol(predictors)) {
    png(paste("Life Expectancy vs ", labels[i], ".png"))
    plot(
        predictors[, i],
        response,
        main = paste("Life Expectancy vs ", labels[i]),
        xlab = labels[i],
        ylab = "Life Expectancy"
    )
    dev.off()
  }
}

# Create matrix with quantitative predictors
num_predictors <- 14
m <- array(
  c(
    who$Adult.Mortality,
    who$infant.deaths,
    who$percentage.expenditure,
    who$Hepatitis.B,
    who$Measles,
    who$BMI,
    who$under.five.deaths,
    who$Polio,
    who$Diphtheria,
    who$HIV.AIDS,
    who$GDP,
    who$Population,
    who$Income.composition.of.resources,
    who$Schooling),
  dim = c(nrow(who), num_predictors)
)
# Create vector of labels for quantitative predictors
labels <- c(
  "Adult Mortality",
  "Infant Deaths",
  "Percentage Expenditure",
  "Hepatitis B",
  "Measles",
  "BMI",
  "Deaths of Children Under 5",
  "Polio",
  "Diphtheria",
  "HIV AIDS",
  "GDP",
  "Population",
  "Income Composition of Resources",
  "Schooling")
# Call plots function
plots(who$Life.expectancy, m, labels)

# Delete unrelated predictors
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

# Variable Transformations
who$GDP <- log(who$GDP)
who$HIV.AIDS <- -exp(-(who$HIV.AIDS))
who$Measles <- log(who$Measles)

# Plot Response vs Transformed Predictors
plots(
  who$Life.expectancy,
  cbind(who$Measles, who$HIV.AIDS, who$GDP),
  c("log(Measles)", "exp(HIV AIDS)", "log(GDP)")
)

# Variable Selection
library(leaps)
attach(who)
n <- nrow(who)
predictors <- cbind(
  BMI,
  Polio,
  Diphtheria,
  HIV.AIDS,
  GDP,
  Income.composition.of.resources,
  Schooling
)
forward <- regsubsets(
    x = predictors,
    y = Life.expectancy,
    method = "forward"
)
summary_forward <- summary(forward)
backward <- regsubsets(
    x = predictors,
    y = Life.expectancy,
    method = "backward"
)
summary_backward <- summary(backward)
exhaustive <- regsubsets(
    x = predictors,
    y = Life.expectancy,
    method = "exhaustive",
    all.best = FALSE,
    nbest = 3
)
summary_exhaustive <- summary(exhaustive)

# Extract Statistics
R2 <- summary_exhaustive$rsq
AdjR2 <- summary_exhaustive$adjr2
SSRes <- summary_exhaustive$rss
Cp <- summary_exhaustive$cp
models <- summary_exhaustive$which

# Calculate number of parameters for each model
p <- apply(models, 1, sum)

# Calculate MSRes for each model
MSRes <- SSRes / (n - p)

# Create table of models and statistics
table <- cbind(p, models, R2, AdjR2, MSRes, Cp, p - Cp)
colnames(table)[ncol(table)] <- "p - Cp"

# Check Multicollinearity
correlation <- cor(predictors)
VIF <- solve(cor(predictors))