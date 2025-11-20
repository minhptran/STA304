library(dplyr)
library(ggplot2)

# Question: Is there a difference in median income between Households with and 
# without children?

# Do households with children earn more than households without children?
income <- read.csv("incomes_cleaned.csv", check.names = FALSE)

income_sub <- income %>%
  filter(GEO != 'Canada') %>%
  filter(
    `Household type including census family structure  (11)` %in% c("Without children", "With children"),
    !is.na(`Median household total income (2020)`),
    `Median household total income (2020)` != 0
  )

without_children <- income_sub %>%
  filter(`Household type including census family structure  (11)` == "Without children") %>%
  pull(`Median household total income (2020)`)

with_children <- income_sub %>%
  filter(`Household type including census family structure  (11)` == "With children") %>%
  pull(`Median household total income (2020)`)

mean_without_children <- mean(without_children)
mean_with_children <- mean(with_children)

var_without <- var(without_children)
var_with <- var(with_children)

summary(without_children)
summary(with_children)

var_without
var_with


# Two Sample t-test (unequal variances)
## T-test since we do not know the population variance 

test <- t.test(without_children, with_children, alternative = "two.sided", var.equal = FALSE)
test

p_val <- test$p.value
p_val

## Since the p-value of our t-test is less than 0.05, we reject H0, and conclude
## that there is a difference between the income of households with children and
## without children



# Assumptions

## Data is continuous



## Data is independent
### This is probably true since people with children wouldn't report that they 
### don't have children when replying to a census, and vice versa



## Data is Normally distributed
## We will use boxplots, histograms, and qq-plots to check this assumption


### Families without children
# par(mfrow=c(3,1))

withoutbox <- boxplot(without_children, ylab = "Median income", main = "Boxplot of Median Income of Households Without Children in 2020")

withoutHist <- hist(without_children, xlab = "Median Income", main = "Histogram of Median Income of Households Without Children in 2020")

qqwithout <- qqnorm(without_children, main = "Normal Q-Q Plot for Median Income of Household Without Children")
qqlinewithout <- qqline(without_children, col = "red")

### Families With Children

withbox <- boxplot(with_children, ylab = "Median income", main = "Boxplot of Median Income of Households With Children in 2020")

withHist <- hist(with_children, xlab = "Median Income", main = "Histogram of Median Income of Households With Children in 2020")

qqwith <- qqnorm(with_children, main = "Normal Q-Q Plot for Median Income of Household Without Children")
qqlinewith <- qqline(with_children, col = "red")
