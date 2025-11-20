library(dplyr)
library(ggplot2)

# Question: Is there a significant difference in income between households of 
# 5 or more people and 4 persons (in 2020)?



income <- read.csv("incomes_cleaned.csv", check.names = FALSE)

income_sub <- income %>%
  filter(GEO != 'Canada') %>%
  filter(
    `Household size (7)` %in% c("4 persons", "5 or more persons"),
    !is.na(`Median household total income (2020)`),
    `Median household total income (2020)` != 0
  )

inc4 <- income_sub %>%
  filter(`Household size (7)` == "4 persons") %>%
  pull(`Median household total income (2020)`)

inc5p <- income_sub%>%
  filter(`Household size (7)` == "5 or more persons") %>%
  pull(`Median household total income (2020)`)

mean4 <- mean(inc4)
mean5p <- mean(inc5p)

var4 <- var(inc4)
var5p <- var(inc5p)

mean4
mean5p

var4
var5p


# Two Sample t-test (unequal variances)
## T-test since we do not know the population variance 

test <- t.test(inc4, inc5p, alternative = "two.sided", var.equal = FALSE)
test

p_val <- test$p.value
p_val

## Since the p-value is less than 0.05, we reject H0, and conclude that there is 
## there is a difference in income between household of 4 persons and households 
## of 5 or more persons


# Assumptions

## Data is continuous


## Data is independent
### Probably true since the two groups are very distinct (4 and 5 or more persons household)


## Data is normally distributed 
### We use boxplot, histogram, and qq plot
par(mfrow=c(3,2))

box4 = boxplot(inc4, ylab = "Median income", main = "Boxplot of Median Income of 4 person housholds")
box5p = boxplot(inc5p, ylab = "Median income", main = "Boxplot of Median Income of 5 or more person housholds")

hist4 = hist(inc4, xlab = "Median Income", main = "Histogram of Median Income of 4 Person Households")
hist5p = hist(inc5p, xlab = "Median Income", main = "Histogram of Median Income of 5 or more Person Households")

qq4 <- qqnorm(inc4, main = "Normal Q-Q Plot for Median income of 4 Person Households")
qqline4 <- qqline(inc4,col = "red")

qq5p <- qqnorm(inc5p, main = "Normal Q-Q Plot for Median income of 5 or more Person Households")
qqline5p <- qqline(inc5p, col = "red")

### From the boxplot and the qq-plot, we can see that the t-test assumptions are not satisfied (It's over :((( )