library(dplyr)
library(mosaic)
library(ggplot2)

income <- read.csv("incomes_cleaned.csv", check.names = FALSE)

# filter by the first 11 rows of each geographic region that is not Canada 
# 11 since these 11 rows have data of all household sizes

# The difference between these rows are household types, e.g. Couple with 
# children, without children, one parent, two parent, etc

income_sub <- income%>%
  filter(GEO != 'Canada') %>%
  filter(!is.na(`Household type including census family structure  (11)`))

# exclude any rows where income is 0 
income_sub <- income%>%
  filter(
    !is.na(`Median household after-tax income (2020)`),
    !is.na(`Median household after-tax income (2015)`),
    `Median household after-tax income (2020)` != 0,
    `Median household after-tax income (2015)` != 0
  )

inc2020 <- income_sub$`Median household after-tax income (2020)`
inc2015 <- income_sub$`Median household after-tax income (2015)`

mean_median_2020 <- mean(inc2020)
mean_median_2015 <- mean(inc2015)

var_median_2020  <- var(inc2020)   
var_median_2015  <- var(inc2015)

mean_median_2020
mean_median_2015

var_median_2020
var_median_2015

n2020 = 156189
n2015 = 156189

# Two sample t-test (unequal variances)

test <- t.test(inc2020, inc2015, alternative = "two.sided", var.equal = FALSE)

p_val <- test$p.value

p_val

# Since P-value is smaller than 0.05, we reject H0 at 5% significance, and 
# conclude that there is a difference in median after tax household income 
# between 2020 and 2015.



# Assumptions

## Data is continuous
### Uh does someone know how to do this one

## Randomly sampled from a population
### This one is probably true since it's data from a census

## Homogeneity of variance
### can see that its similar enough

## Distribution is approximately normal.

### We use boxplot, histogram and QQ plot for this

# 3 by 2 panel 
par(mfrow=c(3,2))

box2020 = boxplot(inc2020, ylab = "2020 Median After Tax Incomes", main = "Boxplot of 2020 Median After Tax Income")
box2015 = boxplot(inc2015, ylab = "2015 Median After Tax Incomes", main = "Boxplot of 2015 Median After Tax Income")

hist2020 = hist(inc2020, xlab = "Median 2020 incomes", main = "Histogram of 2020 median after taxincomes")

hist2015 = hist(inc2015, xlab = "Median 2015 incomes", main = "Histogram of 2015 median after tax incomes")

qq2020 <- qqnorm(inc2020, main = "Normal Q-Q Plot for 2020 Median After Tax Income")
qqline2020 <- qqline(inc2020,col = "red")

qq2015 <- qqnorm(inc2015, main = "Normal Q-Q Plot for 2015 Median After Tax Income")
qqline2015 <- qqline(inc2015,col = "red")


# IT DOES NOT LOOK NORMAL :((((((((



