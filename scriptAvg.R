library(dplyr)
library(readr)

# Load data
income <- read_csv("incomes_cleaned.csv")

# Clean column names
names(income) <- make.names(names(income))

# Explicit list of provinces (NOT territories)
provinces <- c(
  "Ontario",
  "Quebec",
  "British Columbia",
  "Alberta",
  "Manitoba",
  "Saskatchewan",
  "Nova Scotia",
  "New Brunswick",
  "Newfoundland and Labrador",
  "Prince Edward Island"
)

# Compute averages per province for 2015 + 2020
avg_table <- income %>%
  filter(GEO %in% provinces) %>%           # explicit filter
  group_by(GEO) %>%                        # group by province
  summarise(
    avg_2015 = mean(Median.household.after.tax.income..2015., na.rm = TRUE),
    avg_2020 = mean(Median.household.after.tax.income..2020., na.rm = TRUE)
  ) %>%
  ungroup()

avg_table
