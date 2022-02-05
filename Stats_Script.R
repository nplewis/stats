# Load libraries
library(tidyverse)


# Chi-Square ----
# Compare observed data with expected results
# For example: is 60% male promoted unusual if 50% of workers male?
# Pro: Sample need not reflect population (non-parametric)  
# Con: Coarse; may not detect differences

## Favoritism 1 ----
# Promotions: 27 men and 13 women (men = 67.5%)
# Workforce: 110 men and 90 women (men = 55.0%)
# Evidence of favoritism or chance?

# Promotions = observed data
# Workforce = expected results

# Set promotions data as observed (any name)
observed <- c(27, 13)

# Set workforce as expected (any name) using percents as decimals
# Percents must add to 1.00
# Use same order (men, women) as in observed data
expected <- c(.55, .45)

# Chi-square test compares observed to expected
# Form: chisq.test(observed, p = expected)
chisq.test(observed, p=expected)

# How to read results
# X-squared is chi-square statistics (ignore)
# df is degrees of freedom (ignore)
# p-value is probability of result if expected were followed
# This result: 11% chance of getting these results given workforce
# Not less than 5%, so keep the null: Not evidence of favoritism


## Favoritism 2 ----
# Presume 1 women drops out, so now 27 men and 11 women 
# Change observed data to 27 and 11
observed <- c(27, 11)

# Repeat chi-square test
chisq.test(observed, p=expected)

# Read results
# Now p-value is less than 5%
# Only difference: 1 person
# Lesson: Law of small numbers


## Legislature ----
# FL House: 78 Republicans (65.0%), 42 Democrats 
# FL Senate: 24 Republicans (60.0%), 16 Democrats
# FL Legislature: 102 Repub (63.8%), 58 Dems
# FL Registered Voters: 5,169,012 GOP (49.4%), 5,303,254 Dems

# Observed = FL Legislature; Expected = FL Registered Voters
# Is the Legislature party makeup beyond chance?

# Set FL Legislature as observed (Republicans, Democrats)
observed <- c(102, 58)

# Set FL Registered Voters percentages as expected (Republicans, Democrats)
expected <- c(.494, .506)

# Chi-square test compared observed to expected
chisq.test(observed, p=expected)

# Interpret results
# p-value < 0.05, so Legislature party makeup unlikely
# But data cannot tell us why
# Factors: gerrymandering, candidates, campaigns, party strength, etc.

# T-Test ----
# Compare means between two groups
# Example: Is average pay betweeen men and women dissimilar?

## Salaries ----

# Import white_house_pay.csv
# WhiteHouse salary data as of July 1, 2021
# Source: White House PDF
wh <- read_csv("data/white_house_pay.csv")

# Compute overall mean for salaries
mean(wh$salary)

# Compute mean for each gender
wh %>%
  group_by(gender) %>% 
  summarize(meanpay = mean(salary))

# Conclusion: Mean pay for men is higher
# Question: Is that difference greater than chance?

# Plot difference as a boxplot
ggplot(wh) +
  aes(gender, salary) +
  geom_boxplot() +
  expand_limits(y = 0)

# Evaluate result: IQR nearly identical
# Thus, unlikely to be statistically significant

# Create datasets for each gender
wh_men <- wh %>% 
  filter(gender == "Male")

wh_female <- wh %>% 
  filter(gender == "Female")

# Calculate t-test statistic
# Form: t.test(variable1, variable2)
t.test(wh_men$salary, wh_female$salary)

# Interpret results
# t: Value of the t-statistic (ignore)
# df: Degrees of freedom (ignore)
# p-value: probability of result if null hypothesis true
# Result is not below 0.05, so retain the null: nothing going on

### Gender X2 ----
# White House staff: 297 women (60.1%) and 197 men
# Is that unusual?
# Full-time workforce in July 2021 (BLS data): women = 44.8%
# Chi-square test (women, men)
observed <- c(297, 197)
expected <- c(.449, .551)
chisq.test(observed, p=expected)

# Result: Yes, unusual

# What if compare to (roughly) 50% women in population?
observed <- c(297, 197)
expected <- c(.50, .50)
chisq.test(observed, p=expected)

# Yes, still unusual


## Broadway ----
# Q: Is "Hamilton" associated with higher average ticket prices?
# Source: Internet Broadway Database, ibdb.com
# Data stops in March 2020, with onset of covid pandemic

# Import broadway.csv
broadway <- read_csv("data/broadway.csv")

# Calculate overall mean ticket price
mean(broadway$avg_ticket)

# Plot difference as a boxplot
ggplot(broadway) +
  aes(hamilton, avg_ticket) +
  geom_boxplot() +
  expand_limits(y = 0)

# Evaluate results of boxplot
# IQR do not overlap, so difference likely to be significant

# Create broadway_before dataset
broadway_before <- broadway %>% 
  filter(hamilton == "Before")

# Create broadway_after dataset
broadway_after <- broadway %>% 
  filter(hamilton == "After")

# Calculate t-test statistic
# Format: t.test(variable1, variable2)
t.test(broadway_before$avg_ticket, broadway_after$avg_ticket)

# Evaluate results
# Mean price before Hamilton: $93.97
# Mean price after Hamilton: $114.95
# p-value < 0.05, so difference significant
# Before publication, would need to adjust slightly for inflation


# Correlation ----
# Test to determine if 2 measures are related
# Positive correlation: Both increase together or decrease together
# Negative correlation: One increases while the other decreases
# Correlation does not imply causation
# Sometimes causal. Example: height causes weight 
# Usually, just related: TV crime viewing and mean-world syndrome


## FL City Crime ---- 
# Are violent and property crime in Florida correlated?
# Source: FBI Uniform Crime Report for 2019 offenses
# Excludes cities with zero violent crime to enable cor() test

# Import fl_city_crime.csv as crime
crime <- read_csv("data/fl_city_crime.csv")

# Scatterplot prop_crime and vio_crime
crime %>% 
  ggplot(aes(prop_crime, vio_crime)) +
  geom_point() +
  geom_smooth(method = "lm")

# Correlation test
# Format: cor(variable1, variable2)
cor(crime$vio_crime, crime$prop_crime)

# Interpret cor() results
# Number is between 0.0 (no relationship) and 1.0 (perfect correlation)
# This result is exceptional in measuring social relationships 
# Standard advanced by Jacob Cohen:
## 0.10 to 0.29 = small
## 0.30 to 0.49 = moderate
## 0.50 to 1.00 = large


## Trump Voters ----
# Some pundits associate Trump voters with high unemployment areas
# Let's test whether data supports that hypothesis for 2020 election
# Voting: MIT site plus numerous county elections sites
# Unemployment: Bureau of Labor Statistics 2020 county averages

# Import trump_jobless.csv as voters
voters <- read_csv("data/trump_jobless.csv")

# Perform cor() test on jobless_pct, trump_pct
cor(voters$jobless_pct, voters$trump_pct)

# Interpret results
# 0.40 = A moderate correlation
# The negative sign = negative correction
# Thus, as jobless_pct increases, trump_pct decreases
# Opposite of what the pundits said

# However, a correlation test alone is insufficient
# Need to evaluate a scatterplot

# Scatterplot
voters %>% 
  ggplot(aes(jobless_pct, trump_pct)) +
  geom_point() +
  geom_smooth(method = "lm")

# Zoom out scatterplot and evaluate results

# Re-run scatterplot without regression line
voters %>% 
  ggplot(aes(jobless_pct, trump_pct)) +
  geom_point()

# Evaluate results:
# Cor() affected by compressed data range  
# Lesson: Always perform a scatterplot with cor()

  
# Simple Regression ----
# Think of as a correlation test of causation
# Essential: A logical reason for presupposing a causation
# Otherwise, must use cor(), not simple linear regression
# Pro: Quantifies affect of variable1 on variable2
# Con: In real life, a single variable rarely has such effect
# Therefore, rarely used


## Income ----
# Test a presumed relationship between education and income
# Hypothesis: Increased education leads to higher income
# Opposite unlikely: Income does not cause education
# Thus, a reasonable assumption
# Source: Census ACS data for 2019 by county
# Calculated education as percentage with 4-year college degree or more
# Removed one county with 0 with 4-year degree or more

# Import education_income.csv as income
income <- read_csv("data/education_income.csv")

# Scatterplot relationship
income %>% 
  ggplot(aes(college_pct, median_hou_inc)) +
  geom_point() +
  geom_smooth(method = "lm")

# Calculate simple linear regression line
# Technically, build a linear model
# Cause variable (education) is the IV (independent variable)
# Result variable (income) is the DV (dependent variable)
# Format: lm(DV, ~ IV, data = dataset) (tilde ~ means "on")
inc_model <- lm(median_hou_inc ~ college_pct, data = income)

# Use summary() to show results in console
summary(inc_model)

# Interpret results
# Residuals: Difference between observed and predicted (ignore)
# Coefficients: 
## (Intercept) is where line intersections y-axis (income)
## college_pct is contribution education makes
## 1052 means 1 pct-point increase in college_pct results in $1,052 in income
# Pr(>|t|) = probability. Here, *** means p < 0.001
# Multiple R-squared between 0.0 (no cause) and 1.0 (total cause)
# 0.4446 means education can explain 44.5% of variance in income


## Evaluation ----
# Yes, college education leads to higher income
# However, so do other factors
# Multiple linear regression is better suited for causality


# Multiple Regression ----
# Multiple Linear Regression like simple, with more than 1 IV
# Also requires logical reason for each causal factors

# Pro: Identify incremental contribution of each factor
# ie, Mask-wearing reduced Covid-19 spread while controlling for risk factors
# Con: Co-linearity, or overlap between factors
# ie, Mask-wearing greater in areas more prone to social caution
# However, only sure way to parse factors is via an experiment

# Beware the kitchen sink fallacy
# More factors = larger model "fit," which can be misleading
# Best to limit to 2 to 5 IVs most likely to contribute


## FL City Crime ----
# Q. What social factors contribute the most to variance in crime?
# Crime source: FBI Uniform Crime Report 2019 data
# All other: Census ACS 2019 data
# Minimum population 5,000 to avoid law of small numbers

## Evaluate Data ----
# Import fl_cities.csv
cities <- read_csv("data/fl_cities.csv")

# Inspect variables
# DV (dependent variable) is crime_rate, per 1,000 people
# Potential IVs (independent, or causal, variables)
## median_age = median age 
## median_hou_inc = median household income
## poverty_pct = percent of individuals below the poverty line
## jobless_pct = average unemployment rate 
## college_pct = percent of those age 25+ with at least 4-year degree

# Evaluate variables:
# Are these valid causes?
# Do some of these overlap?
# What variables are missing?

## Corr. Matrix ----
# Requires data with only IVs and DV
# Select crime_rate through college_pct
cities_corr <- cities %>% 
  select(crime_rate:college_pct)

# Create correlation matrix
cities_matrix <- cor(cities_corr)

# View result 

# Interpret correlation matrix
# All 5 are at least moderate strength correlations, per Cohen
# Poverty_pct is the largest, and a strong correlation 
# Next, test for statistical significance when controlling for other factors


## Build Equation ----
# Format: lm(DV, ~IV1 + IV2 + IV3, data = dataset)
cities_model <- lm(crime_rate ~ median_age + median_hou_inc + poverty_pct + jobless_pct + college_pct, data = cities)

#View summary 
summary(cities_model)

## Read Results ----
# Residuals: observed vs. predicted (ignore)
# Intercept: Crime rate if no other variables (ignore)

# Estimates: Look first to see if statistically significant. Three are:
## Median_age, p<.05
## Median_hou_inc, p<.05
## Poverty_pct, p<.001
# Standard error: Multiply by 2 to plus/minus get 95% confidence interval
# t-value: Divide Estimate by Standard error 

# Multiple R-squared: Skip with multiple regression
# Adjusted R-squared: Model explains % of variance in crime rate
# Result, .297 = The model explains 30% of the variance
# So, 70% of variance is not accounted for in this model
# Still, 30% is fairly typical in social science

# How to read median_age estimate:
# -2.813e-01 = -0.2813
# As median age increases by 1 unit (year), crime rate decreases by .281 units
# Result is controlling for income, poverty, jobless rate, and education

# How to read poverty_pct estimate:
# 8.837e-01 = 0.8837
# As poverty increases by 1 unit (percent), crime rate increases by .88 units
# Result is controlling for age, income, jobless rate, and education

## Confidence ----
# Apply standard error to build a confidence interval
# Example: poverty_pct estimate is 0.8837
# Standard error is 0.2159
# 2x 0.2159 is 0.4318
# Lower bound: 0.8837 - 0.4318 = 0.4519
# Upper bound: 0.8837 + 0.4318 = 1.3155
# 95% confident true value between 0.4519 and 1.3155
# 95% confident that a 1 pct-point increase in poverty increases crime between 0.45 and 1.31 units
# Key: Range does not dip below zero

