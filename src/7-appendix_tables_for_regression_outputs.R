
# -------------------------------------------------------------------------
# Script purpose: Appendix tables for regression outputs
# Corresponding tables: Tables A4 - A11
# Date created: 23/09/2019
# Date last modified: 22/09/2020
# Author: Naomi Muggleton
# -------------------------------------------------------------------------

rm(list = ls())

library(odbc)
library(DBI)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)

## Set working directory: change as appropriate
wd <- ""
setwd(wd)
rm(wd)

# Load data ---------------------------------------------------------------

GDWPROD2 <- ""
td_uid <- ""

odbc <- dbConnect(odbc(),"") 

vars <- dbGetQuery(odbc, "")
dems <- dbGetQuery(odbc, "")
lt <- dbGetQuery(odbc, "")

## Store as data.table for speed
setDT(vars)
setDT(dems)
setDT(lt)

# Remember to disconnect from the Aster connection.  
dbDisconnect(odbc)

rm(odbc, GDWPROD2, td_uid)

# Clean data --------------------------------------------------------------

# vars
## Step 1: Restructure data
vars[, party_id := factor(party_id)]

## Step 2: Get rid of any transactions from end of 2017 / start of 2019 
## that might've carried through
vars <- vars[year(rep_period) == 2018]

## Step 3: Drop columns that we won't need for Figure
drop <- c("monthly_spend", "N_transactions", "transactions_spend",
          "gambling_dummy")

vars[, (drop) := NULL]

rm(drop)

## Step 4: Set key for faster merge
setkey(vars, party_id, rep_period)

# dems
## Step 1: Restructure data
dems[, party_id := factor(party_id)]
dems[, gender := factor(gender)]

## Step 2: Rename column
setnames(dems, "credit_turnover", "turnover")

## Step 3: Drop columns that we won't need for analysis
drop <- c("marital_status")

dems[, (drop) := NULL]

rm(drop)

## Step 4: Set key for faster merge
setkey(dems, party_id)

# Create full dataset
## Step 1: Merge datasets
data <- dems[vars]

## Step 2: Reorder columns
setcolorder(data, c("party_id", "rep_period", "prop_gambled"))

## Step 3: Lag proportion gambled by one month
### Order the columns first
setorder(data, party_id, rep_period)

### Lag variable by customer
data[, 
     prop_gambled := shift(prop_gambled, type = "lag", n = 1),
     by = party_id]

## Step 4: Clean up
rm(vars, dems)

data$turnover <- data$turnover / 10000

#lt
jsa <- lt[jsa_2013 == 0]  # Restrict plots to those employed in 2013
dis <- lt[disability_2013 == 0]  # Restrict plot to able-bodied in 2013

rm(lt)

jsa <- jsa[, c("jsa_2013", "disability_2013", "disability_2014_2019") := NULL]
dis <- dis[, c("jsa_2013", "jsa_2014_2019", "disability_2013") := NULL]

jsa[, age := 2013 - yob]
dis[, age := 2013 - yob]

jsa[, "yob" := NULL]
dis[, "yob" := NULL]

i <- c("Intecept", "% gambled" , "Age", "Gender", "Turnover")

# Table A4 ----------------------------------------------------------------

m1 <- lm(unplanned_od ~ prop_gambled + age + gender + turnover, data = data)
m2 <- lm(misses_card_payment ~ prop_gambled + age + gender + turnover, 
         data = data)
m3 <- lm(payday_loan ~ prop_gambled + age + gender + turnover, data = data)
m4 <- lm(misses_loan_payment ~ prop_gambled + age + gender + turnover, 
         data = data)
m5 <- lm(missed_mortgage_payment ~ prop_gambled + age + gender + turnover, 
         data = data)

stargazer(m1, m2, m3, m4, m5, 
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Uses unplanned overdraft", 
                             "Misses credit card payment",
                             "Takes a payday loan",
                             "Misses loan payment",
                             "Missed mortgage payment"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a4.tex")

rm(m1, m2, m3, m4, m5)

# Table A5 ----------------------------------------------------------------

m1 <- lm(credit_card ~ prop_gambled + age + gender + turnover, data = data)
m2 <- lm(loan ~ prop_gambled + age + gender + turnover, data = data)
m3 <- lm(mortgage ~ prop_gambled + age + gender + turnover, data = data)
m4 <- lm(utilisation ~ prop_gambled + age + gender + turnover, data = data)
m5 <- lm(debt_recovery ~ prop_gambled + age + gender + turnover, 
         data = data)

stargazer(m1, m2, m3, m4, m5, 
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Credit card", "Loan", "Mortgage", 
                             "Utilisation", "Debt recovery"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a5.tex")

rm(m1, m2, m3, m4, m5)

# Table A6 ----------------------------------------------------------------

m1 <- lm(insurance_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m2 <- lm(mortgage_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m3 <- lm(savings ~ prop_gambled + age + gender + turnover, data = data)
m4 <- lm(ISA ~ prop_gambled + age + gender + turnover, data = data)
m5 <- lm(pension ~ prop_gambled + age + gender + turnover, data = data)

stargazer(m1, m2, m3, m4, m5, 
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Insurance", "Mortgage", "Savings", "ISA", 
                             "Pension"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a6.tex")

rm(m1, m2, m3, m4, m5)

# Table A7 ----------------------------------------------------------------

m1 <- lm(fast_food_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m2 <- lm(gaming_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m3 <- lm(bar_spend ~ prop_gambled + age + gender + turnover, data = data)
m4 <- lm(tobacco_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m5 <- lm(off_licence_spend ~ prop_gambled + age + gender + turnover, 
         data = data)

stargazer(m1, m2, m3, m4, m5, 
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Fast food", "Gaming", "Bars", "Tobacco", 
                             "Off licences"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a7.tex")

rm(m1, m2, m3, m4, m5)

# Table A8 ----------------------------------------------------------------

m1 <- lm(prescription_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m2 <- lm(self_care_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m3 <- lm(fitness_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m4 <- lm(nighttime_spend ~ prop_gambled + age + gender + turnover, 
         data = data)

stargazer(m1, m2, m3, m4,
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Prescriptions", "Self-care", "Fitness",
                             "Nights awake"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a8.tex")

rm(m1, m2, m3, m4)

# Table A9 ----------------------------------------------------------------

m1 <- lm(hobbies_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m2 <- lm(social_activities_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m3 <- lm(education_spend ~ prop_gambled + age + gender + turnover, 
         data = data)
m4 <- lm(travel_spend ~ prop_gambled + age + gender + turnover, 
         data = data)

stargazer(m1, m2, m3, m4,
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Hobbies", "Social activities", "Education",
                             "Travel"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a9.tex")

rm(m1, m2, m3, m4)

# Table A10 ---------------------------------------------------------------

m1 <- lm(disability_2014_2019 ~ prop_gambled + age + gender, data = dis)
m2 <- lm(jsa_2014_2019 ~ prop_gambled + age + gender, data = jsa)

stargazer(m1, m2,
          digits = 10,
          intercept.top = T,
          intercept.bottom = F,
          digit.separator = " ",
          omit.stat = c("adj.rsq", "f", "ser"),
          ci = T,
          p.auto = T,
          report = "vcsp",
          covariate.labels = i,
          model.numbers = T,
          dep.var.labels = c("Disability payments", "Unemployment"),
          dep.var.caption = "Outcome at $t=1$",
          out = "tables/regression_a10.tex")
