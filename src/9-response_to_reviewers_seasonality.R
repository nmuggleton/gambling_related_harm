
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
library(stargazer)
library(miceadds)

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

## Store as data.table for speed
setDT(vars)
setDT(dems)

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
i <- c("Intecept", "% gambled" , "Age", "Gender", "Turnover")


# Models ------------------------------------------------------------------

paste <- paste("unplanned_od", "~ prop_gambled + age + gender + turnover")

as.formula(paste)


cluster <- function(x) {
  
  string <- paste(x, "~ prop_gambled + age + gender + turnover")
  
  formula <- as.formula(string)
  
  model <- lm.cluster(data = data,
                      formula = formula,
                      cluster = "rep_period")
  
  coefs <- data.table(dv = x,
                      iv = names(coef(model)),
                      coef = round(coef(model), 2),
                      lwr = round(confint(model)[, 1], 2),
                      upr = round(confint(model)[, 2], 2),
                      pval = summary(model)[, 4]
                      )
  
  coefs[, pval := ifelse(pval < .001, "p<.001", paste0("p=", round(pval, 3)))]
  
  nums <- c("coef", "lwr", "upr")
  
  coefs[, (nums) := lapply(
    .SD, function(i) {
      gsub("0$", "", i)
      }
    ),
    .SDcols = nums]
  
  print(coefs)

}

cluster("unplanned_od")
cluster("misses_card_payment")
cluster("payday_loan")
cluster("misses_loan_payment")
cluster("missed_mortgage_payment")
cluster("credit_card")
cluster("loan")
cluster("mortgage")
cluster("utilisation")
cluster("debt_recovery")
cluster("insurance_spend")
cluster("mortgage_spend")
cluster("savings")
cluster("ISA")
cluster("pension")
cluster("fast_food_spend")
cluster("gaming_spend")
cluster("bar_spend")
cluster("tobacco_spend")
cluster("off_licence_spend")
cluster("self_care_spend")
cluster("fitness_spend")
cluster("nighttime_spend")
cluster("hobbies_spend")
cluster("social_activities_spend")
cluster("education_spend")
cluster("travel_spend")




