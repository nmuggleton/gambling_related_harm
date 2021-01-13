
# -------------------------------------------------------------------------
# Script purpose: Customer outcome variable summary statistics
# Corresponding table: Supplementary Table 1
# Date created: 09/07/2019
# Date last modified: 10/11/2019
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

GDWPROD2 <- ""  # Redacted (commercially sensitive)
td_uid <- ""    # Redacted (commercially sensitive)

odbc <- dbConnect("")  # Redacted (commercially sensitive)

vars <- dbGetQuery(odbc, "")  # Redacted (commercially sensitive)
dems <- dbGetQuery(odbc, "")  # Redacted (commercially sensitive)
lt <- dbGetQuery(odbc, "")  # Redacted (commercially sensitive)

## Store as data.table for speed
setDT(vars)  # Variables (1 row per customer, per month)
setDT(dems)  # Demographic data (1 row per customer)
setDT(lt)  # Long-term outcomes (1 row per customer)

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

## Step 3: Get rid of non-gamblers
vars <- vars[gambling_dummy == 1]

## Step 4: Drop columns that we won't need for Figure
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

## Bin income in £10,000s
data$turnover <- data$turnover / 10000

#lt
lt <- lt[prop_gambled > 0]
jsa <- lt[jsa_2013 == 0]  # Restrict plots to those employed in 2013
dis <- lt[disability_2013 == 0]  # Restrict plot to able-bodied in 2013

rm(lt)

jsa[, c("jsa_2013", "disability_2013", "disability_2014_2019") := NULL]
dis[, c("jsa_2013", "jsa_2014_2019", "disability_2013") := NULL]

jsa[, age := 2013 - yob]
dis[, age := 2013 - yob]

jsa[, "yob" := NULL]
dis[, "yob" := NULL]


# Panel A -----------------------------------------------------------------

cols <- names(data)[-c(1, 2, 7)]

panela <- data[, 
               (cols = lapply(.SD, sum, na.rm = T)), 
               by = party_id, 
               .SDcols = cols]

a <- panela[,
            .(cols,
              mean = lapply(.SD, mean),
              sd   = lapply(.SD, sd),
              p25  = lapply(.SD, quantile, probs = .25),
              p50  = lapply(.SD, quantile, probs = .50),
              p75  = lapply(.SD, quantile, probs = .75),
              p90  = lapply(.SD, quantile, probs = .90),
              p99  = lapply(.SD, quantile, probs = .99)),
            .SDcols = cols]

a$cols <- factor(a$cols, 
                 c("arrears_pca", "arrears_card", "payday_loan", 
                   "arrears_loan", "arrears_mortgage", "credit_card", 
                   "loan", "mortgage", "pension", "debt_recovery",
                   "insurance_spend", "mortgage_spend", "savings", "isa",
                   "utilisation", "fast_food_spend", "gaming_spend",
                   "bar_spend", "tobacco_spend", "alcohol_spend",
                   "self_care_spend", "prescriptions", "fitness_spend",
                   "nighttime_spend", "hobbies_spend", 
                   "social_activities_spend", "education_spend", 
                   "travel_spend"))

setorder(a, cols)

# Panel B -----------------------------------------------------------------

panelb <- data[, cols, with = F]

b <- panelb[,
            .(cols,
              mean = lapply(.SD, mean, na.rm = T),
              sd   = lapply(.SD, sd, na.rm = T),
              p25  = lapply(.SD, quantile, probs = .25, na.rm = T),
              p50  = lapply(.SD, quantile, probs = .50, na.rm = T),
              p75  = lapply(.SD, quantile, probs = .75, na.rm = T),
              p90  = lapply(.SD, quantile, probs = .90, na.rm = T),
              p99  = lapply(.SD, quantile, probs = .99, na.rm = T)),
            .SDcols = cols]


b$cols <- factor(b$cols, 
                 c("arrears_pca", "arrears_card", "payday_loan", 
                   "arrears_loan", "arrears_mortgage", "credit_card", 
                   "loan", "mortgage", "pension", "debt_recovery",
                   "insurance_spend", "mortgage_spend", "savings", "isa",
                   "utilisation", "fast_food_spend", "gaming_spend",
                   "bar_spend", "tobacco_spend", "alcohol_spend",
                   "self_care_spend", "prescriptions", "fitness_spend",
                   "nighttime_spend", "hobbies_spend", 
                   "social_activities_spend", "education_spend", 
                   "travel_spend"))

setorder(b, cols)

xtable(a)

nrow(panela)
nrow(panelb)


# Panel A tweaks ----------------------------------------------------------

## Some panels should be averages not sums

cols <- names(data)[-c(1, 2, 7)]

panela <- data[, 
               (cols = lapply(.SD, mean, na.rm = T)), 
               by = party_id, 
               .SDcols = cols]

a <- panela[,
            .(cols,
              mean = lapply(.SD, mean, na.rm = T),
              sd   = lapply(.SD, sd, na.rm = T),
              p25  = lapply(.SD, quantile, probs = .25, na.rm = T),
              p50  = lapply(.SD, quantile, probs = .50, na.rm = T),
              p75  = lapply(.SD, quantile, probs = .75, na.rm = T),
              p90  = lapply(.SD, quantile, probs = .90, na.rm = T),
              p99  = lapply(.SD, quantile, probs = .99, na.rm = T)),
            .SDcols = cols]

a$cols <- factor(a$cols, 
                 c("arrears_pca", "arrears_card", "payday_loan", 
                   "arrears_loan", "arrears_mortgage", "credit_card", 
                   "loan", "mortgage", "pension", "debt_recovery",
                   "insurance_spend", "mortgage_spend", "savings", "isa",
                   "utilisation", "fast_food_spend", "gaming_spend",
                   "bar_spend", "tobacco_spend", "alcohol_spend",
                   "self_care_spend", "prescriptions", "fitness_spend",
                   "nighttime_spend", "hobbies_spend", 
                   "social_activities_spend", "education_spend", 
                   "travel_spend"))

setorder(a, cols)

xtable(a)
