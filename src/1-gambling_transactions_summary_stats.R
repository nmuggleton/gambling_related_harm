
# -------------------------------------------------------------------------
# Script purpose: Gambling transactions summary statistics
# Corresponding table: Generate output for Table 1
# Date created: 17/06/2019
# Date last modified: 27/08/2019
# Author: Naomi Muggleton
# -------------------------------------------------------------------------

rm(list = ls())

## Set working directory: uncomment and change as appropriate
wd <- ""
setwd(wd)
rm(wd)

library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)

# Load data ---------------------------------------------------------------

# Outcomes (unit of analysis: customer x month). Carbon copy of GDW table.
# Extracted using FastExport.
data <- fread("data/gambling_variables.txt", na.string = "?")

# Clean data --------------------------------------------------------------

# Gambling variables
## Step 1: Restructure data
data[, party_id := as.factor(party_id)]
data[, rep_period := dmy(rep_period)]

## Step 2: Drop coluns that we won't need for table 1
data <- data[, 
             c("party_id", "rep_period", "N_transactions",
               "transactions_spend", "gambling_dummy", "prop_gambled")]

## Step 3: Get rid of any transactions from end of 2017 / start of 2019 
## that might've carried through
data <- data[year(rep_period) == 2018]

# Panel A: All Individuals ------------------------------------------------

# Here we produce summary statistics for each customer's annual gambling
# behaviour. Sample = our 100k random sample.

## Step 1: As well as summary stats across all customers, we want to look 
## at summary stats among the subset of customers who gambled in our 
## observation period. To do this, we create columns that exclude 
## non-gamblers by (a) duplicating our columns, then (b) coding zeroes to
## NAs.

### Columns we want to duplicate
vars <- c("N_transactions", "transactions_spend", "prop_gambled")

### Add suffix _cond (i.e., conditional on gambling) to generate names for
### new columns 
newvars <- paste0(vars, "_cond")

### Duplicate columns
data[, (newvars) := lapply(.SD, function(x) x), .SDcols = vars]

### If customer didn't gamble this month, then NA, else keep value
data[, 
     (newvars) := lapply(.SD, function(x) 
       ifelse(gambling_dummy == 0, NA, x)), 
     .SDcols = newvars]

### Now we'll calculate:
### * Total number of gambling transactions in 2018 (N_transactions)
### * Total spend on gambling in 2018 (transactions_spend)
### * Whether a customer gambled in 2018 (gambling_dummy)
### * Typical proportion of monthly spend attributed to gambling 
###   (prop_gambled)
data.agg <- data[, 
             .(N_transactions = sum(N_transactions),
               transactions_spend = sum(transactions_spend),
               gambling_dummy = max(gambling_dummy),
               prop_gambled = mean(prop_gambled)),
             by = party_id]

### Duplicate columns
data.agg[, (newvars) := lapply(.SD, function(x) x), .SDcols = vars]

### If customer didn't gamble this month, then NA, else keep value
data.agg[, 
     (newvars) := lapply(.SD, function(x) 
       ifelse(gambling_dummy == 0, NA, x)), 
     .SDcols = newvars]

## Step 2: Melt table to produce summary statistics per variable.
### Check that all to-be melted columns are defined as numeric
nums <- names(data.agg)[-1]
data.agg[, (nums) := lapply(.SD, as.numeric), .SDcols = nums]

### We no longer need the dummy column
data.agg[, gambling_dummy := NULL]

### Melt variables, identifiers = party ID and reporting month
data.melt <- melt(data.agg, id.vars = "party_id")

## Step 3: Generate output
panel_a <- data.melt[,
                     .(mean = mean(value, na.rm = T),
                       sdev = sd(value, na.rm = T),
                       p25 = quantile(value, probs = .25, na.rm = T),
                       p50 = quantile(value, probs = .50, na.rm = T),
                       p75 = quantile(value, probs = .75, na.rm = T),
                       p90 = quantile(value, probs = .95, na.rm = T),
                       p99 = quantile(value, probs = .99, na.rm = T)
                     ),
                     by = variable]

# Proportion of people who gambled in 2018
panel_a_gambling_0_1 <- as.numeric(table(data.agg$N_transactions == 0) / 
                                     nrow(data.agg))[1]

# Sample size
panel_a_N <- length(unique(data$party_id))

# Panel B: All Individuals x Months ---------------------------------------

# Here we produce summary statistics for each customer's monthly gambling
# behaviour. Sample = our 100k random sample.

## Step 1: As well as summary stats across all customers, we want to look 
## at summary stats among the subset of customers who gambled in our 
## observation period. To do this, we create columns that exclude 
## non-gamblers by (a) duplicating our columns, then (b) coding zeroes to
## NAs.

### First, we'll drop newvars and recreate them after aggregating data 
### by customer
data[, (newvars) := NULL]

### Duplicate columns
data[, (newvars) := lapply(.SD, function(x) x), .SDcols = vars]

### If customer didn't gamble this month, then NA, else keep value
data[, 
     (newvars) := lapply(.SD, function(x) 
  ifelse(gambling_dummy == 0, NA, x)), .SDcols = newvars]

## Step 2: Melt table to produce summary statistics per variable.
### Check that all to-be melted columns are defined as numeric
nums <- names(data)[-(1:2)]
data[, (nums) := lapply(.SD, as.numeric), .SDcols = nums]

### We no longer need the dummy column
data[, gambling_dummy := NULL]

### Melt variables, identifiers = party ID and reporting month
data.melt <- melt(data, id.vars = c("party_id", "rep_period"))

## Step 3: Generate output
panel_b <- data.melt[,
                     .(mean = mean(value, na.rm = T),
                       sdev = sd(value, na.rm = T),
                       p25 = quantile(value, probs = .25, na.rm = T),
                       p50 = quantile(value, probs = .50, na.rm = T),
                       p75 = quantile(value, probs = .75, na.rm = T),
                       p90 = quantile(value, probs = .95, na.rm = T),
                       p99 = quantile(value, probs = .99, na.rm = T)),
                     by = variable]


# Proportion of people who gambled
panel_b_gambling_0_1 <- as.numeric(table(data$N_transactions == 0) / 
                                     nrow(data))[1]

# Sample size
panel_b_N <- nrow(unique(data[, c("party_id", "rep_period")]))

# Output ------------------------------------------------------------------

# Panel A
kable(panel_a, digits = 2)
round(panel_a_gambling_0_1, digits = 2)
print(panel_a_N)

# Panel B
kable(panel_b, digits = 2)
round(panel_b_gambling_0_1, digits = 2)
print(panel_b_N)
