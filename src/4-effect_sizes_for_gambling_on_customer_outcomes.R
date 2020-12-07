
# -------------------------------------------------------------------------
# Script purpose: Effect sizes for gambling on customer outcomes
# Corresponding table: Table 2
# Date created: 02/10/2019
# Date last modified: 20/09/2020
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

data$turnover <- data$turnover / 10000

#lt
lt <- lt[prop_gambled > 0]
jsa <- lt[jsa_2013 == 0]  # Restrict plots to those employed in 2013
dis <- lt[disability_2013 == 0]  # Restrict plot to able-bodied in 2013

rm(lt)

jsa <- jsa[, c("jsa_2013", "disability_2013", "disability_2014_2019") := NULL]
dis <- dis[, c("jsa_2013", "jsa_2014_2019", "disability_2013") := NULL]

jsa[, age := scale(2013 - yob)]
dis[, age := scale(2013 - yob)]

jsa$gender_nm <- numeric()
dis$gender_nm <- numeric()

jsa[gender == "Female"]$gender_nm <- -.5
jsa[gender == "Male"]$gender_nm <- .5

dis[gender == "Female"]$gender_nm <- -.5
dis[gender == "Male"]$gender_nm <- .5

# Standardise variables ---------------------------------------------------

data[, age := scale(age)]
data[, turnover := scale(turnover)]

data[, gender := as.numeric(gender) - 1.5]

# Models ------------------------------------------------------------------

list <- names(data)[7:34]

l <- length(list)

models <- tibble(dv = character(length = l),
                 it = numeric(length = l),
                 co = numeric(length = l),
                 lw = numeric(length = l),
                 up = numeric(length = l),
                 cl = numeric(length = l),
                 cu = numeric(length = l))

for (i in 1:l) {
  
  model <- lm(paste(list[i], "~ prop_gambled + age + gender + turnover"), 
              data = data)
  
  models$dv[i] <- list[i]
  models$it[i] <- model$coefficients[1]
  models$co[i] <- model$coefficients[2] / 10
  models$lw[i] <- confint(model)[2, 1] / 10
  models$up[i] <- confint(model)[2, 2] / 10
  models$cl[i] <- confint(model)[1, 1]
  models$cu[i] <- confint(model)[1, 2]
  
  cat(paste0("\n", round(i / l * 100), "% complete"))
  
}

setDT(models)
setcolorder(models, c("dv", "it", "cl", "cu", "co", "lw", "up"))
models[, ef := co / it * 100]

cols <- names(models)[-1]

models[, (cols) := lapply(.SD, function(x) ifelse(abs(x) > 1, {round(x, 2)}, {signif(x, 2)})), .SDcols = cols]

m1 <- lm(dis$disability_2014_2019 ~ prop_gambled + gender_nm + age, data = dis)
m2 <- lm(jsa$jsa_2014_2019 ~ prop_gambled + gender_nm + age, data = jsa)

m1$coefficients[1]
confint(m1)[1, 1]
confint(m1)[1, 2]
m1$coefficients[2] / 10
confint(m1)[2, 1] / 10
confint(m1)[2, 2] / 10
(m1$coefficients[2] / 10) / m1$coefficients[1] * 100

m2$coefficients[1]
confint(m2)[1, 1]
confint(m2)[1, 2]
m2$coefficients[2] / 10
confint(m2)[2, 1] / 10
confint(m2)[2, 2] / 10
(m2$coefficients[2] / 10) / m2$coefficients[1] * 100


