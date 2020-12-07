
# -------------------------------------------------------------------------
# Script purpose: Customer gambling percentile rank vs. 30 outcomes
# Corresponding figure: Generate output for Figure 1
# Date created: 09/07/2019
# Date last modified: 30/09/2020
# Author: Naomi Muggleton
# -------------------------------------------------------------------------

rm(list = ls())

library(odbc)
library(DBI)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(ggpubr)
library(DescTools)

## Set working directory: change as appropriate
wd <- ""
setwd(wd)
rm(wd)

# Load data ---------------------------------------------------------------

GDWPROD2 <- ""  # Redacted (commercially sensitive)
td_uid <- ""  # Redacted (commercially sensitive)

odbc <- dbConnect("")  # Redacted (commercially sensitive)

# Outcomes (unit of analysis: customer x month). Carbon copy of GDW table.
# Extracted via GDW.

## Create a virtual data frame from an existing table on GDW
vars <- dbGetQuery(odbc, "")  # Redacted (commercially sensitive)

## Store as data.table for speed
setDT(vars)

# Outcomes (unit of analysis: customer x month). Carbon copy of GDW table
# bsci_lab_anl.gambling_long_term_outcomes. Extracted via GDW.

## Create a virtual data frame from an existing table on GDW
lt <- dbGetQuery(odbc, "")  # Redacted (commercially sensitive)

## Store as data.table for speed
setDT(lt)

# Remember to disconnect from the GDW connection.
dbDisconnect(odbc)

rm(odbc, GDWPROD2, td_uid)

# Clean data --------------------------------------------------------------

# vars
## Step 1: Restructure data
vars[, party_id := factor(party_id)]

## Step 2: Get rid of any transactions from end of 2017 / start of 2019 
## that might've carried through
vars <- vars[year(rep_period) == 2018]

## Step 3: Get rid of non-gamblers, so that we're left with people who 
## gambled at least once in 2018
vars[, gambled_ever := max(gambling_dummy), by = party_id]
vars <- vars[gambled_ever == 1]

## Step 4: Drop columns that we won't need for Figure
drop <- c("monthly_spend", "N_transactions", "transactions_spend",
          "gambling_dummy", "gambled_ever")

vars[, (drop) := NULL]

rm(drop)

## Step 5: Winsorize non-binary variables
cols <- c(
  "mortgage_spend", "insurance_spend", "ISA", "savings", "fast_food_spend", 
  "bar_spend", "off_licence_spend", "tobacco_spend", "prescription_spend", 
  "self_care_spend", "fitness_spend", "hobbies_spend", 
  "social_activities_spend", "education_spend", "travel_spend"
)

vars[,
     (cols) := lapply(.SD, function(x) Winsorize(
       x, minval = NULL, maxval = NULL, probs = c(0, .975), na.rm = T)
     ),
     .SDcols = cols]

rm(cols)

## Step 6: Set key for faster merge
setkey(vars, party_id, rep_period)

# lt
## Calculate rank
lt <- lt[prop_gambled > 0]
lt[, rank := ntile(prop_gambled, 100)]
lt[, rank := rank / 100]  # Standardise from 0-1
lt[, "prop_gambled" := NULL]  # Drop columns

jsa <- lt[jsa_2013 == 0]  # Restrict plots to those employed in 2013
dis <- lt[disability_2013 == 0]  # Restrict plot to able-bodied in 2013

jsa <- jsa[, c("party_id", "rank", "jsa_2014_2019")]
dis <- dis[, c("party_id", "rank", "disability_2014_2019")]

rm(lt)

# Create full dataset
## Step 1: Merge data
data <- vars

rm(vars)

## Step 2: Predicting future outcomes
### We want to use last month's gambling to predict this month's outcomes.
### To do this we will lag proportion gambled so that we're not looking at
### concurrent gambling.

#### Check columns are in order
setorder(data, party_id, rep_period)

#### Now use lag to shift scores by party ID
data[,
     rank := shift(rank_prop, type = "lag", n = 1),
     by = party_id]

#### Drop January dates, for which rank = NA (thanks to lag)
data <- data[complete.cases(rank)]

# Plot parameters ---------------------------------------------------------

# Set custom hex colour
col_1 <- "#4DBBD5FF"
col_2 <- "#FC4E07"
col_3 <- "#E7B800"

# Globally set theme to classic
theme_set(theme_bw())

# Margin to keep plots uniform
margin <- theme(
  axis.title.x = element_blank(),
  axis.text.x = element_text(
    margin = unit(c(.2, 0, 0, 0), "cm"), colour = "white"
  ),
  axis.text.y = element_text(margin = unit(c(0, .2, 0, 0), "cm")),
  panel.grid = element_blank(),
  axis.ticks.length = unit(-.1, "cm"),
  plot.margin = margin(.2, .3, 0, -.25, "cm"),
  text = element_text(size = 7),
  legend.position = "none",
  plot.title = element_text(
    size = 7,margin = unit(c(.1, 0, -.6, 0), "cm")
  )
)

# Get rid of annoying white space
expand <- expansion(0)

# Convert decimals to percentages, where appropeiate
percent <- percent_format(accuracy = 1)

# Convert double to pounds, where appropriate
pounds <- dollar_format(
  big.mark = ",", 
  largest_with_cents = 9.99, 
  prefix = "£"
)

# Range xlim is always 0-1
xlim <- c(0, 1)

# Produce smooth curve through data points
smooth <- geom_smooth(
  data = data,
  method = "gam",
  formula = y ~ s(x, bs = "cs"),
  fill = col_1, 
  col = col_1,
  alpha = .5,
  na.rm = T,
  fullrange = T,
  size = .1
)

# Produce a point per rank bin
points <- stat_summary(
  fun = mean, 
  geom = "point",
  na.rm = T
)

# If rank == 0 (i.e., didn't gamble that month), col = red
one_point <- stat_summary(
  data = data[rank == 0],
  fun = mean,
  geom = "point",
  na.rm = T,
  col = col_2
)

# Format x-axis
scalex <- scale_x_continuous(
  name = "Rank of % gambled",
  breaks = seq(0, 1, .25),
  labels = c("0%", "25%", "50%", "75%", "100%"),
  expand = expand
)

# Panel A -----------------------------------------------------------------

p1a <- ggplot(data, aes(x = rank, y = unplanned_od)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .08, .02),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .08),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Uses unplanned\n overdraft")

p1b <- ggplot(data, aes(x = rank, y = misses_card_payment)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .1, .025),
    labels = percent_format(accuracy = .1)
  ) +
  coord_cartesian(
    ylim = c(0, .1),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Misses credit card\n payment")

p1c <- ggplot(data, aes(x = rank, y = payday_loan)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .05, .01),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .05),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Takes a payday loan\n")

p1d <- ggplot(data, aes(x = rank, y = misses_loan_payment)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .08, .02),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .08),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Misses loan payment\n")

p1e <- ggplot(data, aes(x = rank, y = missed_mortgage_payment)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .12, .04),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .12),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Missed mortgage\n payment")

# Panel B -----------------------------------------------------------------

p2a <- ggplot(data, aes(x = rank, y = credit_card)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 1, .25),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, 1),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Credit card\n")

p2b <- ggplot(data, aes(x = rank, y = loan)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 1, .25),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, 1),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Loan\n")

p2c <- ggplot(data, aes(x = rank, y = mortgage)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 1, .25),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, 1),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Mortgage\n")

p2d <- ggplot(data, aes(x = rank, y = utilisation)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .6, .2),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .6),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Credit card utilisation\n")

p2e <- ggplot(data, aes(x = rank, y = debt_recovery)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .1, .025),
    labels = percent_format(accuracy = .1)
  ) +
  coord_cartesian(
    ylim = c(0, .1),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Debt recovery\n")

# Panel C -----------------------------------------------------------------

p3a <- ggplot(data, aes(x = rank, y = insurance_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 180, 30),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 180),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Insurance\n")

p3b <- ggplot(data, aes(x = rank, y = mortgage_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 600, 200),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 600),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Mortgage\n")

p3c <- ggplot(data, aes(x = rank, y = savings)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 8000, 2000),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 8000),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Savings\n")

p3d <- ggplot(data, aes(x = rank, y = ISA)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 1260, 420),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 1260),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Tax-preferred savings\n")

p3e <- ggplot(data, aes(x = rank, y = pension)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .12, .04),
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .12),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Pension\n")

# Panel D -----------------------------------------------------------------

p4a <- ggplot(data, aes(x = rank, y = fast_food_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 40, 10),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 40),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Fast food\n")

p4b <- ggplot(data, aes(x = rank, y = gaming_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .4, .1),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, .4),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Gaming\n")

p4c <- ggplot(data, aes(x = rank, y = bar_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 30, 10),
    labels = pounds
  ) +
  coord_cartesian(ylim = c(0, 30),
                  xlim = xlim) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Bars\n")

p4d <- ggplot(data, aes(x = rank, y = tobacco_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, .8, .2),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, .8),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Tobacco\n")

p4e <- ggplot(data, aes(x = rank, y = off_licence_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 5, 1),
    labels = pounds
  ) +
  coord_cartesian(ylim = c(0, 5),
                  xlim = xlim) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Off licence\n")

# Panel E -----------------------------------------------------------------

p5a <- ggplot(data, aes(x = rank, y = prescription_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 3, .5),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 3),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Prescriptions\n")

p5b <- ggplot(data, aes(x = rank, y = self_care_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 25, 5),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 25),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Self-care\n")

p5c <- ggplot(data, aes(x = rank, y = fitness_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 12, 4),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 12),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Fitness\n")

p5d <- ggplot(data, aes(x = rank, y = nighttime_spend)) +
  points + 
  one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 2.5, .5)
  ) +
  coord_cartesian(
    ylim = c(0, 2.5),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Nights awake\n")

p5e <- ggplot(dis, aes(x = rank, y = disability_2014_2019)) +
  points +
  geom_smooth(
    data = dis,
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    fill = col_3, 
    col = col_3,
    alpha = .5,
    na.rm = T,
    fullrange = T,
    size = .1
  ) +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .08),
    xlim = xlim
  ) +
  labs(title = " Disability payments\n")

# Panel F -----------------------------------------------------------------

## Adapt x-axis for bottom row of plot
margin <- theme(
  axis.title.x = element_blank(),
  axis.text.y = element_text(margin = unit(c(0, .2, 0, 0), "cm")),
  axis.text.x = element_text(margin = unit(c(.2, 0, 0, 0), "cm")),
  panel.grid = element_blank(),
  axis.ticks.length = unit(-.1, "cm"),
  plot.margin = margin(.2, .3, 0, -.25, "cm"),
  text = element_text(size = 7),
  legend.position = "none",
  plot.title = element_text(size = 7, margin = unit(
    c(.1, 0, -.6, 0), "cm"))
)

p6a <- ggplot(data, aes(x = rank, y = hobbies_spend)) +
  points + one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 100, 25),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 100),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Hobbies\n")

p6b <- ggplot(data, aes(x = rank, y = social_activities_spend)) +
  points + one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 45, 15),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 45),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Social activities\n")

p6c <- ggplot(data, aes(x = rank, y = education_spend)) +
  points + one_point +
  smooth +
  scalex +
  labs(x = "Percentile rank") +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 12, 4),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 12),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Education\n")

p6d <- ggplot(data, aes(x = rank, y = travel_spend)) +
  points + one_point +
  smooth +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    breaks = seq(0, 180, 60),
    labels = pounds
  ) +
  coord_cartesian(
    ylim = c(0, 180),
    xlim = xlim
  ) +
  scale_color_manual(values = c(col_2, "black")) +
  labs(title = " Travel\n")

p6e <- ggplot(jsa, aes(x = rank, y = jsa_2014_2019)) +
  points +
  geom_smooth(
    data = jsa,
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    fill = col_3, 
    col = col_3,
    alpha = .5,
    na.rm = T,
    fullrange = T,
    size = .1
  ) +
  scalex +
  margin +
  scale_y_continuous(
    expand = expand,
    name = NULL,
    labels = percent
  ) +
  coord_cartesian(
    ylim = c(0, .07),
    xlim = xlim
  ) +
  labs(title = " Unemployment\n")

# Labels ------------------------------------------------------------------

a <- ggplot() + 
  ggtitle("Financial\ndistress") + 
  theme_void() + 
  theme(
    plot.title = element_text(
      size = 7, 
      angle = 90, 
      hjust = .5, 
      vjust = 0, 
      margin = unit(c(2, 2, 2, 0), unit = "cm")
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

b <- ggplot() + 
  ggtitle("Financial\nengagement") + 
  theme_void() + 
  theme(
    plot.title = element_text(
      size = 7, 
      angle = 90, 
      hjust = .5, 
      vjust = 0,
      margin = unit(c(2, 2, 2, 0), unit = "cm")
      ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

c <- ggplot() + 
  ggtitle("Financial\nplanning") + 
  theme_void() + 
  theme(
    plot.title = element_text(
      size = 7, 
      angle = 90, 
      hjust = .5, 
      vjust = 0,
      margin = unit(c(2, 2, 2, 0), unit = "cm")
      ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

d <- ggplot() + 
  ggtitle("Lifestyle\n") + 
  theme_void() + 
  theme(
    plot.title = element_text(
      size = 7, 
      angle = 90, 
      hjust = .5, 
      vjust = 0,
      margin = unit(c(2, 2, 2, 0), unit = "cm")
      ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

e <- ggplot() + 
  ggtitle("Health and\nwellbeing") + 
  theme_void() + 
  theme(
    plot.title = element_text(
      size = 7, 
      angle = 90, 
      hjust = .5, 
      vjust = 0,
      margin = unit(c(2, 2, 2, 0), unit = "cm")
      ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

f <- ggplot() + 
  ggtitle("Leisure and\ninterests") + 
  theme_void() + 
  theme(plot.title = element_text(
    size = 7, 
    angle = 90, 
    hjust = .5, 
    vjust = 0,
    margin = unit(c(2, 2, 2, 0), unit = "cm")
    ),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

# Arrange -----------------------------------------------------------------

figure_1 <- ggarrange(
  a, p1a, p1b, p1c, p1d, p1e, b, p2a, p2b, p2c, p2d, p2e, c, p3a, p3b, p3c, 
  p3d, p3e, d, p4a, p4b, p4c, p4d, p4e, e, p5a, p5b, p5c, p5d, p5e, f, p6a,
  p6b, p6c, p6d, p6e, nrow = 6, ncol = 6, align = "v", 
  widths = c(1, rep(5, 5)))

ggsave(figure_1, file = "2-data_analysis/output/figure_1.pdf", 
       height = 18.3, width = 18.3, unit = "cm")
