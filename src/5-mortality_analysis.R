
# -------------------------------------------------------------------------
# Script purpose: Mortality analysis
# Corresponding figure: Figure 2
# Date created: 01/08/2019
# Date last modified: 21/09/2020
# Author: Naomi Muggleton
# -------------------------------------------------------------------------

rm(list = ls())

## Set working directory: change as appropriate
wd <- ""
setwd(wd)

library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(survival)
library(ggthemes)
library(ggfortify)
library(ggsci)
library(colortools)
library(survminer)

theme_set(theme_bw())

# Load data ---------------------------------------------------------------

GDWPROD2 <- ""
td_uid <- ""

odbc <- dbConnect(odbc(),"") 

data <- dbGetQuery(odbc, "")

## Store as data.table for speed
setDT(data)

# Remember to disconnect from the Aster connection.  
dbDisconnect(odbc)

rm(odbc, GDWPROD2, td_uid)

# Clean data --------------------------------------------------------------

names(data) <- str_to_lower(names(data))

setnames(data, "time_o", "time")

data <- data[gender != "NA"]

factors <- c("party_id", "gender", "status")

dates <- c("death_dt", "dob", "last_obs")

numbers <- c("gambling_prop", "time")

data[, (factors) := lapply(.SD, factor), .SDcols = factors]
data[, (dates) := lapply(.SD, dmy), .SDcols = dates]
data[, (numbers) := lapply(.SD, as.numeric), .SDcols = numbers]

rm(factors, dates, numbers)

data[, age := dmy("01/01/2014") - dob]

data[complete.cases(death_dt), last_obs := death_dt - day(death_dt) + 1]

data <- data[year(last_obs) > 2013]

data <- data[time <= 5.410]

# Set up plots ------------------------------------------------------------

colours <- hue_pal()(4)

# Colour palettes for men
men <- sequential(colours[1],
                  what = "saturation", 
                  percentage = (100 / 3), 
                  plot = F,
                  v = .75)

# Colour palettes for women
women <- sequential(colours[2], 
                    what = "saturation", 
                    percentage = (100 / 3), 
                    plot = F, 
                    v = .75)

labels <- c("0%", "10%", "20%", "30%") # Break points
xlim <- c(0, 5) # Specify limits
scalex <- scale_x_continuous(name = "Time (years)", 
                             breaks = seq(0, 5, 1),
                             expand = expansion(0))

yname <- "Survival probability" # Axis name
expand <- expansion(0) # Remove padding (personal preference)
theme <- theme(legend.position = "top", # Put legend at the top
               legend.spacing.x = unit(2, "mm"),
               axis.text.y = element_text(margin = unit(c(0, .2, 0, 0), "cm"),
                                          size = 7),
               axis.text.x = element_text(margin = unit(c(.2, 0, 0, 0), "cm"),
                                          size = 7),
               axis.title.y = element_text(margin = unit(c(0, 0, 0, .2), "cm"),
                                           size = 7),
               axis.title.x = element_text(margin = unit(c(0, 0, .2, 0), "cm"),
                                           size = 7),
               panel.grid = element_blank(),
               axis.ticks.length = unit(-.1, "cm"),
               plot.margin = margin(0, .1, 0, 0, "cm"),
               plot.title = element_text(size = 7),
               legend.text = element_text(size = 7),
               legend.title = element_text(size = 7),
               legend.key.height = unit(2.5, "mm"))

# Survival analysis -------------------------------------------------------

data$cens <- ifelse(is.na(data$death_dt), 0, 1)

sobj <- Surv(data$time, data$cens)

cxmod <- coxph(sobj ~ gambling_prop + age + gender, data = data)

ages <- quantile(data$age, probs = c(.25, .5, .75), na.rm = T)

newdat <- expand.grid(
  gambling_prop = seq(0, .3, .1),
  age = ages,
  gender = levels(data$gender)
)

surv <- seq(.99, .01, by = -.01)

### Label our rows for easier cross-referencing
rownames(newdat) <- letters[1:nrow(newdat)]

## Step 4: Create time-to-event curves
### This will allow us to make plots that capture time-to-event.
cxsf <- survfit(cxmod, data = data, newdata = newdat)

### Nice summary of a survival curve. Compared to the summary() function, 
### surv_summary() creates a data frame containing a nice summary from 
### survfit results.
surv_cxmod0 <- surv_summary(cxsf)

## Step 5: Bind model outputs to input
### Tag together
surv_cxmod <- cbind(surv_cxmod0,
                    newdat[as.character(surv_cxmod0$strata), ])

### Convert to data.table for speed
setDT(surv_cxmod)

### Don't treat as.numeric
surv_cxmod[, gambling_prop := factor(gambling_prop)]

### Create example people (i.e., these are NOT a real customers) to 
### illustrate model
yng <- surv_cxmod[surv_cxmod$age == ages[1]]
med <- surv_cxmod[surv_cxmod$age == ages[2]]
old <- surv_cxmod[surv_cxmod$age == ages[3]]

### Example for manuscript

ex <- med[gender == "F" & time == 5]

(1 - ex$surv) * 1000
(1 - ex$upper) * 1000
(1 - ex$lower) * 1000

odds <- (1 - ex$surv) * 1000
odds[4] / odds[1]

# Plots -------------------------------------------------------------------

## Male
### Younger men
a <- ggplot(yng[gender == "M"],
            aes(x = time, y = surv, col = gambling_prop, 
                fill = gambling_prop)) +
  geom_step() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values = men, labels = labels) +
  scale_fill_manual(values = men, labels = labels) +
  scalex +
  scale_y_continuous(name = yname, 
                     labels = percent_format(accuracy = .01), 
                     expand = expand) +
  coord_cartesian(x = c(0, 4.8), y = c(.9975, 1)) +
  theme +
  labs(title = "32-year-old",
       fill ="Men",
       colour = "Men")

### Middle-aged men
b <- ggplot(med[gender == "M"],
            aes(x = time, 
                y = surv, 
                col = gambling_prop),
            fill = gambling_prop) +
  geom_line() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values = men,
                      labels = labels) +
  scale_fill_manual(values = men,
                    labels = labels) +
  scalex +
  scale_y_continuous(name = yname, labels = percent_format(accuracy = .01), 
                     expand = expand) +
  coord_cartesian(x = c(0, 4.8), y = c(.99, 1)) +
  theme +
  labs(title = "44-year-old")

### Older men
c <- ggplot(old[gender == "M"],
            aes(x = time, 
                y = surv, 
                col = gambling_prop),
            fill = gambling_prop) +
  geom_line() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values = men,
                      labels = labels) +
  scale_fill_manual(values = men,
                    labels = labels) +
  scalex +
  scale_y_continuous(name = yname, labels = percent_format(accuracy = .01), 
                     expand = expand) +
  coord_cartesian(x = c(0, 4.8), y = c(.965, 1)) +
  theme +
  labs(title = "57-year-old")

## Women
### Younger women
d <- ggplot(yng[gender == "F"],
            aes(x = time, 
                y = surv,
                col = gambling_prop),
            fill = gambling_prop) +
  geom_line() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values =  women,
                      labels = labels) +
  scale_fill_manual(values =  women,
                    labels = labels) +
  scalex +
  scale_y_continuous(name = yname, labels = percent_format(accuracy = .01), 
                     expand = expand) +
  coord_cartesian(x = c(0, 4.8), y = c(.998, 1)) +
  theme +
  labs(title = "32-year-old",
       fill ="Women",
       colour = "Women")

### Middle-aged women
e <- ggplot(med[gender == "F"],
            aes(x = time, 
                y = surv, 
                col = gambling_prop),
            fill = gambling_prop) +
  geom_line() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values =  women,
                      labels = labels) +
  scale_fill_manual(values =  women,
                    labels = labels) +
  scalex +
  scale_y_continuous(name = yname, labels = percent_format(accuracy = .01), 
                     expand = expand) +
  coord_cartesian(x = c(0, 4.91), y = c(.993, 1)) +
  theme +
  labs(title = "44-year-old")

### Older women
f <- ggplot(old[gender == "F"],
            aes(x = time, y = surv, 
                col = gambling_prop),
            fill = gambling_prop) +
  geom_line() +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = gambling_prop), 
              alpha = .5, col = NA) +
  scale_colour_manual(values =  women, labels = labels) +
  scale_fill_manual(values =  women, labels = labels) +
  scalex +
  scale_y_continuous(name = yname, labels = percent_format(accuracy = .01),
                     expand = expand) +
  coord_cartesian(x = c(0, 4.8), y = c(.974, 1)) +
  theme +
  labs(title = "57-year-old")

mn <-  ggarrange(a, b, c, 
                 nrow = 1, ncol = 3, common.legend = T, legend = "top", align = "hv")
wom <- ggarrange(d, e, f,
                 nrow = 1, ncol = 3, common.legend = T, legend = "top", align = "hv")

ggarrange(mn, wom, nrow = 2, ncol = 1, align = "hv") %>% 
  ggsave(filename = "2-data_analysis/output/figure_2.pdf",
         width = 12.0, height = 8, units = "cm")
