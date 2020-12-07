
# -------------------------------------------------------------------------
# Script purpose: Persistence analysis
# Corresponding figure: Figure 3
# Date created: 01/08/2019
# Date last modified: 22/09/2020
# Author: Naomi Muggleton
# -------------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggsci)
library(colortools)
library(scales)
library(ggalluvial)

# Load data ---------------------------------------------------------------

GDWPROD2 <- ""
td_uid <- ""

odbc <- dbConnect(odbc(),"") 

year <- dbGetQuery(odbc, "")
qurt <- dbGetQuery(odbc, "")

## Store as data.table for speed
setDT(year)
setDT(qurt)

# Remember to disconnect from the Aster connection.  
dbDisconnect(odbc)

rm(odbc, GDWPROD2, td_uid)

# Figure a ----------------------------------------------------------------

## Clean data
setnames(year, "PARTY_ID", "party_id")

year[, party_id := factor(party_id)]

year$group <- character()
year[prop_spend == 0]$group <- "0%"
year[prop_spend > 0 & prop_spend < .05]$group <- "<5%"
year[prop_spend >= .05 & prop_spend < .1]$group <- "5-10%"
year[prop_spend >= .1]$group <- "10%+"

year[,
     group := factor(group,
                     levels = rev(c("0%", "<1%",
                                    "1-2%", "<5%",
                                    "5-10%", "10%+")))]

year.subset <- year[rep_dt %in% c(2012, 2015, 2018)]
year.subset$year <- numeric()

year.subset[rep_dt == 2012]$year <- 1
year.subset[rep_dt == 2015]$year <- 2
year.subset[rep_dt == 2018]$year <- 3

year.subset$group <- droplevels(year.subset$group)

## Calculate no. individuals that equal 100%
l <- length(unique(year$party_id))

## Make plot
a <- ggplot(year.subset, 
            aes(x = year, stratum = group, alluvium = party_id, fill = group)) +
  theme_bw() +
  geom_bar(aes(x = year, y = length(party_id) / length(year.subset$party_id), 
               fill = group), 
           stat = "identity", position = "stack", width = (1 / 3)) +
  scale_fill_manual(values = hue_pal()(4)) +
  geom_flow(data = year.subset[rep_dt != 2018], aes.flow = "backward") +
  geom_flow(data = year.subset[rep_dt != 2012], aes.flow = "forward") +
  coord_cartesian(expand = F, ylim = c(0, l)) +
  scale_x_continuous(name = NULL, breaks = 1:3,
                     labels = c("2012", "2015", "2018")) +
  scale_y_continuous(name = "Fraction of customers",
                     labels = function(x) paste0(round(x / l * 100, 1), "%"),
                     breaks = quantile(0:l, probs = c(0, .25, .5, .75, 1)),
                     limits = c(0, l)) +
  labs(tag = "a") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        text = element_text(size = 7),
        panel.grid = element_blank(),
        plot.tag = element_text(size = 8, face = "bold"),
        legend.key.height = unit(2.5, "mm"),
        legend.key.width = unit(2.5, "mm"),
        legend.spacing.x = unit(.75, "mm"),
        legend.margin = margin(0, 0, 0, 0, "lines"))


# Figure b ----------------------------------------------------------------

setnames(qurt, "PARTY_ID", "party_id")

qurt[, party_id := factor(party_id)]
qurt[, rep_dt := dmy(rep_dt)]
qurt[, quarter := floor_date(rep_dt, unit = "quarter")]

q <- qurt[, .(gambling = sum(gambling), spend = sum(spend),
              prop_spend = sum(gambling) / sum(spend)),
          by = c("party_id", "quarter")]

keep <- q[prop_spend >= .1 & year(quarter) == 2015 & month(quarter) == 4]$party_id

q <- q[party_id %in% keep]

q$group <- character()
q[prop_spend == 0]$group <- "0%"
q[prop_spend > 0 & prop_spend < .05]$group <- "<5%"
q[prop_spend >= .05 & prop_spend < .1]$group <- "5-10%"
q[prop_spend >= .1]$group <- "10%+"

q[, 
  group := factor(group, 
                  levels = rev(c("0%", "<5%", "5-10%", "10%+")))]

x <- sort(unique(q$quarter))
label <- rep(paste0(1:4), 7)
x1 <- sort(unique(floor_date(q$quarter, unit = "years")))
label1 <- year(x1)


q[, year := year(quarter)]
q[, Q := quarter(quarter)]
q[, label := ifelse(Q == 1, paste(Q, year, sep = "\n"), paste0(Q, "\n"))]

quart <- function(x) {
  
  year <- year(x)
  quarter <- quarter(x)
  lab <- ifelse(quarter == 1, 
                paste(quarter, year, sep = "\n"),
                paste0(quarter, "\n"))
  print(lab)
}

labs <- unique(q[, label, by = quarter])
setorder(labs, quarter)

b <- ggplot(q, aes(x = quarter,
                   y = length(party_id), fill = group)) +
  geom_bar(aes(x = quarter, y = length(party_id), fill = group), 
           stat = "identity", position = "fill", width = 60) +
  scale_x_date(name = NULL, breaks = "3 months", labels = quart) +
  theme_bw() +
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  scale_y_continuous(name = "Fraction of Q2 2015 10%+ gamblers",
                     labels = percent, expand = expand_scale(0)) +
  coord_cartesian(ylim = c(0, 1), expand = 0) + 
  labs(tag = "b") +
  scale_fill_manual(values = hue_pal()(4)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 7),
        text = element_text(size = 7),
        panel.grid = element_blank(),
        plot.tag = element_text(size = 8, face = "bold"),
        legend.key.height = unit(2.5, "mm"),
        legend.key.width = unit(2.5, "mm"),
        legend.spacing.x = unit(.75, "mm"),
        legend.margin = margin(0, 0, 0, 0, "lines"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

plot <- ggarrange(a, b, nrow = 2, align = "hv")

ggsave(plot, filename = "2-data_analysis/output/figure_3.pdf", 
       unit = "cm", width = 8.9)
