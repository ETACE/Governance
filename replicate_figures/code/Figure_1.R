# Title:    Replication file for Fig. 1
# Authors:  MG-PH-MN
# Version:  2020 December 14

library(tseries)
library(tidyverse)
library(ggtern)
library(viridis)
library(RColorBrewer)
library(reshape2)

rm(list = ls())

# import data and stationarity test --------------------------------------------
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")

data_hhi <- read_csv("batch_mean_herfindahl_index.csv") %>%
  setNames(c("time","HHI")) %>%
  filter(time > 20000 & time <= 50000)

adf.test(data_hhi$HHI, alternative = "stationary")

data_hhi <- read_csv("batch_mean_herfindahl_index.csv") %>%
  setNames(c("time","HHI"))

data_nfirms <- read_csv("batch_mean_num_active_firms.csv") %>%
  setNames(c("time","N_firms"))

data_both <- bind_cols(data_hhi, data_nfirms[,2])

# time series ------------------------------------------------------------------
ggplot(data_both, aes(x=time)) +
  geom_line(aes(y=HHI, colour = "HHI"), size = 1.2) +
  geom_line(aes(y=N_firms/50, colour = "Num. Firms"), size = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Num. Firms")) +
  scale_colour_manual(values = c("blue", "black")) +
  labs(colour = "Variable") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(linetype="solid", colour ="black", size = .8),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13),
        plot.title = element_text(face="bold", size=15),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        strip.text = element_text(face="bold", size=13),
        legend.position = "bottom")
