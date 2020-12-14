# Title:    Replication file for Fig. 6
# Authors:  MG-PH-MN
# Version:  2020 December 14


rm(list = ls())

# this code takes data
library(tidyverse)
library(ggtern)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(arrangements)
library(latex2exp)

rm(list = ls())

# import and clean data --------------------------------------------------------
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")
load("marginal_effects.Rdata")

# clean
df_full <- DATA %>%
  filter(t %% 500 == 0) %>%
  filter(t > 2000 & t <= 6000) %>%
  mutate(t = as.factor(t)) %>%
  mutate(m = paste("M = ", m, sep = "")) %>%
  mutate(l = paste("LTI = ", l, sep = "")) %>%
  select(t, m, l, gap, exp_jump, mvi, buyback, real_investment, rd_intensity) %>%
  setNames(c("time", "M", "LTI", "gap", "exp_jump", "mvi", "buyback", "investment", "rd_intensity")) %>%
  mutate(buyback = as.numeric(buyback)) %>%
  mutate(investment = as.numeric(investment)) %>%
  mutate(investment_share = as.numeric(investment/(buyback+investment))) %>%
  mutate(rd_intensity = as.numeric(rd_intensity)) %>%
  mutate(probability_innovate = 0.6*(1 - exp(-10*(rd_intensity)^0.6))) %>%
  mutate(zeta = 1/gap) %>%
  filter(exp_jump < 0.002) %>%
  na.omit()

# organize data for the bar plot
rank <- 5
df_full_bar <- df_full %>%
  select(time, M, LTI, gap, exp_jump, investment_share) %>%
  arrange(time, M, LTI, -gap) %>%
  group_by(time, M, LTI) %>%
  mutate(ranking = order(gap, decreasing = T)) %>%
  filter(ranking <= rank) %>%
  mutate(exp_jump_diff = (exp_jump[ranking == 1] - mean(exp_jump[ranking != 1]))) %>%
  mutate(investment_share_mean = (investment_share[ranking == 1] - mean(investment_share[ranking != 1]))) %>%
  ungroup() %>%
  arrange(time, M, LTI, ranking) %>%
  select(time, M, LTI, ranking, gap, exp_jump, exp_jump_diff, investment_share_mean) %>% 
  filter(ranking == 1) %>% 
  mutate(LTI = if_else(LTI == "LTI = 0.1", "Short-term dominated", "Long-term dominated")) %>% 
  mutate(LTI = factor(LTI, levels = c("Short-term dominated", "Long-term dominated")))

# barplot ----------------------------------------------------------------------
options(scipen=10000)
ggplot(df_full_bar, aes(x=time, y=exp_jump_diff, group = time, col = time, fill = time)) +
  facet_wrap(LTI~., scales = "free")  +
  geom_bar(stat = "identity") +
  scale_color_viridis(discrete = T, option = "D") +
  scale_fill_viridis(discrete = T, option = "D") +
  xlab("Time") +
  ylab("Difference in expected produtivity growth") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 15, face = "bold"),
        legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="black", size = .8),
        legend.text = element_text(size = 14)
  )
