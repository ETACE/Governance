# Title:    Replication file for Fig. 4 and 5
# Authors:  MG-PH-MN
# Version:  2020 December 14

library(tseries)
library(tidyverse)
library(ggtern)
library(viridis)
library(RColorBrewer)
library(reshape2)

rm(list = ls())

# import and clean data --------------------------------------------------------
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")
load("data_time_evolution.Rdata")

rm(i, p1, p2, par1, par2, parameter1, parameter2, pathdata, pathfigs, probe, runs, parameter_list)

data <- DATA
rm(DATA)

# preparation of parameters
runs <- max(data$r)
log <- FALSE

check_stationarity = FALSE
time <-c(2500, 10000, 50000)

par1 <- c("0.0","0.01","0.02","0.04","0.06","0.08",
          "0.1", "0.12","0.14","0.16","0.18",
          "0.2","0.22","0.24","0.26","0.28",
          "0.3","0.32","0.34","0.36","0.38",
          "0.4","0.42","0.44","0.46","0.48",
          "0.5","0.52","0.54","0.56","0.58",
          "0.6","0.62","0.64","0.66","0.68",
          "0.7","0.72","0.74","0.76","0.78",
          "0.8","0.82","0.84","0.86","0.88",
          "0.9","0.92","0.94","0.96","0.98","1.0")
par2 <- par1

data <- data %>%
  filter(t %in% time)

# splines for the productivity growth ------------------------------------------
variable <- "annual_growth_weighted_productivity"
log <- FALSE

TEMP_ALL <- list()
kk <- 0

# reorganize the data
for(t in time){
  
  kk <- kk+1
  TEMP <- c()
  DATA1 <- c()
  
  for(p1 in 1:length(par1))  {
    for(p2 in 1:length(par2))  {
      
      eval(parse(text=paste("temp = data[data$par1==par1[p1] & data$par2==par2[p2] & data$t==t,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      TEMP <- rbind(TEMP,
                    data.frame(x=as.numeric(par2[p2]),
                               y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                               z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                               val = mean(temp)))
      DATA1 = rbind(DATA1,
                    data.frame(r=1:runs,
                               x=as.numeric(par2[p2]),
                               y=as.numeric(par1[p1]),
                               val=temp))
    }
  }
  
  # change names (for the legend)
  TEMP_ALL[[kk]] <- TEMP %>%
    setNames(c("x", "y", "z", "Prod.Growth"))
  
}

# polish the dataframe
for (i in 1:length(time)){
  TEMP_ALL[[i]] <- TEMP_ALL[[i]] %>%
    mutate(time = paste0("t = ",time[i], sep = ""))
}

TEMP_ALL_SPLINE_PROD <- bind_rows(TEMP_ALL) %>%
  mutate(time = if_else(time == "t = 2500", "Young", time)) %>% 
  mutate(time = if_else(time == "t = 10000", "Mature", time)) %>% 
  mutate(time = if_else(time == "t = 50000", "Steady State", time)) %>% 
  mutate(time = factor(time, levels = c("Young", "Mature", "Steady State")))

# change names (for the legend) and filter the values of manager (x)
TEMP <- TEMP_ALL_SPLINE_PROD %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = if_else(x == "0.1", "Low", x)) %>% 
  mutate(x = if_else(x == "0.5", "Intermediate", x)) %>% 
  mutate(x = if_else(x == "0.9", "High", x)) %>%
  mutate(x = factor(x, levels = c("Low", "Intermediate", "High"))) %>% 
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  setNames(c("Manager.autonomy", "Ownership.LTI", "Ownership.STI", "Prod.Growth", "time"))

# plot it
ggplot(TEMP, aes(x=Ownership.LTI, y=Prod.Growth, group=Manager.autonomy, col=Manager.autonomy)) +
  facet_wrap(time~., nrow=1) +
  geom_smooth(method = "loess", se = F) +
  scale_color_manual(values = c("red", "blue", "black")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.background = element_rect(linetype="solid", colour ="black", size = .8)
  )

# splines for the herfindahl-hirschman index -----------------------------------
variable <- "av_herfindahl_index"
log <- FALSE

TEMP_ALL <- list()
kk <- 0

# reorganize the data
for(t in time){
  
  kk <- kk+1
  TEMP <- c()
  DATA1 <- c()
  
  for(p1 in 1:length(par1))  {
    for(p2 in 1:length(par2))  {
      
      eval(parse(text=paste("temp = data[data$par1==par1[p1] & data$par2==par2[p2] & data$t==t,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      TEMP <- rbind(TEMP,
                    data.frame(x=as.numeric(par2[p2]),
                               y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                               z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                               val = mean(temp)))
      DATA1 = rbind(DATA1,
                    data.frame(r=1:runs,
                               x=as.numeric(par2[p2]),
                               y=as.numeric(par1[p1]),
                               val=temp))
    }
  }
  
  # change names (for the legend)
  TEMP_ALL[[kk]] <- TEMP %>%
    setNames(c("x", "y", "z", "Prod.Growth"))
  
}

# polish the dataframe
for (i in 1:length(time)){
  TEMP_ALL[[i]] <- TEMP_ALL[[i]] %>%
    mutate(time = paste0("t = ",time[i], sep = ""))
}

TEMP_ALL_SPLINE_HHI <- bind_rows(TEMP_ALL) %>%
  mutate(time = if_else(time == "t = 2500", "Young", time)) %>% 
  mutate(time = if_else(time == "t = 10000", "Mature", time)) %>% 
  mutate(time = if_else(time == "t = 50000", "Steady State", time)) %>% 
  mutate(time = factor(time, levels = c("Young", "Mature", "Steady State")))

# change names (for the legend) and filter the values of manager (x)
TEMP <- TEMP_ALL_SPLINE_HHI %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = if_else(x == "0.1", "Low", x)) %>% 
  mutate(x = if_else(x == "0.5", "Intermediate", x)) %>% 
  mutate(x = if_else(x == "0.9", "High", x)) %>%
  mutate(x = factor(x, levels = c("Low", "Intermediate", "High"))) %>% 
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  setNames(c("Manager.autonomy", "Ownership.LTI", "Ownership.STI", "HHI", "time"))

# plot it
ggplot(TEMP, aes(x=Ownership.LTI, y=HHI, group=Manager.autonomy, col=Manager.autonomy)) +
  facet_wrap(time~., nrow = 1) +
  geom_smooth(method = "loess", se = F) +
  scale_color_manual(values = c("red", "blue", "black")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.background = element_rect(linetype="solid", colour ="black", size = .8)
  )
