# Title:    Replication file for Fig. 7 to 10
# Authors:  MG-PH-MN
# Version:  2020 December 18

library(tidyverse)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(latex2exp)

rm(list = ls())

# import and clean data --------------------------------------------------------
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")
load("data_sim_experiment.Rdata")

rm(i, p0, p1, p2, par0, par1, par2, parameter0, parameter1, parameter2, pathdata, 
   num_procs, runs, parameter_list, getdata, retrieve_data)

data <- DATA
rm(DATA)

# preparation of parameters
runs <- max(data$r)
log <- FALSE

check_stationarity <- TRUE
time <-c(2500, 5000, 10000, 50000)

par0 <- c("0.001", "0.01", "0.1")
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
par2 <- c("0.1", "0.5", "0.9")

data <- data %>%
  filter(t %in% time)

# clean data
data_low <- data %>%
  filter(par0 == "0.001")
data_medium <- data %>%
  filter(par0 == "0.01")
data_high <- data %>%
  filter(par0 == "0.1")

# reorganize productivity data -------------------------------------------------
variable <- "annual_growth_weighted_productivity"

# reorganize data
for (i in 1:length(time)){
  
  TEMP_LOW <- c()
  DATA_LOW <- c()
  TEMP_MEDIUM <- c()
  DATA_MEDIUM <- c()
  TEMP_HIGH <- c()
  DATA_HIGH <- c()
  
  count <- 0
  for(p1 in 1:length(par1))  {
    for(p2 in 1:length(par2))  {
      count <- count + 1
      
      # low
      eval(parse(text=paste("temp = data_low[data_low$par1==par1[p1] & data_low$par2==par2[p2] & data_low$t==time[i] ,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      if (time[i] %in% c(2500,5000) & count == 14){
        TEMP_LOW <- rbind(TEMP_LOW,
                          data.frame(x=as.numeric(par2[p2]),
                                     y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                     z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                     val = mean(temp)))
        DATA_LOW = rbind(DATA_LOW,
                         data.frame(r=1:runs,
                                    x=as.numeric(par2[p2]),
                                    y=as.numeric(par1[p1]),
                                    # val=c(temp,0)))
                                    val=temp))
      } else {
        TEMP_LOW <- rbind(TEMP_LOW,
                          data.frame(x=as.numeric(par2[p2]),
                                     y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                     z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                     val = mean(temp)))
        DATA_LOW = rbind(DATA_LOW,
                         data.frame(r=1:runs,
                                    x=as.numeric(par2[p2]),
                                    y=as.numeric(par1[p1]),
                                    val=temp))
      }
      
      # medium
      eval(parse(text=paste("temp = data_medium[data_medium$par1==par1[p1] & data_medium$par2==par2[p2] & data_medium$t==time[i] ,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      TEMP_MEDIUM <- rbind(TEMP_MEDIUM,
                           data.frame(x=as.numeric(par2[p2]),
                                      y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                      z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                      val = mean(temp)))
      DATA_MEDIUM = rbind(DATA_MEDIUM,
                          data.frame(r=1:runs,
                                     x=as.numeric(par2[p2]),
                                     y=as.numeric(par1[p1]),
                                     val=temp))
      # high
      eval(parse(text=paste("temp = data_high[data_high$par1==par1[p1] & data_high$par2==par2[p2] & data_high$t==time[i] ,]$",
                            variable,
                            sep="")))
      TEMP_HIGH <- rbind(TEMP_HIGH,
                         data.frame(x=as.numeric(par2[p2]),
                                    y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                    z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                    val = mean(temp)))
      DATA_HIGH = rbind(DATA_HIGH,
                        data.frame(r=1:runs,
                                   x=as.numeric(par2[p2]),
                                   y=as.numeric(par1[p1]),
                                   val=temp))

    }
  }
  
  if (i == 1){
    EARLY <- list()
    EARLY[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    EARLY[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    EARLY[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    EARLY[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    EARLY[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    EARLY[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 2){
    MED_1 <- list()
    MED_1[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    MED_1[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    MED_1[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_1[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_1[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    MED_1[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 3){
    MED_2 <- list()
    MED_2[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    MED_2[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    MED_2[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_2[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_2[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    MED_2[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 4){
    LATE <- list()
    LATE[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    LATE[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    LATE[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    LATE[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    LATE[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    LATE[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  }
}

# merge EARLY and LATE
TEMP_LOW <- bind_rows(EARLY[[1]], MED_1[[1]], MED_2[[1]], LATE[[1]])
DATA_LOW <- bind_rows(EARLY[[2]], MED_1[[2]], MED_2[[2]], LATE[[2]])
TEMP_MEDIUM <- bind_rows(EARLY[[3]], MED_1[[3]], MED_2[[3]], LATE[[3]])
DATA_MEDIUM <- bind_rows(EARLY[[4]], MED_1[[4]], MED_2[[4]], LATE[[4]])
TEMP_HIGH <- bind_rows(EARLY[[5]], MED_1[[5]], MED_2[[5]], LATE[[5]])
DATA_HIGH <- bind_rows(EARLY[[6]], MED_1[[6]], MED_2[[6]], LATE[[6]])

# polish and merge data
TEMP_LOW_TRI <- TEMP_LOW %>%
  setNames(c("x", "y", "z", "Prod.Growth", "time")) %>%
  mutate(gamma = "0.001") %>%
  select(time, everything())

TEMP_LOW <- TEMP_LOW %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "Prod.Growth")) %>%
  mutate(gamma = "0.001")

TEMP_MEDIUM_TRI <- TEMP_MEDIUM %>%
  setNames(c("x", "y", "z", "Prod.Growth", "time")) %>%
  mutate(gamma = "0.01") %>%
  select(time, everything())

TEMP_MEDIUM <- TEMP_MEDIUM %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "Prod.Growth")) %>%
  mutate(gamma = "0.01")

TEMP_HIGH_TRI <- TEMP_HIGH %>%
  setNames(c("x", "y", "z", "Prod.Growth", "time")) %>%
  mutate(gamma = "0.1") %>%
  select(time, everything())

TEMP_HIGH <- TEMP_HIGH %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "Prod.Growth")) %>%
  mutate(gamma = "0.1")

TEMP_ALL_TRI <- merge(TEMP_LOW_TRI, TEMP_MEDIUM_TRI, all = T) %>%
  merge(., TEMP_HIGH_TRI, all=T) %>%
  melt(id = c("time", "x", "y", "z", "gamma"))

TEMP_ALL <- merge(TEMP_LOW, TEMP_MEDIUM, all = T) %>%
  merge(., TEMP_HIGH, all=T) %>%
  melt(id = c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "gamma")) %>%
  filter(time %in% c(5000, 50000))

# keep only equilibrium 
TEMP_ALL_EQ <- TEMP_ALL %>% 
  filter(time %in% c(50000)) %>%
  mutate(Manager.autonomy = as.character(Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.1", "Low", Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.5", "Intermediate", Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.9", "High", Manager.autonomy)) %>%
  mutate(Manager.autonomy = factor(Manager.autonomy, levels = c("Low", "Intermediate", "High"))) %>% 
  mutate(gamma = if_else(gamma == "0.001", "Low bonus", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.01", "Baseline", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.1", "High bonus", gamma)) %>% 
  mutate(gamma = factor(gamma, levels = c("Low bonus", "Baseline", "High bonus")))

# decide whether to remove corner
TEMP_ALL_EQ2 <- TEMP_ALL_EQ %>%
  filter(Ownership.LTI > 0.02)

# splines for productivity growth ---------------------------------------------- 
ggplot(TEMP_ALL_EQ2, aes(x=Ownership.LTI, y=value, group=Manager.autonomy, col=Manager.autonomy)) +
  facet_wrap(.~gamma, nrow = 1) +
  geom_smooth(method = "loess", se = F) +
  scale_color_manual(values = c("red", "blue", "black")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.background = element_rect(linetype="solid", colour ="black", size = .8)
  )
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig7.pdf")

# keep only variables for figure later on
THE_PROD <- TEMP_ALL_TRI
rm(list=setdiff(ls(), "THE_PROD"))

# import and clean data --------------------------------------------------------
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")
load("data_sim_experiment.Rdata")

rm(i, p0, p1, p2, par0, par1, par2, parameter0, parameter1, parameter2, pathdata, 
   num_procs, runs, parameter_list, getdata, retrieve_data)

data <- DATA
rm(DATA)

# preparation of parameters
runs <- max(data$r)
log <- FALSE

check_stationarity <- TRUE
time <-c(2500, 5000, 10000, 50000)

par0 <- c("0.001", "0.01", "0.1")
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
par2 <- c("0.1", "0.5", "0.9")

data <- data %>%
  filter(t %in% time)

# clean data
data_low <- data %>%
  filter(par0 == "0.001")
data_medium <- data %>%
  filter(par0 == "0.01")
data_high <- data %>%
  filter(par0 == "0.1")

# reorganize herfindahl data ---------------------------------------------------
variable <- "av_herfindahl_index"

# reorganize data
for (i in 1:length(time)){
  
  TEMP_LOW <- c()
  DATA_LOW <- c()
  TEMP_MEDIUM <- c()
  DATA_MEDIUM <- c()
  TEMP_HIGH <- c()
  DATA_HIGH <- c()
  
  count <- 0
  for(p1 in 1:length(par1))  {
    for(p2 in 1:length(par2))  {
      count <- count + 1
      
      # low
      eval(parse(text=paste("temp = data_low[data_low$par1==par1[p1] & data_low$par2==par2[p2] & data_low$t==time[i] ,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      if (time[i] %in% c(2500,5000) & count == 14){
        TEMP_LOW <- rbind(TEMP_LOW,
                          data.frame(x=as.numeric(par2[p2]),
                                     y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                     z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                     val = mean(temp)))
        DATA_LOW = rbind(DATA_LOW,
                         data.frame(r=1:runs,
                                    x=as.numeric(par2[p2]),
                                    y=as.numeric(par1[p1]),
                                    # val=c(temp,0)))
                                    val=temp))
      } else {
        TEMP_LOW <- rbind(TEMP_LOW,
                          data.frame(x=as.numeric(par2[p2]),
                                     y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                     z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                     val = mean(temp)))
        DATA_LOW = rbind(DATA_LOW,
                         data.frame(r=1:runs,
                                    x=as.numeric(par2[p2]),
                                    y=as.numeric(par1[p1]),
                                    val=temp))
      }
      
      # medium
      eval(parse(text=paste("temp = data_medium[data_medium$par1==par1[p1] & data_medium$par2==par2[p2] & data_medium$t==time[i] ,]$",
                            variable,
                            sep="")))
      
      if(log){
        temp <- log(temp)
      }
      
      TEMP_MEDIUM <- rbind(TEMP_MEDIUM,
                           data.frame(x=as.numeric(par2[p2]),
                                      y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                      z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                      val = mean(temp)))
      DATA_MEDIUM = rbind(DATA_MEDIUM,
                          data.frame(r=1:runs,
                                     x=as.numeric(par2[p2]),
                                     y=as.numeric(par1[p1]),
                                     val=temp))
      # high
      eval(parse(text=paste("temp = data_high[data_high$par1==par1[p1] & data_high$par2==par2[p2] & data_high$t==time[i] ,]$",
                            variable,
                            sep="")))
      TEMP_HIGH <- rbind(TEMP_HIGH,
                         data.frame(x=as.numeric(par2[p2]),
                                    y=(1-as.numeric(par2[p2]))*as.numeric(par1[p1]),
                                    z = (1-as.numeric(par2[p2]))*(1-as.numeric(par1[p1])),
                                    val = mean(temp)))
      DATA_HIGH = rbind(DATA_HIGH,
                        data.frame(r=1:runs,
                                   x=as.numeric(par2[p2]),
                                   y=as.numeric(par1[p1]),
                                   val=temp))
      
    }
  }
  
  if (i == 1){
    EARLY <- list()
    EARLY[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    EARLY[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    EARLY[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    EARLY[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    EARLY[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    EARLY[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 2){
    MED_1 <- list()
    MED_1[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    MED_1[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    MED_1[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_1[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_1[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    MED_1[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 3){
    MED_2 <- list()
    MED_2[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    MED_2[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    MED_2[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_2[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    MED_2[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    MED_2[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  } else if (i == 4){
    LATE <- list()
    LATE[[1]] <- TEMP_LOW %>%
      mutate(time = rep(time[i]))
    LATE[[2]] <- DATA_LOW %>%
      mutate(time = rep(time[i]))
    LATE[[3]] <- TEMP_MEDIUM %>%
      mutate(time = rep(time[i]))
    LATE[[4]] <- DATA_MEDIUM %>%
      mutate(time = rep(time[i]))
    LATE[[5]] <- TEMP_HIGH %>%
      mutate(time = rep(time[i]))
    LATE[[6]] <- DATA_HIGH %>%
      mutate(time = rep(time[i]))
  }
}

# merge EARLY and LATE
TEMP_LOW <- bind_rows(EARLY[[1]], MED_1[[1]], MED_2[[1]], LATE[[1]])
DATA_LOW <- bind_rows(EARLY[[2]], MED_1[[2]], MED_2[[2]], LATE[[2]])
TEMP_MEDIUM <- bind_rows(EARLY[[3]], MED_1[[3]], MED_2[[3]], LATE[[3]])
DATA_MEDIUM <- bind_rows(EARLY[[4]], MED_1[[4]], MED_2[[4]], LATE[[4]])
TEMP_HIGH <- bind_rows(EARLY[[5]], MED_1[[5]], MED_2[[5]], LATE[[5]])
DATA_HIGH <- bind_rows(EARLY[[6]], MED_1[[6]], MED_2[[6]], LATE[[6]])

# polish and merge data
TEMP_LOW_TRI <- TEMP_LOW %>%
  setNames(c("x", "y", "z", "HHI", "time")) %>%
  mutate(gamma = "0.001") %>%
  select(time, everything())

TEMP_LOW <- TEMP_LOW %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "HHI")) %>%
  mutate(gamma = "0.001")

TEMP_MEDIUM_TRI <- TEMP_MEDIUM %>%
  setNames(c("x", "y", "z", "HHI", "time")) %>%
  mutate(gamma = "0.01") %>%
  select(time, everything())

TEMP_MEDIUM <- TEMP_MEDIUM %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "HHI")) %>%
  mutate(gamma = "0.01")

TEMP_HIGH_TRI <- TEMP_HIGH %>%
  setNames(c("x", "y", "z", "HHI", "time")) %>%
  mutate(gamma = "0.1") %>%
  select(time, everything())

TEMP_HIGH <- TEMP_HIGH %>%
  mutate(x = as.character(x)) %>%
  filter(x %in% c("0.1", "0.5", "0.9")) %>%
  mutate(x = as.factor(x)) %>%
  mutate(y = y/(y+z)) %>%
  select(time, everything()) %>%
  setNames(c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "HHI")) %>%
  mutate(gamma = "0.1")

TEMP_ALL_TRI <- merge(TEMP_LOW_TRI, TEMP_MEDIUM_TRI, all = T) %>%
  merge(., TEMP_HIGH_TRI, all=T) %>%
  melt(id = c("time", "x", "y", "z", "gamma"))

TEMP_ALL_TRI_toplot <- merge(TEMP_LOW_TRI, TEMP_MEDIUM_TRI, all = T) %>%
  merge(., TEMP_HIGH_TRI, all=T) %>%
  melt(id = c("time", "x", "y", "z", "gamma")) %>%
  filter(time %in% c(5000,50000))

TEMP_ALL <- merge(TEMP_LOW, TEMP_MEDIUM, all = T) %>%
  merge(., TEMP_HIGH, all=T) %>%
  melt(id = c("time", "Manager.autonomy", "Ownership.LTI", "Ownership.STI", "gamma")) %>%
  filter(time %in% c(5000, 50000))

# keep only equilibrium 
TEMP_ALL_EQ <- TEMP_ALL %>% 
  filter(time %in% c(50000)) %>%
  mutate(Manager.autonomy = as.character(Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.1", "Low", Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.5", "Intermediate", Manager.autonomy)) %>% 
  mutate(Manager.autonomy = if_else(Manager.autonomy == "0.9", "High", Manager.autonomy)) %>%
  mutate(Manager.autonomy = factor(Manager.autonomy, levels = c("Low", "Intermediate", "High"))) %>% 
  mutate(gamma = if_else(gamma == "0.001", "Low bonus", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.01", "Baseline", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.1", "High bonus", gamma)) %>% 
  mutate(gamma = factor(gamma, levels = c("Low bonus", "Baseline", "High bonus")))

# decide whether to remove corner
TEMP_ALL_EQ2 <- TEMP_ALL_EQ %>%
  filter(Ownership.LTI > 0.02)

# splines for herfindahl -------------------------------------------------------
ggplot(TEMP_ALL_EQ2, aes(x=Ownership.LTI, y=value, group=Manager.autonomy, col=Manager.autonomy)) +
  facet_wrap(.~gamma, nrow = 1) +
  geom_smooth(method = "loess", se = F) +
  scale_color_manual(values = c("red", "blue", "black")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.background = element_rect(linetype="solid", colour ="black", size = .8)
  )
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig8.pdf")

# keep only variables for figure later on
THE_HHI <- TEMP_ALL_TRI
rm(list=setdiff(ls(), c("THE_PROD", "THE_HHI")))

# competition-growth plot ------------------------------------------------------

# reorganize the data
compgro <- merge(THE_PROD, THE_HHI, all = T) %>%
  group_by(variable) %>%
  mutate(grouped_id = row_number()) %>%
  ungroup() %>%
  spread(variable, value) %>%
  select(-grouped_id) %>%
  mutate(time = as.factor(time))

# remove outliers
threshold <- 0.1
compgro <- compgro %>%
  dplyr::select(x, y, z, time, gamma, Prod.Growth, HHI) %>%
  setNames(c("M", "L", "S", "time", "gamma", "prod_growth", "hhi")) %>%
  filter(M < (1-threshold) & M > threshold) %>%
  filter(L < (1-threshold) & L > threshold) %>%
  filter(S < (1-threshold) & S > threshold)

# prepare data for the plot
compgro2 <- compgro %>% 
  mutate(time = as.character(time)) %>% 
  filter(!time %in% c("5000")) %>% 
  mutate(time = if_else(time == "2500", "Young", time)) %>% 
  mutate(time = if_else(time == "10000", "Mature", time)) %>% 
  mutate(time = if_else(time == "50000", "Steady State", time)) %>% 
  mutate(time = factor(time, levels = c("Young", "Mature", "Steady State"))) %>% 
  mutate(gamma = if_else(gamma == "0.001", "Low bonus", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.01", "Baseline", gamma)) %>% 
  mutate(gamma = if_else(gamma == "0.1", "High bonus", gamma)) %>% 
  mutate(gamma = factor(gamma, levels = c("Low bonus", "Baseline", "High bonus"))) %>% 
  mutate(Md = rep("Low")) %>% 
  mutate(Md = if_else(M > 0.33 & M < 0.66, "Intermediate", Md)) %>% 
  mutate(Md = if_else(M > 0.66, "High", Md)) %>% 
  mutate(Md = factor(Md, levels = c("Low", "Intermediate", "High"))) %>% 
  mutate(Ld = rep("Short-term dominated")) %>% 
  mutate(Ld = if_else(L > 0.2 & L < 0.6, "Balanced", Ld)) %>% 
  mutate(Ld = if_else(L > 0.6, "Long-term dominated", Ld)) %>% 
  mutate(Ld = factor(Ld, levels = c("Short-term dominated", "Balanced", "Long-term dominated")))


# separate for different periods with colors for different periods
ggplot(compgro2, aes(x=(1-hhi), y=prod_growth, col = time, group = time)) +
  facet_wrap(time~., nrow = 1) +
  geom_point(alpha = .5, size = .9) +
  scale_color_viridis(option = "D", discrete = T, name = "life-cycle stage") + 
  xlab("competition (1-HHI)") +
  ylab("productivity growth") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13),
        plot.title = element_text(face="bold", size=15),
        legend.title = element_text(face="bold", size=13),
        legend.text = element_text(size = 13),
        strip.text = element_text(face="bold", size=13),
        legend.position = "right")
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig9.pdf")

# pooled with colors for different periods
ggplot(compgro2, aes(x=(1-hhi), y=prod_growth)) +
  geom_point(alpha = .5, size = .9, aes(col = time)) +
  scale_color_viridis(option = "D", discrete = T, name = "life-cycle stage") +
  geom_smooth(method = "loess", span = 0.5, size = 1.5, col = "black", se = F) +
  xlab("competition (1-HHI)") +
  ylab("roductivity growth") +
  theme_bw() +
  guides() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13),
        plot.title = element_text(face="bold", size=15),
        legend.title = element_text(face="bold", size=13),
        legend.text = element_text(size = 13),
        strip.text = element_text(face="bold", size=13),
        legend.position = "right")
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig10.pdf")