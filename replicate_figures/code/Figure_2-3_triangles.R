# Title:    Replication file for Fig. 2-3 (triangles)
# Authors:  MG-PH-MN
# Version:  2021 January 12

library(tidyverse)
library(ggtern)
library(viridis)
library(RColorBrewer)

rm(list = ls())

# Import and clean data ====
setwd("~/Documents/GitHub/Governance/replicate_figures/data/")
load("triangle_plot.Rdata")

rm(i, p1, p2, par1, par2, parameter1, parameter2, pathdata, pathfigs, probe, runs, parameter_list,
   gamplot, getdata, retrieve_data)

data <- DATA %>%
  filter(t == 50000)

rm(DATA)

# Preparation ====
runs <- 20
log <- FALSE

par1 <- c("0.0", "0.01","0.02","0.04","0.06","0.08",
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

# Plot triangle for the productivity growth ====
variable <- "annual_growth_weighted_productivity"
log <- FALSE

TEMP <- c()
DATA1 <- c()

for(p1 in 1:length(par1))  {
  for(p2 in 1:length(par2))  {
    
    eval(parse(text=paste("temp = data[data$par1==par1[p1] & data$par2==par2[p2],]$",
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
TEMP1 <- TEMP %>%
  setNames(c("x", "y", "z", "Prod.Growth"))

# plot it
ggtern(TEMP1, aes(x=x,y=y,z=z)) +
  geom_point(size=6, aes(color=Prod.Growth), alpha = 1) +
  scale_color_viridis(direction = 1) +
  geom_segment(aes(x=10, y=0, z=90, xend=10,yend=90,zend=0),
               size=.7, color="red", linetype = "dashed") +
  geom_segment(aes(x=50, y=0, z=50, xend=50,yend=50,zend=0),
               size=.7, color="blue", linetype = "dashed") +
  geom_segment(aes(x=90, y=0, z=10, xend=90,yend=10,zend=0),
               size=.7, color="black", linetype = "dashed") +
  labs(x = "M", xarrow  = "Manager Influence",
       y = "LTI", yarrow  = "LTI Influence",
       z = "STI", zarrow  = "STI Influence") +
  theme_bw() +
  theme_showarrows() +
  theme_clockwise() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black", linetype = "dotted"),
        panel.ontop = TRUE,
        axis.text = element_text(size = 13),
        legend.position=c(0.85, 0.8)
  )
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig2_triangle.pdf")

# Plot triangle for the herfidahl-hirshman index ====
variable = "av_herfindahl_index"
log <- FALSE

TEMP <- c()
DATA1 <- c()

for(p1 in 1:length(par1))  {
  for(p2 in 1:length(par2))  {
    
    eval(parse(text=paste("temp = data[data$par1==par1[p1] & data$par2==par2[p2],]$",
                          variable,
                          sep="")))
    
    if(log){
      temp =log(temp)
    }
    
    TEMP = rbind(TEMP,
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
TEMP2 <- TEMP %>%
  setNames(c("x", "y", "z", "HHI"))

# plot it
ggtern(TEMP2, aes(x=x,y=y,z=z)) +
  geom_point(size=6, aes(color=HHI), alpha = 1) +
  scale_color_viridis(direction = -1) +
  geom_segment(aes(x=10, y=0, z=90, xend=10,yend=90,zend=0),
               size=.7, color="red", linetype = "dashed") +
  geom_segment(aes(x=50, y=0, z=50, xend=50,yend=50,zend=0),
               size=.7, color="blue", linetype = "dashed") +
  geom_segment(aes(x=90, y=0, z=10, xend=90,yend=10,zend=0),
               size=.7, color="black", linetype = "dashed") +
  labs(x = "M", xarrow  = "Manager Influence",
       y = "LTI", yarrow  = "LTI Influence",
       z = "STI", zarrow  = "STI Influence") +
  theme_bw() +
  theme_showarrows() +
  theme_clockwise() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black", linetype = "dotted"),
        panel.ontop = TRUE,
        axis.text = element_text(size = 13),
        legend.position=c(0.85, 0.8)
  )
ggsave("~/Documents/GitHub/Governance/replicate_figures/figs/fig3_triangle.pdf")

rm(list = setdiff(ls(), c("TEMP1", "TEMP2", "data")))
