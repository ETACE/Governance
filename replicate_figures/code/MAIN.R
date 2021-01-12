# Title:    MAIN Replication file for the governance paper
# Authors:  MG-PH-MN
# Version:  2020 December 22

library(tseries)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(reshape2)

setwd("~/Documents/GitHub/Governance/replicate_figures/code/")

# compute number for the calibration in table 1
source("compute_TFPgrowth.R")

# plot all figures
source("Figure_1.R")
source("Figure_2-3.R")
source("Figure_4-5.R")
source("Figure_6.R")
source("Figure_7-10.R")

# plot also the triangles
source("Figure_2-3_triangles.R")
