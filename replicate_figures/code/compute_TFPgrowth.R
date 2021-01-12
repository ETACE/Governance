# Title:    Replication file for empirical data
# Authors:  MG-PH-MN
# Version:  2020 December 21

# preparation -----------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(reshape2)

# productivity growth by industry EU-KLEMS ------------------------------------

# import own written functions
setwd("~/Documents/GitHub/Governance/replicate_figures/code/")
source("euklems_functions.R")

# set auxiliary useful variables
years <- seq(as.Date("1970/12/31"), as.Date("2015/12/31"), "years")
countries <- c("DE", "FR", "UK")
nace_sectors <- c("TOT", "MARKT", "A", "B", "C", "D-E", "F",
                  "G", "H", "I", "J", "K", "L", "M-N", "O-U")
nace_sectors_long <- c("TOTAL INDUSTRIES",
                       "MARKET ECONOMY",
                       "AGRICULTURE, FORESTRY AND FISHING",
                       "MINING AND QUARRYING",
                       "TOTAL MANUFACTURING",
                       "ELECTRICITY, GAS AND WATER SUPPLY",
                       "CONSTRUCTION",
                       "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
                       "TRANSPORTATION AND STORAGE",
                       "ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                       "INFORMATION AND COMMUNICATION",
                       "FINANCIAL AND INSURANCE ACTIVITIES",
                       "REAL ESTATE ACTIVITIES",
                       "PROFESSIONAL, SCIENTIFIC, TECHNICAL, ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                       "COMMUNITY SOCIAL AND PERSONAL SERVICES")
tfp <- vector(mode="list", length = length(nace_sectors))
names(tfp) <- nace_sectors

# load capital tables and output tables for the selected countries
setwd("~/Documents/GitHub/Governance/replicate_figures/data/eu_klems_data/")
output <- lapply(paste0(countries,"_output_17i.xlsx"), read_excel_allsheets)
names(output) <- countries

# load data for each industry
for (i in 1:length(nace_sectors)){
  tfp[[i]] <- extract_euklems(data_list = output,
                              variable = "TFPva_I",
                              countries = countries,
                              years = years,
                              output = T,
                              time0 = "1998/01/01",
                              nace_cod = nace_sectors[i]) %>%
    group_by(country) %>%
    mutate(., value_index = value / value[year == "1998-12-31"]) %>%
    ungroup(.) %>%
    mutate(., country = if_else(country == "DE", "Germany", country)) %>%
    mutate(., country = if_else(country == "FR", "France", country)) %>%
    mutate(., country = if_else(country == "UK", "United Kingdom", country))
}
rm(i)

tfp <- bind_rows(tfp, .id = "col_label") %>%
  select(-col_label) 

# average productivity growth
g <- tfp %>%
  filter(year < "2008-12-31" | year > "2010-12-31") %>% # exclude crisis years
  filter(variable == "TOT") %>%                         # total industry
  summarise(average = mean(dl_value, na.rm = T)) %>% 
  as.numeric()
g

