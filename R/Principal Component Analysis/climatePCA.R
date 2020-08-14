#### Climate Data Project: Principal Component Analysis Script ####
## Author: Wes Slaughter
## Org: Sierra Streams Institute
## Last Modified: 8/13/2020

# Hello! Welcome to the Princiapl Component Analysis (PCA) script. This script's
# primary purpose is to prepare the data for PCA and run PCA

# Load in necessary libraries
library(plyr)
library(tidyverse)
library(vegan)
library(zoo)

# Set working directory
setwd("C:/Users/Wes/Documents/SSI/Climate Data Project/FINALRUN")

# Bring in the raw datasets:

# Water Quality (WQ), Climate (CL), Bugs (BMI) dataframe
# Subset to only 200-2016, as these are the years for which we 
# have climate data

WQ.CL.BMI.raw <- read.csv("AllVars.csv")[,-1] %>% 
                    subset(WY < 2017)
