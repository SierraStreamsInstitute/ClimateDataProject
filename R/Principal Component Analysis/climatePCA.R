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

#Clean data to remove outliers.
#Outliers here deteremined after internal discussion of "plausible"
#versus "impossible" outliers. 

#For now, primary outliers/changes are DO values >14, and 
#changing one high conductivity value that had a misplaced decimal
WQ.CL.BMI.raw <- WQ.CL.BMI.raw[WQ.CL.BMI.raw$O2 < 14,]
WQ.CL.BMI.raw[WQ.CL.BMI.raw$Cond > 859, colnum] <- 86.6 #Note: change colnum to your Cond column.
#Alternatively, just manually change the value of 860.667 to 86.67

